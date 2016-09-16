// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/segv_handler.h"

#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if defined(USE_STACKOVERFLOW_TRAPS) && defined(DART_PRECOMPILED_RUNTIME)

#ifdef __APPLE__
#define _XOPEN_SOURCE
#include <ucontext.h>
#undef _XOPEN_SOURCE
#else  // __APPLE__
#include <ucontext.h>
#endif  // __APPLE__

#include "vm/code_generator.h"
#include "vm/handles.h"
#include "vm/isolate.h"
#include "vm/safepoint.h"
#include "vm/thread.h"
#include "vm/zone.h"

namespace dart {

static struct sigaction new_sigaction_;
static struct sigaction old_sigaction_;
static struct sigaction old_sigaction2_;

void SegvHandler::InitOnce() {
  int ret;

  // We use new-style handlers to context info.
  new_sigaction_.sa_flags = SA_SIGINFO;
  new_sigaction_.sa_sigaction = &SegvHandler::SignalHandler;

  // We don't block any additional signals while we execute the handler.
  // Should we block SIGPROF?
  ret = sigemptyset(&new_sigaction_.sa_mask);
  if (ret != 0) FATAL("sigemptyset() failed.");

  if (sigaddset(&new_sigaction_.sa_mask, SIGPROF) != 0) {
    FATAL("sigaddset() failed");
  }

  ret = sigaction(SIGSEGV, &new_sigaction_, &old_sigaction_);
  if (ret != 0) FATAL("sigaction() failed.");

  ret = sigaction(SIGBUS, &new_sigaction_, &old_sigaction2_);
  if (ret != 0) FATAL("sigaction() failed.");
}

void SegvHandler::TearDown() {
  if (sigaction(SIGBUS, &old_sigaction2_, NULL) != 0) {
    FATAL("sigaction() failed.");
  }
  if (sigaction(SIGSEGV, &old_sigaction_, NULL) != 0) {
    FATAL("sigaction() failed.");
  }
}

// There is one assumptions this signal handler makes:
//
// The registers used for TLS functionality are valid at all points in time
// due to usage of `Thread::Current()`.
void SegvHandler::SignalHandler(int signal, siginfo_t* siginfo, void* context) {
  // Condition 1) Was a read/write/execute permission violation?
  //
  // NOTE: There are OS/platform differences in what signal gets delivered and
  // what `si_code` contains:
  //   * Linux usually uses normally SIGSEGV/SEGV_ACCERR
  //   * MacOS normally SIGBUS/BUS_ADRALN.
  if (((signal == SIGSEGV) || (signal == SIGBUS)) &&
      ((siginfo->si_code == SEGV_ACCERR) ||
       (siginfo->si_code == BUS_ADRALN))) {
    // Condition 2) Is it a dart thread and was it executing dart code?
    Thread* thread = Thread::Current();
    if (thread != NULL &&
        thread->task_kind() == Thread::kMutatorTask &&
        thread->vm_tag() == VMTag::kDartTagId) {
      void* fault_address = siginfo->si_addr;

      // Case 3.1) Was the executing Dart code accessing our polling
      //           address?
      //   => The thread should be interrupted.
      //
      // Case 3.2) Was the executing Dart code accessing the stackoverflow
      //           guard page?
      //   => The thread should throw a stackoverflow exception.
      //
      // Case 3.3) Was the executing Dart code peeking too far ahead on the
      //           stack region?
      //   => We should let the OS page in the necessary stack pages.
      //
      if (fault_address == thread->InterruptPagePollingAddress()) {
        // We now save the PC and transfer control to our interrupt continuation
        // handler - which runs outside of a signal handler context.
        //
        // => It can perform a e.g. a GC and then return back to Dart.
        SetContinuationPC(thread, context);
        return;
      } else if (thread->os_thread()->IsStackGuardPage(fault_address)) {
        // We now save the PC and transfer control to our interrupt continuation
        // handler - which runs outside of a signal handler context.
        //
        // => It will throw the (preallocated) stackoverflow exception.
        thread->set_has_stackoverflow(true);
        SetContinuationPC(thread, context);
        return;
      } else if (thread->os_thread()->IsStackPage(fault_address)) {
        // The only other invalid access from Dart code is when peeking beyond
        // the bottom of the stack (but above the guard page).  This happens
        // when stacks grow dynamically downwards page-by-page and our peeking
        // code tried to look ahead 2 pages.
        //
        // => In that case we'll simply let the OS grow downwards 4k-by-4k.

        // NOTE: It should be said that this is actually (on linux at least)
        // only necessary for the main thread of the process.  Other threads
        // created with a (newer) pthread library will use the (more sane)
        // private mmap approach, instead of the page-by-page downwards growing
        // approach!

        // This [kPokingDistance] is either equal to or smaller than the page
        // size used by the OS (we don't want to call
        // `VirtualMemory::PageSize()` in this signal handling code!)
        static const int kPokingDistance = 4096;
        static const int kPokingMask = ~(kPokingDistance - 1);

        void* current_fp = __builtin_frame_address(0);

        intptr_t from_stack_address =
            reinterpret_cast<uintptr_t>(current_fp) & kPokingMask;
        intptr_t to_stack_address =
            reinterpret_cast<intptr_t>(fault_address) & kPokingMask;

        // We start in the page following the one this function uses.
        from_stack_address -= kPokingDistance;

        while (from_stack_address > to_stack_address) {
          volatile uint8_t* byte =
              reinterpret_cast<uint8_t*>(from_stack_address);
          *byte = 0x42;
          from_stack_address -= kPokingDistance;
        }

        // We paged in the necessary stack pages so we can return to the Dart
        // code where we left off and it will work fine this time.
        return;
      }
    }
  }

  // NOTE: Strictly speaking we shouldn't try to print anything inside a signal
  // handler (or any I/O, memory allocation, ...), but we are crashing anyway
  // here.
  FATAL("Unexpected crash (SEGV).");
}

void SegvHandler::SetContinuationPC(Thread* thread, void* context) {
  intptr_t* pc_pointer = GetPointerToSavedPc(context);
  thread->set_saved_interrupt_pc(*pc_pointer);
  const RawCode* code = thread->guard_page_continuation_stub_->ptr();
  *pc_pointer = code->entry_point_;
}

intptr_t* SegvHandler::GetPointerToSavedPc(void* context) {
#if defined(__arm__) || defined(__arm64__) || defined(__aarch64__)
  // See http://www.heyrick.co.uk/armwiki/The_Status_register
  const int kThumbEnabledBit = 1 << 5;
#endif

  intptr_t* pc_pointer;
  bool crashed_in_thumb_mode = false;

#ifdef __APPLE__
#ifdef __x86_64__
  pc_pointer = reinterpret_cast<intptr_t*>(
      &(reinterpret_cast<ucontext_t*>(context)->uc_mcontext->__ss.__rip));
#elif defined(__arm64__) || defined(__aarch64__)
  ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
  pc_pointer = reinterpret_cast<intptr_t*>(&ucontext->uc_mcontext->__ss.__pc);
  crashed_in_thumb_mode =
      (ucontext->uc_mcontext->__ss.__cpsr & kThumbEnabledBit) != 0;
#else
#error "Unsupported architecture"
#endif

#elif __linux__

#ifdef __x86_64__
  pc_pointer = reinterpret_cast<intptr_t*>(
      &(reinterpret_cast<ucontext_t*>(context)->uc_mcontext.gregs[REG_RIP]));
#elif defined(__arm__)
  ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
  pc_pointer = reinterpret_cast<intptr_t*>(&ucontext->uc_mcontext.arm_pc);
  crashed_in_thumb_mode =
      (ucontext->uc_mcontext.arm_cpsr & kThumbEnabledBit) != 0;

#elif defined(__arm64__) || defined(__aarch64__)
  ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
  pc_pointer = reinterpret_cast<intptr_t*>(&ucontext->uc_mcontext.pc);
  crashed_in_thumb_mode =
      (ucontext->uc_mcontext.pstate & kThumbEnabledBit) != 0;
#else
#error "Unsupported architecture"
#endif

#else
#error "Unsupported OS"
#endif

  if (crashed_in_thumb_mode) {
    FATAL("Unexpected crash in thumb mode during Dart execution");
  }

  return pc_pointer;
}


extern "C" void InterruptContinuation(
    intptr_t *rip, intptr_t* rip2, intptr_t exit_frame_fp) {
  Thread* thread = Thread::Current();
  Isolate* isolate =  thread->isolate();
  TransitionGeneratedToVM transition(thread);
  StackZone zone(thread);
  HANDLESCOPE(thread);

  intptr_t* top_exit_frame = reinterpret_cast<intptr_t*>(
      reinterpret_cast<uint8_t*>(thread) +
      Thread::top_exit_frame_info_offset());

  ASSERT(thread->vm_tag() == VMTag::kDartTagId);
  thread->set_vm_tag(VMTag::kVMTagId);

  bool throw_stackoverflow_exception = false;
  if (thread->has_stackoverflow()) {
    thread->set_has_stackoverflow(false);
    throw_stackoverflow_exception = true;
  }

  *rip = thread->saved_interrupt_pc();
  *rip2 = thread->saved_interrupt_pc();
  thread->set_saved_interrupt_pc(0);
  *top_exit_frame = exit_frame_fp;
  RealStackOverflow(
      isolate, thread, zone.GetZone(), throw_stackoverflow_exception);
  *top_exit_frame = 0;

  thread->set_vm_tag(VMTag::kDartTagId);
}

}  // namespace dart

#endif  // defined(USE_STACKOVERFLOW_TRAPS) && defined(DART_PRECOMPILED_RUNTIME)
