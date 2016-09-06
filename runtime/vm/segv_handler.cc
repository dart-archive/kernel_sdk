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

#ifdef __x86_64__
extern "C" void InterruptContinuationRaw();
#elif defined(__arm__)
extern "C" void InterruptContinuationRawARM();
#elif defined(__arm64__) || defined(__aarch64__)
extern "C" void InterruptContinuationRawARM64();
#else
#error "Unsupported architecture"
#endif

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

// There is one assumption this signal handler makes:  That registers used for
// TLS are valid at all points in time due to usage of `Thread::Current()`.
void SegvHandler::SignalHandler(int signal, siginfo_t* siginfo, void* context) {
  // Condition 1) Was a read/write/execute permission violation?
  //
  // NOTE: There are OS/platform differences in what signal gets delivered and
  // what `si_code` contains:
  //   * Linux usually uses normally SIGSEGV/SEGV_ACCERR
  //   * MacOS normally SIGBUS/BUS_ADRALN.
  if ((signal == SIGSEGV || signal == SIGBUS) &&
      ((siginfo->si_code == SEGV_ACCERR) ||
       (siginfo->si_code == BUS_ADRALN))) {
    // Condition 2) Is it a dart thread and was it executing dart code?
    Thread* thread = Thread::Current();
    if (thread != NULL &&
        thread->task_kind() == Thread::kMutatorTask &&
        thread->vm_tag() == VMTag::kDartTagId) {
      // Condition 3) Was the executing dart code accessing our polling address?
      void* fault_address = siginfo->si_addr;
      if (fault_address == thread->InterruptPagePollingAddress()) {
        // We now save the PC and transfer control to our interrupt continuation
        // handler - which runs outside of a signal handler context.
        // It can perform a e.g. a GC and then return back to Dart.
#ifdef __APPLE__
#ifdef __x86_64__
        ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
        thread->set_saved_interrupt_pc(ucontext->uc_mcontext->__ss.__rip);
        ucontext->uc_mcontext->__ss.__rip =
            reinterpret_cast<intptr_t>(&InterruptContinuationRaw);
        return;
#elif defined(__arm64__) || defined(__aarch64__)
        ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
        thread->set_saved_interrupt_pc(ucontext->uc_mcontext->__ss.__pc);

        // See http://www.heyrick.co.uk/armwiki/The_Status_register
        const int kThumbEnabledBit = 1 << 5;
        bool crashed_in_arm_mode =
            (ucontext->uc_mcontext->__ss.__cpsr & kThumbEnabledBit) == 0;

        // Since our precompiler is currently only emitting ARM code and not
        // thumb code, we know that we always have to come here in arm mode.
        if (crashed_in_arm_mode) {
          ucontext->uc_mcontext->__ss.__pc =
              reinterpret_cast<intptr_t>(&InterruptContinuationRawARM64);
          return;
        }
#else
#error "Unsupported architecture"
#endif

#elif __linux__

#ifdef __x86_64__
        ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
        thread->set_saved_interrupt_pc(ucontext->uc_mcontext.gregs[REG_RIP]);
        ucontext->uc_mcontext.gregs[REG_RIP] =
            reinterpret_cast<intptr_t>(&InterruptContinuationRaw);
        return;
#elif defined(__arm__)
        ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
        thread->set_saved_interrupt_pc(ucontext->uc_mcontext.arm_pc);

        // See http://www.heyrick.co.uk/armwiki/The_Status_register
        const int kThumbEnabledBit = 1 << 5;
        bool crashed_in_arm_mode =
            (ucontext->uc_mcontext.arm_cpsr & kThumbEnabledBit) == 0;
        if (crashed_in_arm_mode) {
          ucontext->uc_mcontext.arm_pc =
              reinterpret_cast<intptr_t>(&InterruptContinuationRawARM);
          return;
        }
#elif defined(__arm64__) || defined(__aarch64__)
        ucontext_t* ucontext = reinterpret_cast<ucontext_t*>(context);
        thread->set_saved_interrupt_pc(ucontext->uc_mcontext.pc);

        // See http://www.heyrick.co.uk/armwiki/The_Status_register
        const int kThumbEnabledBit = 1 << 5;
        bool crashed_in_arm_mode =
            (ucontext->uc_mcontext.pstate & kThumbEnabledBit) == 0;

        // Since our precompiler is currently only emitting ARM code and not
        // thumb code, we know that we always have to come here in arm mode.
        if (crashed_in_arm_mode) {
          ucontext->uc_mcontext.pc =
              reinterpret_cast<intptr_t>(&InterruptContinuationRawARM64);
          return;
        }
#else
#error "Unsupported architecture"
#endif

#else
#error "Unsupported OS"
#endif
      }
    }
  }

  // NOTE: Strictly speaking we shouldn't try to print anything inside a signal
  // handler (or any I/O, memory allocation, ...), but we are crashing anyway
  // here.
  FATAL("Unexpected crash (SEGV).");
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

  *rip = thread->saved_interrupt_pc();
  *rip2 = thread->saved_interrupt_pc();
  thread->set_saved_interrupt_pc(0);
  *top_exit_frame = exit_frame_fp;
  RealStackOverflow(isolate, thread, zone.GetZone());
  *top_exit_frame = 0;

  thread->set_vm_tag(VMTag::kDartTagId);
}

}  // namespace dart

#endif  // defined(USE_STACKOVERFLOW_TRAPS) && defined(DART_PRECOMPILED_RUNTIME)
