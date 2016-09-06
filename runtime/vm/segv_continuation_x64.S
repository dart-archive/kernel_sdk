#ifdef __x86_64__
#ifdef DART_PRECOMPILED_RUNTIME

#define DART_PP %r15
#define DART_CODE_REG %r12

  .text
#ifdef __APPLE__
  .global  _InterruptContinuationRaw
#define InterruptContinuation _InterruptContinuation
_InterruptContinuationRaw:
#else
  .global  InterruptContinuationRaw
InterruptContinuationRaw:
#endif
  # Setup slot for restored PC
  pushq $0xFFFFFFFFDEADBEEF

  # Save GPRs
  # NOTE: We assume here that [InterruptContinuation] will not cause any
  # floating point caller-saved floating point registers to be written.
  # Not pushing: rsp, rbp
  pushq %r15
  pushq %r14
  pushq %r13
  pushq %r12
  pushq %r11
  pushq %r10
  pushq %r9
  pushq %r8
  pushq %rdi
  pushq %rsi
  pushq %rdx
  pushq %rcx
  pushq %rbx
  pushq %rax

  # Make an artificial exit frame (see `Assembler::EnterStubFrame()`).
  # We do this because the stack walker assumes the stack always starts with an
  # exit frame
  pushq $0xFFFFFFFFDEADBEEF
  pushq %rbp
  movq %rsp, %rbp
  pushq DART_CODE_REG
  pushq DART_PP

  # Prepare arguments:
  #   Argument 0: pointer to saved PC
  #   Argument 1: pointer to saved PC in artificial frame
  #   Argument 2: artificial frame pointer
  leaq 128(%rbp), %rdi
  leaq 8(%rbp), %rsi
  movq %rbp, %rdx

  # Add stack padding to guarantee 16-byte alignment.
  movq %rsp, %rbx
  andq $0xfffffffffffffff0, %rsp

  # Save FPU state
  subq $256, %rsp
  movups %xmm0, 0(%rsp)
  movups %xmm1, 16(%rsp)
  movups %xmm2, 32(%rsp)
  movups %xmm3, 48(%rsp)
  movups %xmm4, 64(%rsp)
  movups %xmm5, 80(%rsp)
  movups %xmm6, 96(%rsp)
  movups %xmm7, 112(%rsp)
  movups %xmm8, 128(%rsp)
  movups %xmm9, 144(%rsp)
  movups %xmm10, 160(%rsp)
  movups %xmm11, 176(%rsp)
  movups %xmm12, 192(%rsp)
  movups %xmm13, 208(%rsp)
  movups %xmm14, 224(%rsp)
  movups %xmm15, 240(%rsp)

  call InterruptContinuation

  # Restore FPU state
  movups 0(%rsp), %xmm0
  movups 16(%rsp), %xmm1
  movups 32(%rsp), %xmm2
  movups 48(%rsp), %xmm3
  movups 64(%rsp), %xmm4
  movups 80(%rsp), %xmm5
  movups 96(%rsp), %xmm6
  movups 112(%rsp), %xmm7
  movups 128(%rsp), %xmm8
  movups 144(%rsp), %xmm9
  movups 160(%rsp), %xmm10
  movups 176(%rsp), %xmm11
  movups 192(%rsp), %xmm12
  movups 208(%rsp), %xmm13
  movups 224(%rsp), %xmm14
  movups 240(%rsp), %xmm15
  addq $256, %rsp

  # Remove stack padding to guarantee 16-byte alignment.
  movq %rbx, %rsp

  # Leave the artificial exit frame.
  popq %r15
  popq %r12
  movq %rbp, %rsp
  popq %rbp
  popq %rdx

  # Restore GPRs
  popq %rax
  popq %rbx
  popq %rcx
  popq %rdx
  popq %rsi
  popq %rdi
  popq %r8
  popq %r9
  popq %r10
  popq %r11
  popq %r12
  popq %r13
  popq %r14
  popq %r15

  # Jump to restored PC
  ret

#endif  // DART_PRECOMPILED_RUNTIME
#endif  // __x86_64__
