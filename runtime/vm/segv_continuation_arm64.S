#if defined(__aarch64__)
#ifdef DART_PRECOMPILED_RUNTIME

#define DART_SP x15
#define DART_PP x27
#define DART_CODE_REG x24
#define FP x29
#define LR x30
#define TMP x16

  .text
  .align  2
#ifdef __APPLE__
  .global _InterruptContinuation
  .global _InterruptContinuationRawARM64
#define InterruptContinuation _InterruptContinuation
_InterruptContinuationRawARM64:
#else
  .global InterruptContinuation
  .global InterruptContinuationRawARM64
InterruptContinuationRawARM64:
#endif
  // User code trapped with ARM, so we continue in ARM but might call ARM/Thumb
  // continuation

  // Safe GPRs
  // NOTE: We assume here that [InterruptContinuation] will not cause any
  // floating point caller-saved floating point registers to be written.

  str x30, [DART_SP, #-8]! // pc
  str x30, [DART_SP, #-8]!
  str x29, [DART_SP, #-8]!
  str x28, [DART_SP, #-8]!
  str x27, [DART_SP, #-8]!
  str x26, [DART_SP, #-8]!
  str x25, [DART_SP, #-8]!
  str x24, [DART_SP, #-8]!
  str x23, [DART_SP, #-8]!
  str x22, [DART_SP, #-8]!
  str x21, [DART_SP, #-8]!
  str x20, [DART_SP, #-8]!
  str x19, [DART_SP, #-8]!
  str x18, [DART_SP, #-8]!
  str x17, [DART_SP, #-8]!
  str x16, [DART_SP, #-8]!
  str x14, [DART_SP, #-8]!
  str x13, [DART_SP, #-8]!
  str x12, [DART_SP, #-8]!
  str x11, [DART_SP, #-8]!
  str x10, [DART_SP, #-8]!
  str x9, [DART_SP, #-8]!
  str x8, [DART_SP, #-8]!
  str x7, [DART_SP, #-8]!
  str x6, [DART_SP, #-8]!
  str x5, [DART_SP, #-8]!
  str x4, [DART_SP, #-8]!
  str x3, [DART_SP, #-8]!
  str x2, [DART_SP, #-8]!
  str x1, [DART_SP, #-8]!
  str x0, [DART_SP, #-8]!

  // Make an artificial exit frame (see Assembler::EnterStubFrame()`).
  // We do this because the stack walker assumes the stack always starts with an
  // exit frame
  str x0, [DART_SP, #-8]! // pc slot
  str FP, [DART_SP, #-8]!
  str DART_CODE_REG, [DART_SP, #-8]!
  str DART_PP, [DART_SP, #-8]!

  // Save old C stack pointer (Dart uses x15) and restore real sp
  add x0, sp, #0
  str x0, [DART_SP, #-8]!

  // Get the address of saved pc
  add x0, DART_SP, #280
  // Get the address of the saved pc from the artificial frame
  add x1, DART_SP, #32
  // Get the exit frame pointer
  add x2, DART_SP, #24

  // Safe caller-saved FPU state
  // (v8-v15 are callee-saved)
  str q1, [DART_SP, #-16]!
  str q2, [DART_SP, #-16]!
  str q3, [DART_SP, #-16]!
  str q4, [DART_SP, #-16]!
  str q5, [DART_SP, #-16]!
  str q6, [DART_SP, #-16]!
  str q7, [DART_SP, #-16]!
  str q16, [DART_SP, #-16]!
  str q17, [DART_SP, #-16]!
  str q18, [DART_SP, #-16]!
  str q19, [DART_SP, #-16]!
  str q20, [DART_SP, #-16]!
  str q21, [DART_SP, #-16]!
  str q22, [DART_SP, #-16]!
  str q23, [DART_SP, #-16]!
  str q24, [DART_SP, #-16]!
  str q25, [DART_SP, #-16]!
  str q26, [DART_SP, #-16]!
  str q27, [DART_SP, #-16]!
  str q28, [DART_SP, #-16]!
  str q29, [DART_SP, #-16]!
  str q30, [DART_SP, #-16]!
  str q31, [DART_SP, #-16]!

  // Add stack padding to guarantee 16-byte alignment.
  add x19, DART_SP, #0
  mov x20, x19
  bic x20, x20, #15
  add sp, x20, #0

  // Call the real interrupt handler.
  bl InterruptContinuation

  // Remove stack padding.
  mov DART_SP, x19

  // Restore caller-saved FPU state
  // (v8-v15 are callee-saved)
  ldr q31, [DART_SP, #0]!
  ldr q30, [DART_SP, #16]!
  ldr q29, [DART_SP, #16]!
  ldr q28, [DART_SP, #16]!
  ldr q27, [DART_SP, #16]!
  ldr q26, [DART_SP, #16]!
  ldr q25, [DART_SP, #16]!
  ldr q24, [DART_SP, #16]!
  ldr q23, [DART_SP, #16]!
  ldr q22, [DART_SP, #16]!
  ldr q21, [DART_SP, #16]!
  ldr q20, [DART_SP, #16]!
  ldr q19, [DART_SP, #16]!
  ldr q18, [DART_SP, #16]!
  ldr q17, [DART_SP, #16]!
  ldr q16, [DART_SP, #16]!
  ldr q7, [DART_SP, #16]!
  ldr q6, [DART_SP, #16]!
  ldr q5, [DART_SP, #16]!
  ldr q4, [DART_SP, #16]!
  ldr q3, [DART_SP, #16]!
  ldr q2, [DART_SP, #16]!
  ldr q1, [DART_SP, #16]!

  // Restore old C stack pointer
  ldr x0, [DART_SP, #16]!
  add sp, x0, #0

  // Drop artificial frame
  ldr DART_PP, [DART_SP, #8]!
  ldr x0, [DART_SP, #8]!
  ldr FP, [DART_SP, #8]!
  ldr x0, [DART_SP, #8]!

  // Restore frame
  ldr x0, [DART_SP, #8]!
  ldr x1, [DART_SP, #8]!
  ldr x2, [DART_SP, #8]!
  ldr x3, [DART_SP, #8]!
  ldr x4, [DART_SP, #8]!
  ldr x5, [DART_SP, #8]!
  ldr x6, [DART_SP, #8]!
  ldr x7, [DART_SP, #8]!
  ldr x8, [DART_SP, #8]!
  ldr x9, [DART_SP, #8]!
  ldr x10, [DART_SP, #8]!
  ldr x11, [DART_SP, #8]!
  ldr x12, [DART_SP, #8]!
  ldr x13, [DART_SP, #8]!
  ldr x14, [DART_SP, #8]!
  ldr x16, [DART_SP, #8]!
  ldr x17, [DART_SP, #8]!
  ldr x18, [DART_SP, #8]!
  ldr x19, [DART_SP, #8]!
  ldr x20, [DART_SP, #8]!
  ldr x21, [DART_SP, #8]!
  ldr x22, [DART_SP, #8]!
  ldr x23, [DART_SP, #8]!
  ldr x24, [DART_SP, #8]!
  ldr x25, [DART_SP, #8]!
  ldr x26, [DART_SP, #8]!
  ldr x27, [DART_SP, #8]!
  ldr x28, [DART_SP, #8]!
  ldr x29, [DART_SP, #8]!
  ldr x30, [DART_SP, #8]!
  ldr TMP, [DART_SP, #8]!
  add DART_SP, DART_SP, #8

  // NOTE: Due to us using `ret` or `br` for returning to Dart code, we are
  // forced one register (either `lr` or `TMP).  As opposed to 32-bit arm, we
  // cannot restore the entire processor state!
  // => We therefore make an ASSUMPTION: The [StackOverflowInstr] will not keep
  // meaning full things in `TMP` across the load of the guard page.
  br TMP

#endif  // DART_PRECOMPILED_RUNTIME
#endif  // defined(__aarch64__)
