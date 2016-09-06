#ifdef __arm__
#ifdef DART_PRECOMPILED_RUNTIME

#define DART_PP r5
#define DART_CODE_REG r6
#define FP r11

  .arch armv7-a

  .text
  .align  2
  .global  InterruptContinuation
  .global  InterruptContinuationRawARM
  .type  InterruptContinuationRawARM, %function
InterruptContinuationRawARM:
  @ User code trapped with ARM, so we continue in ARM but might call ARM/Thumb
  @ continuation

  @ Save GPRs
  stmfd sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r12, lr, pc}

  @ Make an artificial exit frame (see Assembler::EnterStubFrame()`).
  @ We do this because the stack walker assumes the stack always starts with an
  @ exit frame

  mov r0, sp
  stmfd sp!, {r0}
  stmfd sp!, {DART_PP, DART_CODE_REG, FP, r12 /* slot for pc */}

  @ Get the address of saved pc
  add r0, sp, #72
  @ Get the address of the saved pc from the artificial frame
  add r1, sp, #12
  @ Get the exit frame pointer
  add r2, sp, #8

  @ Add stack padding to guarantee 16-byte alignment.
  mov r11, sp
  bic sp, sp, #15

  @ Save FPU registers
  vpush {d0-d15}

  @ Call the real interrupt handler.
  blx InterruptContinuation

  @ Restore FPU registers
  vpop {d0-d15}

  @ Remove stack padding.
  mov sp, r11

  @ Drop artificial frame
  ldmfd sp!, {DART_PP, DART_CODE_REG, FP, r12 /* slot for pc */}
  pop {r0}

  @ Restore frame
  ldmfd sp!, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r12, lr, pc}

#endif  // DART_PRECOMPILED_RUNTIME
#endif  // __arm__
