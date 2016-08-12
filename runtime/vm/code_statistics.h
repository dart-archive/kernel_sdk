// Copyright (c) 2015, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_CODE_STATISTICS_H_
#define VM_CODE_STATISTICS_H_

#include "vm/object.h"
#include "vm/assembler.h"
#include "vm/intermediate_language.h"

namespace dart {

class CombinedCodeStatistics {
 public:
  enum EntryCounter {
#define DO(type) kTag##type,
FOR_EACH_INSTRUCTION(DO)
#undef DO

    kPolymorphicInstanceCallAsStaticCall,

    kTagCheckedSmiSlowPath,
    kTagBoxAllocationSlowPath,
    kTagAllocateContextSlowPath,
    kTagCheckStackOverflowSlowPath,
    kTagMegamorphicSlowPath,

    kTagCheckArgumentCount,
    kTagCopyParameters,
    kTagStubCode,
    kTagFrameEntry,
    kTagLoadClosureContext,

    kNumEntries,
  };

  CombinedCodeStatistics();

  void Begin(Instruction* instruction);
  void End(Instruction* instruction);

  void DumpStatistics();

 private:
  friend class CodeStatistics;

  void SlowSort();
  void Swap(intptr_t a, intptr_t b);

  typedef struct {
    const char* name;
    intptr_t bytes;
    intptr_t count;
  } Entry;

  Entry entries_[kNumEntries];
  intptr_t unaccounted_bytes_;
  intptr_t alignment_bytes_;
  intptr_t object_header_bytes_;
  intptr_t wasteful_function_count_;
  intptr_t return_const_count_;
  intptr_t return_const_with_load_field_count_;
};

class CodeStatistics {
 public:
  explicit CodeStatistics(Assembler* assembler);

  void Begin(Instruction* instruction);
  void End(Instruction* instruction);

  void SpecialBegin(intptr_t tag);
  void SpecialEnd(intptr_t tag);

  void AppendTo(CombinedCodeStatistics* stat);

  void Finalize();

 private:
  static const int kStackSize = 8;

  Assembler* assembler_;

  typedef struct {
    intptr_t bytes;
    intptr_t count;
  } Entry;

  Entry entries_[CombinedCodeStatistics::kNumEntries];
  intptr_t instruction_bytes_;
  intptr_t unaccounted_bytes_;
  intptr_t alignment_bytes_;

  intptr_t stack_[kStackSize];
  intptr_t stack_index_;
};

}  // namespace dart

#endif  // VM_CODE_STATISTICS_H_
