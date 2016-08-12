// Copyright (c) 2015, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/code_statistics.h"

namespace dart {

CombinedCodeStatistics::CombinedCodeStatistics() {
  unaccounted_bytes_ = 0;
  alignment_bytes_ = 0;
  object_header_bytes_ = 0;
  wasteful_function_count_ = 0;
  return_const_count_ = 0;
  return_const_with_load_field_count_ = 0;
  intptr_t i = 0;

#define DO(type) \
  entries_[i].name = #type; \
  entries_[i].bytes = 0; \
  entries_[i++].count = 0;

  FOR_EACH_INSTRUCTION(DO)

#undef DO

#define INIT_SPECIAL_ENTRY(tag, str)  \
  entries_[tag].name = str;           \
  entries_[tag].bytes = 0;            \
  entries_[tag].count = 0;

  INIT_SPECIAL_ENTRY(kPolymorphicInstanceCallAsStaticCall,
                     "PolymorphicInstanceCall -> StaticCall")

  INIT_SPECIAL_ENTRY(kTagCheckedSmiSlowPath, "<slow-path:checked-smi>")
  INIT_SPECIAL_ENTRY(kTagBoxAllocationSlowPath, "<slow-path:box-allocation>")
  INIT_SPECIAL_ENTRY(kTagAllocateContextSlowPath,
                     "<slow-path:allocate-context>")
  INIT_SPECIAL_ENTRY(kTagCheckStackOverflowSlowPath,
                     "<slow-path:stack-overflow>")
  INIT_SPECIAL_ENTRY(kTagMegamorphicSlowPath, "<slow-path:megamorphic>")

  INIT_SPECIAL_ENTRY(kTagCheckArgumentCount, "<check argument count>");
  INIT_SPECIAL_ENTRY(kTagCopyParameters, "<copy parameters>");
  INIT_SPECIAL_ENTRY(kTagStubCode, "<stub-code>");
  INIT_SPECIAL_ENTRY(kTagFrameEntry, "<frame-entry>");
  INIT_SPECIAL_ENTRY(kTagLoadClosureContext, "<load-closure-context>");

#undef INIT_SPECIAL_ENTRY
}

void CombinedCodeStatistics::DumpStatistics() {
  ASSERT(unaccounted_bytes_ >= 0);

  SlowSort();

  intptr_t instruction_bytes = 0;
  for (intptr_t i = 0; i < kNumEntries; i++) {
    instruction_bytes += entries_[i].bytes;
  }
  intptr_t total = object_header_bytes_ +
                   instruction_bytes +
                   unaccounted_bytes_ +
                   alignment_bytes_;
  float ftotal = static_cast<float>(total) / 100.0;

  fprintf(stderr, "--------------------\n");

  for (intptr_t i = 0; i < kNumEntries; i++) {
    const char* name = entries_[i].name;
    intptr_t bytes = entries_[i].bytes;
    intptr_t count = entries_[i].count;
    float percent = bytes / ftotal;
    float avg = static_cast<float>(bytes) / count;
    if (bytes > 0) {
      fprintf(
          stderr,
          "%5.2f %% "
          "% 8" Pd " bytes  "
          "% 8" Pd " count "
          "%8.2f avg bytes/entry    "
          "-    %s\n",
          percent,
          bytes,
          count,
          avg,
          name);
    }
  }

  fprintf(stderr, "--------------------\n");

  fprintf(stderr, "%5.2f %% % 8" Pd " bytes unaccounted\n",
      unaccounted_bytes_/ftotal, unaccounted_bytes_);
  fprintf(stderr, "%5.2f %% % 8" Pd " bytes alignment\n",
      alignment_bytes_ / ftotal, alignment_bytes_);
  fprintf(stderr, "%5.2f %% % 8" Pd " bytes instruction object header\n",
      object_header_bytes_ / ftotal, object_header_bytes_);
  fprintf(stderr, "%5.2f %% % 8" Pd " bytes instructions\n",
      instruction_bytes / ftotal, instruction_bytes);
  fprintf(stderr, "--------------------\n");
  fprintf(stderr, "%5.2f %% % 8" Pd " bytes in total\n",
      total/ftotal, total);
  fprintf(stderr, "--------------------\n");
  fprintf(stderr, "% 8" Pd " return-constant functions\n", return_const_count_);
  fprintf(stderr, "% 8" Pd " return-constant-with-load-field functions\n",
      return_const_with_load_field_count_);
  fprintf(stderr, "% 8" Pd " wasteful functions (body < 2 * frame overhead)\n",
      wasteful_function_count_);
  fprintf(stderr, "--------------------\n");
}


void CombinedCodeStatistics::SlowSort() {
  for (intptr_t upper = kNumEntries - 1; upper >= 0; upper--) {
    intptr_t largest_index = 0;
    intptr_t largest_value = entries_[largest_index].bytes;
    for (intptr_t i = 1; i <= upper; i++) {
      intptr_t bytes = entries_[i].bytes;
      if (largest_value < bytes) {
        largest_index = i;
        largest_value = bytes;
      }
    }
    if (largest_index != upper) Swap(largest_index, upper);
  }
}

void CombinedCodeStatistics::Swap(intptr_t a, intptr_t b) {
  const char* a_name = entries_[a].name;
  intptr_t a_bytes = entries_[a].bytes;
  intptr_t a_count = entries_[a].count;

  entries_[a].name = entries_[b].name;
  entries_[a].bytes = entries_[b].bytes;
  entries_[a].count = entries_[b].count;

  entries_[b].name = a_name;
  entries_[b].bytes = a_bytes;
  entries_[b].count = a_count;
}

CodeStatistics::CodeStatistics(Assembler* assembler)
    : assembler_(assembler) {
  memset(entries_, 0, CombinedCodeStatistics::kNumEntries * sizeof(Entry));
  instruction_bytes_ = 0;
  unaccounted_bytes_ = 0;
  alignment_bytes_ = 0;

  stack_index_ = -1;
  for (intptr_t i = 0; i < kStackSize; i++) stack_[i] = -1;
}

void CodeStatistics::Begin(Instruction* instruction) {
  SpecialBegin(static_cast<intptr_t>(instruction->tag()));
}

void CodeStatistics::End(Instruction* instruction) {
  SpecialEnd(static_cast<intptr_t>(instruction->tag()));
}

void CodeStatistics::SpecialBegin(intptr_t tag) {
  stack_index_++;
  ASSERT(stack_index_ < kStackSize);
  ASSERT(stack_[stack_index_] == -1);
  ASSERT(tag < CombinedCodeStatistics::kNumEntries);
  stack_[stack_index_] = assembler_->CodeSize();
  ASSERT(stack_[stack_index_] >= 0);
}

void CodeStatistics::SpecialEnd(intptr_t tag) {
  ASSERT(stack_[stack_index_] >= 0);
  ASSERT(tag < CombinedCodeStatistics::kNumEntries);

  intptr_t diff = assembler_->CodeSize() - stack_[stack_index_];
  ASSERT(diff >= 0);
  ASSERT(entries_[tag].bytes >= 0);
  ASSERT(entries_[tag].count >= 0);
  entries_[tag].bytes += diff;
  entries_[tag].count++;
  instruction_bytes_ += diff;
  stack_[stack_index_] = -1;
  stack_index_--;
}

void CodeStatistics::Finalize() {
  intptr_t function_size = assembler_->CodeSize();
  unaccounted_bytes_ = function_size - instruction_bytes_;
  ASSERT(unaccounted_bytes_ >= 0);
  alignment_bytes_ =
      Utils::RoundUp(function_size, OS::PreferredCodeAlignment()) -
      function_size;
  assembler_ = NULL;
}

void CodeStatistics::AppendTo(CombinedCodeStatistics* stat) {
  intptr_t sum = 0;
  bool returns_constant = true;
  bool returns_const_with_load_field_ = true;

  for (intptr_t i = 0; i < CombinedCodeStatistics::kNumEntries; i++) {
    intptr_t bytes = entries_[i].bytes;
    stat->entries_[i].count += entries_[i].count;
    if (bytes > 0) {
      sum += bytes;
      stat->entries_[i].bytes += bytes;
      if (i != CombinedCodeStatistics::kTagFrameEntry &&
          i != CombinedCodeStatistics::kTagParallelMove &&
          i != CombinedCodeStatistics::kTagReturn &&
          i != CombinedCodeStatistics::kTagCheckStackOverflow &&
          i != CombinedCodeStatistics::kTagCheckStackOverflowSlowPath) {
        returns_constant = false;
        if (i != CombinedCodeStatistics::kTagLoadField &&
            i != CombinedCodeStatistics::kTagTargetEntry &&
            i != CombinedCodeStatistics::kTagJoinEntry) {
          returns_const_with_load_field_ = false;
        }
      }
    }
  }
  stat->unaccounted_bytes_ += unaccounted_bytes_;
  ASSERT(stat->unaccounted_bytes_ >= 0);
  stat->alignment_bytes_ += alignment_bytes_;
  stat->object_header_bytes_ += Instructions::HeaderSize();

  intptr_t frame_overhead =
      entries_[CombinedCodeStatistics::kTagFrameEntry].bytes +
      entries_[CombinedCodeStatistics::kTagReturn].bytes;

  bool is_wasteful = sum < (2 * frame_overhead);
  if (is_wasteful) stat->wasteful_function_count_++;
  if (returns_constant) stat->return_const_count_++;
  if (returns_const_with_load_field_) {
    stat->return_const_with_load_field_count_++;
  }
}

}  // namespace dart
