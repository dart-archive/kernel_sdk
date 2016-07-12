// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/globals.h"  // Needed here to get TARGET_ARCH_DBC.
#if defined(TARGET_ARCH_DBC)

#include "vm/intermediate_language.h"

#include "vm/cpu.h"
#include "vm/compiler.h"
#include "vm/dart_entry.h"
#include "vm/flow_graph.h"
#include "vm/flow_graph_compiler.h"
#include "vm/flow_graph_range_analysis.h"
#include "vm/locations.h"
#include "vm/object_store.h"
#include "vm/parser.h"
#include "vm/simulator.h"
#include "vm/stack_frame.h"
#include "vm/stub_code.h"
#include "vm/symbols.h"

#define __ compiler->assembler()->

namespace dart {

DECLARE_FLAG(bool, emit_edge_counters);
DECLARE_FLAG(int, optimization_counter_threshold);

// List of instructions that are still unimplemented by DBC backend.
#define FOR_EACH_UNIMPLEMENTED_INSTRUCTION(M)                                  \
  M(IndirectGoto)                                                              \
  M(LoadCodeUnits)                                                             \
  M(LoadUntagged)                                                              \
  M(AllocateUninitializedContext)                                              \
  M(BinaryInt32Op)                                                             \
  M(UnaryDoubleOp)                                                             \
  M(SmiToDouble)                                                               \
  M(Int32ToDouble)                                                             \
  M(MintToDouble)                                                              \
  M(DoubleToInteger)                                                           \
  M(DoubleToSmi)                                                               \
  M(DoubleToDouble)                                                            \
  M(DoubleToFloat)                                                             \
  M(FloatToDouble)                                                             \
  M(UnboxedConstant)                                                           \
  M(BinaryDoubleOp)                                                            \
  M(MathUnary)                                                                 \
  M(MathMinMax)                                                                \
  M(Box)                                                                       \
  M(Unbox)                                                                     \
  M(BoxInt64)                                                                  \
  M(CaseInsensitiveCompareUC16)                                                \
  M(BinaryMintOp)                                                              \
  M(ShiftMintOp)                                                               \
  M(UnaryMintOp)                                                               \
  M(InvokeMathCFunction)                                                       \
  M(MergedMath)                                                                \
  M(GuardFieldClass)                                                           \
  M(GuardFieldLength)                                                          \
  M(IfThenElse)                                                                \
  M(BinaryFloat32x4Op)                                                         \
  M(Simd32x4Shuffle)                                                           \
  M(Simd32x4ShuffleMix)                                                        \
  M(Simd32x4GetSignMask)                                                       \
  M(Float32x4Constructor)                                                      \
  M(Float32x4Zero)                                                             \
  M(Float32x4Splat)                                                            \
  M(Float32x4Comparison)                                                       \
  M(Float32x4MinMax)                                                           \
  M(Float32x4Scale)                                                            \
  M(Float32x4Sqrt)                                                             \
  M(Float32x4ZeroArg)                                                          \
  M(Float32x4Clamp)                                                            \
  M(Float32x4With)                                                             \
  M(Float32x4ToInt32x4)                                                        \
  M(Int32x4Constructor)                                                        \
  M(Int32x4BoolConstructor)                                                    \
  M(Int32x4GetFlag)                                                            \
  M(Int32x4Select)                                                             \
  M(Int32x4SetFlag)                                                            \
  M(Int32x4ToFloat32x4)                                                        \
  M(BinaryInt32x4Op)                                                           \
  M(TestCids)                                                                  \
  M(BinaryFloat64x2Op)                                                         \
  M(Float64x2Zero)                                                             \
  M(Float64x2Constructor)                                                      \
  M(Float64x2Splat)                                                            \
  M(Float32x4ToFloat64x2)                                                      \
  M(Float64x2ToFloat32x4)                                                      \
  M(Simd64x2Shuffle)                                                           \
  M(Float64x2ZeroArg)                                                          \
  M(Float64x2OneArg)                                                           \
  M(ExtractNthOutput)                                                          \
  M(BinaryUint32Op)                                                            \
  M(ShiftUint32Op)                                                             \
  M(UnaryUint32Op)                                                             \
  M(UnboxedIntConverter)                                                       \
  M(GrowRegExpStack)                                                           \
  M(BoxInteger32)                                                              \
  M(UnboxInteger32)                                                            \
  M(CheckedSmiOp)                                                              \
  M(CheckArrayBound)                                                           \
  M(RelationalOp)                                                              \
  M(EqualityCompare)                                                           \
  M(LoadIndexed)

// Location summaries actually are not used by the unoptimizing DBC compiler
// because we don't allocate any registers.
static LocationSummary* CreateLocationSummary(
    Zone* zone,
    intptr_t num_inputs,
    Location output = Location::NoLocation(),
    LocationSummary::ContainsCall contains_call = LocationSummary::kNoCall) {
  const intptr_t kNumTemps = 0;
  LocationSummary* locs = new(zone) LocationSummary(
      zone, num_inputs, kNumTemps, contains_call);
  for (intptr_t i = 0; i < num_inputs; i++) {
    locs->set_in(i, (contains_call == LocationSummary::kNoCall) ?
        Location::RequiresRegister() : Location::RegisterLocation(i));
  }
  if (!output.IsInvalid()) {
    // For instructions that call we default to returning result in R0.
    locs->set_out(0, output);
  }
  return locs;
}


#define DEFINE_MAKE_LOCATION_SUMMARY(Name, ...)                                \
  LocationSummary* Name##Instr::MakeLocationSummary(Zone* zone, bool opt)      \
      const {                                                                  \
    return CreateLocationSummary(zone, __VA_ARGS__);                           \
  }                                                                            \

#define EMIT_NATIVE_CODE(Name, ...)                                            \
  DEFINE_MAKE_LOCATION_SUMMARY(Name, __VA_ARGS__);                             \
  void Name##Instr::EmitNativeCode(FlowGraphCompiler* compiler)                \

#define DEFINE_UNIMPLEMENTED_MAKE_LOCATION_SUMMARY(Name)                       \
  LocationSummary* Name##Instr::MakeLocationSummary(Zone* zone, bool opt)      \
      const {                                                                  \
    if (!opt) UNIMPLEMENTED();                                                 \
    return NULL;                                                               \
  }                                                                            \

#define DEFINE_UNIMPLEMENTED_EMIT_NATIVE_CODE(Name)                            \
  void Name##Instr::EmitNativeCode(FlowGraphCompiler* compiler) {              \
    UNIMPLEMENTED();                                                           \
  }

#define DEFINE_UNIMPLEMENTED_EMIT_BRANCH_CODE(Name)                            \
  void Name##Instr::EmitBranchCode(FlowGraphCompiler*, BranchInstr*) {         \
    UNIMPLEMENTED();                                                           \
  }                                                                            \
  Condition Name##Instr::EmitComparisonCode(FlowGraphCompiler*,                \
                                            BranchLabels) {                    \
    UNIMPLEMENTED();                                                           \
    return NEXT_IS_TRUE;                                                       \
  }

#define DEFINE_UNIMPLEMENTED(Name)                                             \
  DEFINE_UNIMPLEMENTED_MAKE_LOCATION_SUMMARY(Name)                             \
  DEFINE_UNIMPLEMENTED_EMIT_NATIVE_CODE(Name)                                  \

FOR_EACH_UNIMPLEMENTED_INSTRUCTION(DEFINE_UNIMPLEMENTED)

#undef DEFINE_UNIMPLEMENTED

DEFINE_UNIMPLEMENTED_EMIT_BRANCH_CODE(TestCids)
DEFINE_UNIMPLEMENTED_EMIT_BRANCH_CODE(RelationalOp)
DEFINE_UNIMPLEMENTED_EMIT_BRANCH_CODE(EqualityCompare)


EMIT_NATIVE_CODE(InstanceOf, 2, Location::SameAsFirstInput(),
                 LocationSummary::kCall) {
  SubtypeTestCache& test_cache = SubtypeTestCache::Handle();
  if (!type().IsVoidType() && type().IsInstantiated()) {
    test_cache = SubtypeTestCache::New();
  }

  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());  // Value.
    __ Push(locs()->in(1).reg());  // Instantiator type arguments.
  }

  __ PushConstant(type());
  __ PushConstant(test_cache);
  __ InstanceOf(negate_result() ? 1 : 0);
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());

  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


DEFINE_MAKE_LOCATION_SUMMARY(AssertAssignable, 2,
                             Location::SameAsFirstInput(),
                             LocationSummary::kCall);


EMIT_NATIVE_CODE(AssertBoolean,
                 1, Location::SameAsFirstInput(),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }
  __ AssertBoolean(Isolate::Current()->type_checks() ? 1 : 0);
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());
  if (compiler->is_optimizing()) {
    __ Drop1();
  }
}


LocationSummary* PolymorphicInstanceCallInstr::MakeLocationSummary(
    Zone* zone, bool optimizing) const {
  return MakeCallSummary(zone);
}


void PolymorphicInstanceCallInstr::EmitNativeCode(FlowGraphCompiler* compiler) {
  Unsupported(compiler);
  UNREACHABLE();
}


EMIT_NATIVE_CODE(Stop, 0) {
  __ Stop(message());
}


EMIT_NATIVE_CODE(CheckStackOverflow,
                 0, Location::NoLocation(),
                 LocationSummary::kCall) {
  __ CheckStack();
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kRuntimeCall,
                                 Thread::kNoDeoptId,
                                 token_pos());
}


EMIT_NATIVE_CODE(PushArgument, 1) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }
}


EMIT_NATIVE_CODE(LoadLocal, 0) {
  ASSERT(!compiler->is_optimizing());
  ASSERT(local().index() != 0);
  __ Push((local().index() > 0) ? (-local().index()) : (-local().index() - 1));
}


EMIT_NATIVE_CODE(StoreLocal, 0) {
  ASSERT(!compiler->is_optimizing());
  ASSERT(local().index() != 0);
  if (HasTemp()) {
    __ StoreLocal(
      (local().index() > 0) ? (-local().index()) : (-local().index() - 1));
  } else {
    __ PopLocal(
      (local().index() > 0) ? (-local().index()) : (-local().index() - 1));
  }
}


EMIT_NATIVE_CODE(LoadClassId, 1, Location::RequiresRegister()) {
  if (compiler->is_optimizing()) {
    __ LoadClassId(locs()->out(0).reg(), locs()->in(0).reg());
  } else {
    __ LoadClassIdTOS();
  }
}


EMIT_NATIVE_CODE(Constant, 0, Location::RequiresRegister()) {
  if (compiler->is_optimizing()) {
    __ LoadConstant(locs()->out(0).reg(), value());
  } else {
    __ PushConstant(value());
  }
}


EMIT_NATIVE_CODE(Return, 1) {
  if (compiler->is_optimizing()) {
    __ Return(locs()->in(0).reg());
  } else {
    __ ReturnTOS();
  }
}


LocationSummary* StoreStaticFieldInstr::MakeLocationSummary(
    Zone* zone, bool opt) const {
  const intptr_t kNumInputs = 1;
  const intptr_t kNumTemps = 1;
  LocationSummary* locs = new(zone) LocationSummary(
      zone, kNumInputs, kNumTemps, LocationSummary::kNoCall);
  for (intptr_t i = 0; i < kNumInputs; i++) {
    locs->set_in(i, Location::RequiresRegister());
  }
  for (intptr_t i = 0; i < kNumTemps; i++) {
    locs->set_temp(i, Location::RequiresRegister());
  }
  return locs;
}


void StoreStaticFieldInstr::EmitNativeCode(FlowGraphCompiler* compiler) {
  if (compiler->is_optimizing()) {
    __ LoadConstant(locs()->temp(0).reg(),
                    Field::ZoneHandle(field().Original()));
    __ StoreField(locs()->temp(0).reg(),
                  Field::static_value_offset() / kWordSize,
                  locs()->in(0).reg());
  } else {
    const intptr_t kidx = __ AddConstant(field());
    __ StoreStaticTOS(kidx);
  }
}


EMIT_NATIVE_CODE(LoadStaticField, 1, Location::RequiresRegister()) {
  if (compiler->is_optimizing()) {
    __ LoadField(locs()->out(0).reg(),
                 locs()->in(0).reg(),
                 Field::static_value_offset() / kWordSize);
  } else {
    const intptr_t kidx = __ AddConstant(StaticField());
    __ PushStatic(kidx);
  }
}


EMIT_NATIVE_CODE(InitStaticField, 0) {
  ASSERT(!compiler->is_optimizing());
  __ InitStaticTOS();
}


EMIT_NATIVE_CODE(ClosureCall,
                 1,
                 Location::RegisterLocation(0),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }

  intptr_t argument_count = ArgumentCount();
  const Array& arguments_descriptor =
      Array::ZoneHandle(ArgumentsDescriptor::New(argument_count,
                                                 argument_names()));
  const intptr_t argdesc_kidx =
      compiler->assembler()->AddConstant(arguments_descriptor);
  __ StaticCall(argument_count, argdesc_kidx);
  compiler->RecordAfterCall(this);

  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


static void EmitBranchOnCondition(FlowGraphCompiler* compiler,
                                  Condition true_condition,
                                  BranchLabels labels) {
  if (true_condition == NEXT_IS_TRUE) {
    __ Jump(labels.true_label);
    if (labels.fall_through != labels.false_label) {
      __ Jump(labels.false_label);
    }
  } else {
    ASSERT(true_condition == NEXT_IS_FALSE);
    __ Jump(labels.false_label);
    if (labels.fall_through != labels.true_label) {
      __ Jump(labels.true_label);
    }
  }
}


Condition StrictCompareInstr::EmitComparisonCode(FlowGraphCompiler* compiler,
                                                 BranchLabels labels) {
  ASSERT((kind() == Token::kNE_STRICT) ||
         (kind() == Token::kEQ_STRICT));

  Token::Kind comparison;
  Condition condition;
  if (labels.fall_through == labels.false_label) {
    condition = NEXT_IS_TRUE;
    comparison = kind();
  } else {
    // Flip comparision to save a jump.
    condition = NEXT_IS_FALSE;
    comparison = (kind() == Token::kEQ_STRICT) ? Token::kNE_STRICT
                                               : Token::kEQ_STRICT;
  }

  if (!compiler->is_optimizing()) {
    const Bytecode::Opcode eq_op = needs_number_check() ?
        Bytecode::kIfEqStrictNumTOS : Bytecode::kIfEqStrictTOS;
    const Bytecode::Opcode ne_op = needs_number_check() ?
        Bytecode::kIfNeStrictNumTOS : Bytecode::kIfNeStrictTOS;
    __ Emit(comparison == Token::kEQ_STRICT ? eq_op : ne_op);
  } else {
    const Bytecode::Opcode eq_op = needs_number_check() ?
        Bytecode::kIfEqStrictNum : Bytecode::kIfEqStrict;
    const Bytecode::Opcode ne_op = needs_number_check() ?
        Bytecode::kIfNeStrictNum : Bytecode::kIfNeStrict;
    __ Emit(Bytecode::Encode(
        (comparison == Token::kEQ_STRICT) ? eq_op : ne_op,
        locs()->in(0).reg(),
        locs()->in(1).reg()));
  }

  if (needs_number_check() && token_pos().IsReal()) {
    compiler->RecordSafepoint(locs());
    compiler->AddCurrentDescriptor(RawPcDescriptors::kRuntimeCall,
                                   Thread::kNoDeoptId,
                                   token_pos());
  }

  return condition;
}


void StrictCompareInstr::EmitBranchCode(FlowGraphCompiler* compiler,
                                        BranchInstr* branch) {
  ASSERT((kind() == Token::kEQ_STRICT) ||
         (kind() == Token::kNE_STRICT));

  BranchLabels labels = compiler->CreateBranchLabels(branch);
  Condition true_condition = EmitComparisonCode(compiler, labels);
  EmitBranchOnCondition(compiler, true_condition, labels);
}


EMIT_NATIVE_CODE(StrictCompare,
                 2,
                 Location::RequiresRegister(),
                 needs_number_check() ? LocationSummary::kCall
                                      : LocationSummary::kNoCall) {
  ASSERT((kind() == Token::kEQ_STRICT) ||
         (kind() == Token::kNE_STRICT));

  Label is_true, is_false;
  BranchLabels labels = { &is_true, &is_false, &is_false };
  Condition true_condition = EmitComparisonCode(compiler, labels);
  EmitBranchOnCondition(compiler, true_condition, labels);
  Label done;
  if (compiler->is_optimizing()) {
    const Register result = locs()->out(0).reg();
    __ Bind(&is_false);
    __ LoadConstant(result, Bool::False());
    __ Jump(&done);
    __ Bind(&is_true);
    __ LoadConstant(result, Bool::True());
    __ Bind(&done);
  } else {
    __ Bind(&is_false);
    __ PushConstant(Bool::False());
    __ Jump(&done);
    __ Bind(&is_true);
    __ PushConstant(Bool::True());
    __ Bind(&done);
  }
}


LocationSummary* BranchInstr::MakeLocationSummary(Zone* zone,
                                                  bool opt) const {
  comparison()->InitializeLocationSummary(zone, opt);
  if (!comparison()->HasLocs()) {
    return NULL;
  }
  // Branches don't produce a result.
  comparison()->locs()->set_out(0, Location::NoLocation());
  return comparison()->locs();
}


void BranchInstr::EmitNativeCode(FlowGraphCompiler* compiler) {
  comparison()->EmitBranchCode(compiler, this);
}


EMIT_NATIVE_CODE(Goto, 0) {
  if (!compiler->is_optimizing()) {
    // Add a deoptimization descriptor for deoptimizing instructions that
    // may be inserted before this instruction.
    compiler->AddCurrentDescriptor(RawPcDescriptors::kDeopt,
                                   GetDeoptId(),
                                   TokenPosition::kNoSource);
  }
  if (HasParallelMove()) {
    compiler->parallel_move_resolver()->EmitNativeCode(parallel_move());
  }
  // We can fall through if the successor is the next block in the list.
  // Otherwise, we need a jump.
  if (!compiler->CanFallThroughTo(successor())) {
    __ Jump(compiler->GetJumpLabel(successor()));
  }
}


Condition TestSmiInstr::EmitComparisonCode(FlowGraphCompiler* compiler,
                                           BranchLabels labels) {
  ASSERT((kind() == Token::kEQ) ||
         (kind() == Token::kNE));
  Register left = locs()->in(0).reg();
  Register right = locs()->in(1).reg();
  __ TestSmi(left, right);
  return (kind() == Token::kEQ) ? NEXT_IS_TRUE : NEXT_IS_FALSE;
}


void TestSmiInstr::EmitBranchCode(FlowGraphCompiler* compiler,
                                  BranchInstr* branch) {
  BranchLabels labels = compiler->CreateBranchLabels(branch);
  Condition true_condition = EmitComparisonCode(compiler, labels);
  EmitBranchOnCondition(compiler, true_condition, labels);
}


EMIT_NATIVE_CODE(TestSmi,
                 2,
                 Location::RequiresRegister(),
                 LocationSummary::kNoCall) {
  // Never emitted outside of the BranchInstr.
  UNREACHABLE();
}


EMIT_NATIVE_CODE(CreateArray,
                 2, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
    __ Push(locs()->in(1).reg());
  }
  __ CreateArrayTOS();
  compiler->RecordSafepoint(locs());
  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


EMIT_NATIVE_CODE(StoreIndexed, 3) {
  if (compiler->is_optimizing()) {
    if (class_id() != kArrayCid) {
      Unsupported(compiler);
      UNREACHABLE();
    }
    __ StoreIndexed(locs()->in(kArrayPos).reg(),
                    locs()->in(kIndexPos).reg(),
                    locs()->in(kValuePos).reg());
  } else {
    ASSERT(class_id() == kArrayCid);
    __ StoreIndexedTOS();
  }
}


EMIT_NATIVE_CODE(StringInterpolate,
                 1, Location::RegisterLocation(0),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }
  const intptr_t kArgumentCount = 1;
  const Array& arguments_descriptor = Array::Handle(
      ArgumentsDescriptor::New(kArgumentCount, Object::null_array()));
  __ PushConstant(CallFunction());
  const intptr_t argdesc_kidx = __ AddConstant(arguments_descriptor);
  __ StaticCall(kArgumentCount, argdesc_kidx);
  compiler->RecordAfterCall(this);

  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


EMIT_NATIVE_CODE(NativeCall,
                 0, Location::NoLocation(),
                 LocationSummary::kCall) {
  SetupNative();

  const intptr_t argc_tag = NativeArguments::ComputeArgcTag(function());

  ASSERT(!link_lazily());
  const ExternalLabel label(reinterpret_cast<uword>(native_c_function()));
  const intptr_t target_kidx =
      __ object_pool_wrapper().FindImmediate(label.address());
  const intptr_t argc_tag_kidx =
      __ object_pool_wrapper().FindImmediate(static_cast<uword>(argc_tag));
  __ PushConstant(target_kidx);
  __ PushConstant(argc_tag_kidx);
  if (is_bootstrap_native()) {
    __ NativeBootstrapCall();
  } else {
    __ NativeCall();
  }
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 Thread::kNoDeoptId,
                                 token_pos());
}


EMIT_NATIVE_CODE(OneByteStringFromCharCode,
                 1, Location::RequiresRegister(),
                 LocationSummary::kNoCall) {
  ASSERT(compiler->is_optimizing());
  const Register char_code = locs()->in(0).reg();  // Char code is a smi.
  const Register result = locs()->out(0).reg();
  __ OneByteStringFromCharCode(result, char_code);
}


EMIT_NATIVE_CODE(StringToCharCode,
                 1, Location::RequiresRegister(),
                 LocationSummary::kNoCall) {
  ASSERT(cid_ == kOneByteStringCid);
  const Register str = locs()->in(0).reg();
  const Register result = locs()->out(0).reg();  // Result char code is a smi.
  __ StringToCharCode(result, str);
}



EMIT_NATIVE_CODE(AllocateObject,
                 0, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  if (ArgumentCount() == 1) {
    __ PushConstant(cls());
    __ AllocateT();
    compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                   Thread::kNoDeoptId,
                                   token_pos());
  } else {
    const intptr_t kidx = __ AddConstant(cls());
    __ Allocate(kidx);
    compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                   Thread::kNoDeoptId,
                                   token_pos());
  }
  compiler->RecordSafepoint(locs());
  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


EMIT_NATIVE_CODE(StoreInstanceField, 2) {
  ASSERT(!HasTemp());
  ASSERT(offset_in_bytes() % kWordSize == 0);
  if (compiler->is_optimizing()) {
    const Register value = locs()->in(1).reg();
    const Register instance = locs()->in(0).reg();
    __ StoreField(instance, offset_in_bytes() / kWordSize, value);
  } else {
    __ StoreFieldTOS(offset_in_bytes() / kWordSize);
  }
}


EMIT_NATIVE_CODE(LoadField, 1, Location::RequiresRegister()) {
  ASSERT(offset_in_bytes() % kWordSize == 0);
  if (compiler->is_optimizing()) {
    const Register result = locs()->out(0).reg();
    const Register instance = locs()->in(0).reg();
    __ LoadField(result, instance, offset_in_bytes() / kWordSize);
  } else {
    __ LoadFieldTOS(offset_in_bytes() / kWordSize);
  }
}


EMIT_NATIVE_CODE(BooleanNegate, 1, Location::RequiresRegister()) {
  if (compiler->is_optimizing()) {
    __ BooleanNegate(locs()->out(0).reg(), locs()->in(0).reg());
  } else {
    __ BooleanNegateTOS();
  }
}


EMIT_NATIVE_CODE(AllocateContext,
                 0, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  ASSERT(!compiler->is_optimizing());
  __ AllocateContext(num_context_variables());
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 Thread::kNoDeoptId,
                                 token_pos());
}


EMIT_NATIVE_CODE(CloneContext,
                 1, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  ASSERT(!compiler->is_optimizing());
  __ CloneContext();
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 Thread::kNoDeoptId,
                                 token_pos());
}


EMIT_NATIVE_CODE(CatchBlockEntry, 0) {
  __ Bind(compiler->GetJumpLabel(this));
  compiler->AddExceptionHandler(catch_try_index(),
                                try_index(),
                                compiler->assembler()->CodeSize(),
                                catch_handler_types_,
                                needs_stacktrace());
  __ MoveSpecial(-exception_var().index()-1,
                 Simulator::kExceptionSpecialIndex);
  __ MoveSpecial(-stacktrace_var().index()-1,
                 Simulator::kStacktraceSpecialIndex);
  __ SetFrame(compiler->StackSize());
}


EMIT_NATIVE_CODE(Throw, 0, Location::NoLocation(), LocationSummary::kCall) {
  __ Throw(0);
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());
  __ Trap();
}


EMIT_NATIVE_CODE(ReThrow, 0, Location::NoLocation(), LocationSummary::kCall) {
  compiler->SetNeedsStacktrace(catch_try_index());
  __ Throw(1);
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());
  __ Trap();
}

EMIT_NATIVE_CODE(InstantiateType,
                 1, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }
  __ InstantiateType(__ AddConstant(type()));
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());
  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}

EMIT_NATIVE_CODE(InstantiateTypeArguments,
                 1, Location::RequiresRegister(),
                 LocationSummary::kCall) {
  if (compiler->is_optimizing()) {
    __ Push(locs()->in(0).reg());
  }
  __ InstantiateTypeArgumentsTOS(
      type_arguments().IsRawInstantiatedRaw(type_arguments().Length()),
      __ AddConstant(type_arguments()));
  compiler->RecordSafepoint(locs());
  compiler->AddCurrentDescriptor(RawPcDescriptors::kOther,
                                 deopt_id(),
                                 token_pos());
  if (compiler->is_optimizing()) {
    __ PopLocal(locs()->out(0).reg());
  }
}


void DebugStepCheckInstr::EmitNativeCode(FlowGraphCompiler* compiler) {
  __ DebugStep();
  compiler->AddCurrentDescriptor(stub_kind_, Thread::kNoDeoptId, token_pos());
}


void GraphEntryInstr::EmitNativeCode(FlowGraphCompiler* compiler) {
  if (!compiler->CanFallThroughTo(normal_entry())) {
    __ Jump(compiler->GetJumpLabel(normal_entry()));
  }
}


LocationSummary* Instruction::MakeCallSummary(Zone* zone) {
  LocationSummary* result = new(zone) LocationSummary(
      zone, 0, 0, LocationSummary::kCall);
  // TODO(vegorov) support allocating out registers for calls.
  // Currently we require them to be fixed.
  result->set_out(0, Location::RegisterLocation(0));
  return result;
}


CompileType BinaryUint32OpInstr::ComputeType() const {
  return CompileType::Int();
}


CompileType ShiftUint32OpInstr::ComputeType() const {
  return CompileType::Int();
}


CompileType UnaryUint32OpInstr::ComputeType() const {
  return CompileType::Int();
}


static const intptr_t kMintShiftCountLimit = 63;


bool ShiftMintOpInstr::has_shift_count_check() const {
  return !RangeUtils::IsWithin(
      right()->definition()->range(), 0, kMintShiftCountLimit);
}


CompileType LoadIndexedInstr::ComputeType() const {
  switch (class_id_) {
    case kArrayCid:
    case kImmutableArrayCid:
      return CompileType::Dynamic();

    case kTypedDataFloat32ArrayCid:
    case kTypedDataFloat64ArrayCid:
      return CompileType::FromCid(kDoubleCid);
    case kTypedDataFloat32x4ArrayCid:
      return CompileType::FromCid(kFloat32x4Cid);
    case kTypedDataInt32x4ArrayCid:
      return CompileType::FromCid(kInt32x4Cid);
    case kTypedDataFloat64x2ArrayCid:
      return CompileType::FromCid(kFloat64x2Cid);

    case kTypedDataInt8ArrayCid:
    case kTypedDataUint8ArrayCid:
    case kTypedDataUint8ClampedArrayCid:
    case kExternalTypedDataUint8ArrayCid:
    case kExternalTypedDataUint8ClampedArrayCid:
    case kTypedDataInt16ArrayCid:
    case kTypedDataUint16ArrayCid:
    case kOneByteStringCid:
    case kTwoByteStringCid:
    case kExternalOneByteStringCid:
    case kExternalTwoByteStringCid:
      return CompileType::FromCid(kSmiCid);

    case kTypedDataInt32ArrayCid:
    case kTypedDataUint32ArrayCid:
      return CompileType::Int();

    default:
      UNREACHABLE();
      return CompileType::Dynamic();
  }
}


Representation LoadIndexedInstr::representation() const {
  switch (class_id_) {
    case kArrayCid:
    case kImmutableArrayCid:
    case kTypedDataInt8ArrayCid:
    case kTypedDataUint8ArrayCid:
    case kTypedDataUint8ClampedArrayCid:
    case kExternalTypedDataUint8ArrayCid:
    case kExternalTypedDataUint8ClampedArrayCid:
    case kTypedDataInt16ArrayCid:
    case kTypedDataUint16ArrayCid:
    case kOneByteStringCid:
    case kTwoByteStringCid:
    case kExternalOneByteStringCid:
    case kExternalTwoByteStringCid:
      return kTagged;
    case kTypedDataInt32ArrayCid:
      return kUnboxedInt32;
    case kTypedDataUint32ArrayCid:
      return kUnboxedUint32;
    case kTypedDataFloat32ArrayCid:
    case kTypedDataFloat64ArrayCid:
      return kUnboxedDouble;
    case kTypedDataInt32x4ArrayCid:
      return kUnboxedInt32x4;
    case kTypedDataFloat32x4ArrayCid:
      return kUnboxedFloat32x4;
    case kTypedDataFloat64x2ArrayCid:
      return kUnboxedFloat64x2;
    default:
      UNREACHABLE();
      return kTagged;
  }
}


Representation StoreIndexedInstr::RequiredInputRepresentation(
    intptr_t idx) const {
  // Array can be a Dart object or a pointer to external data.
  if (idx == 0) {
    return kNoRepresentation;  // Flexible input representation.
  }
  if (idx == 1) {
    return kTagged;  // Index is a smi.
  }
  ASSERT(idx == 2);
  switch (class_id_) {
    case kArrayCid:
    case kOneByteStringCid:
    case kTwoByteStringCid:
    case kExternalOneByteStringCid:
    case kExternalTwoByteStringCid:
    case kTypedDataInt8ArrayCid:
    case kTypedDataUint8ArrayCid:
    case kExternalTypedDataUint8ArrayCid:
    case kTypedDataUint8ClampedArrayCid:
    case kExternalTypedDataUint8ClampedArrayCid:
    case kTypedDataInt16ArrayCid:
    case kTypedDataUint16ArrayCid:
      return kTagged;
    case kTypedDataInt32ArrayCid:
      return kUnboxedInt32;
    case kTypedDataUint32ArrayCid:
      return kUnboxedUint32;
    case kTypedDataFloat32ArrayCid:
    case kTypedDataFloat64ArrayCid:
      return kUnboxedDouble;
    case kTypedDataFloat32x4ArrayCid:
      return kUnboxedFloat32x4;
    case kTypedDataInt32x4ArrayCid:
      return kUnboxedInt32x4;
    case kTypedDataFloat64x2ArrayCid:
      return kUnboxedFloat64x2;
    default:
      UNREACHABLE();
      return kTagged;
  }
}


void Environment::DropArguments(intptr_t argc) {
#if defined(DEBUG)
    // Check that we are in the backend - register allocation has been run.
    ASSERT(locations_ != NULL);

    // Check that we are only dropping PushArgument instructions from the
    // environment.
    ASSERT(argc <= values_.length());
    for (intptr_t i = 0; i < argc; i++) {
      ASSERT(values_[values_.length() - i - 1]->definition()->IsPushArgument());
    }
#endif
    values_.TruncateTo(values_.length() - argc);
}


EMIT_NATIVE_CODE(CheckSmi, 1) {
  __ CheckSmi(locs()->in(0).reg());
  compiler->EmitDeopt(deopt_id(),
                      ICData::kDeoptCheckSmi,
                      licm_hoisted_ ? ICData::kHoisted : 0);
}


EMIT_NATIVE_CODE(CheckEitherNonSmi, 2) {
  intptr_t left_cid = left()->Type()->ToCid();
  intptr_t right_cid = right()->Type()->ToCid();
  const Register left = locs()->in(0).reg();
  const Register right = locs()->in(1).reg();
  if (this->left()->definition() == this->right()->definition()) {
    __ CheckSmi(left);
  } else if (left_cid == kSmiCid) {
    __ CheckSmi(right);
  } else if (right_cid == kSmiCid) {
    __ CheckSmi(left);
  } else {
    __ CheckSmi(left);
    compiler->EmitDeopt(deopt_id(), ICData::kDeoptBinaryDoubleOp,
                        licm_hoisted_ ? ICData::kHoisted : 0);
    __ CheckSmi(right);
  }
  compiler->EmitDeopt(deopt_id(), ICData::kDeoptBinaryDoubleOp,
                      licm_hoisted_ ? ICData::kHoisted : 0);
}


EMIT_NATIVE_CODE(CheckClassId, 1) {
  __ CheckClassId(locs()->in(0).reg(),
                  compiler->ToEmbeddableCid(cid_, this));
  compiler->EmitDeopt(deopt_id(), ICData::kDeoptCheckClass);
}


EMIT_NATIVE_CODE(CheckClass, 1) {
  const Register value = locs()->in(0).reg();
  if (IsNullCheck()) {
    ASSERT(DeoptIfNull() || DeoptIfNotNull());
    if (DeoptIfNull()) {
      __ IfEqNull(value);
    } else {
      __ IfNeNull(value);
    }
  } else {
    ASSERT((unary_checks().GetReceiverClassIdAt(0) != kSmiCid) ||
           (unary_checks().NumberOfChecks() > 1));
    const intptr_t may_be_smi =
        (unary_checks().GetReceiverClassIdAt(0) == kSmiCid) ? 1 : 0;
    if (IsDenseSwitch()) {
      ASSERT(cids_[0] < cids_[cids_.length() - 1]);
      const intptr_t low_cid = cids_[0];
      const intptr_t cid_mask = ComputeCidMask();
      __ CheckDenseSwitch(value, may_be_smi);
      __ Nop(compiler->ToEmbeddableCid(low_cid, this));
      __ Nop(__ AddConstant(Smi::Handle(Smi::New(cid_mask))));
    } else {
      GrowableArray<CidTarget> sorted_ic_data;
      FlowGraphCompiler::SortICDataByCount(unary_checks(),
                                           &sorted_ic_data,
                                           /* drop_smi = */ true);
      const intptr_t sorted_length = sorted_ic_data.length();
      if (!Utils::IsUint(8, sorted_length)) {
        Unsupported(compiler);
        UNREACHABLE();
      }
      __ CheckCids(value, may_be_smi, sorted_length);
      for (intptr_t i = 0; i < sorted_length; i++) {
        __ Nop(compiler->ToEmbeddableCid(sorted_ic_data[i].cid, this));
      }
    }
  }
  compiler->EmitDeopt(deopt_id(),
                      ICData::kDeoptCheckClass,
                      licm_hoisted_ ? ICData::kHoisted : 0);
}


EMIT_NATIVE_CODE(BinarySmiOp, 2, Location::RequiresRegister()) {
  const Register left = locs()->in(0).reg();
  const Register right = locs()->in(1).reg();
  const Register out = locs()->out(0).reg();
  const bool can_deopt = CanDeoptimize();
  bool needs_nop = false;
  switch (op_kind()) {
    case Token::kADD:
      __ Add(out, left, right);
      needs_nop = true;
      break;
    case Token::kSUB:
      __ Sub(out, left, right);
      needs_nop = true;
      break;
    case Token::kMUL:
      __ Mul(out, left, right);
      needs_nop = true;
      break;
    case Token::kTRUNCDIV:
      ASSERT(can_deopt);
      __ Div(out, left, right);
      break;
    case Token::kBIT_AND:
      ASSERT(!can_deopt);
      __ BitAnd(out, left, right);
      break;
    case Token::kBIT_OR:
      ASSERT(!can_deopt);
      __ BitOr(out, left, right);
      break;
    case Token::kBIT_XOR:
      ASSERT(!can_deopt);
      __ BitXor(out, left, right);
      break;
    case Token::kMOD:
      __ Mod(out, left, right);
      needs_nop = true;
      break;
    case Token::kSHR:
      __ Shr(out, left, right);
      needs_nop = true;
      break;
    case Token::kSHL:
      __ Shl(out, left, right);
      needs_nop = true;
      break;
    default:
      UNREACHABLE();
  }
  if (can_deopt) {
    compiler->EmitDeopt(deopt_id(), ICData::kDeoptBinarySmiOp);
  } else if (needs_nop) {
    __ Nop(0);
  }
}


EMIT_NATIVE_CODE(UnarySmiOp, 1, Location::RequiresRegister()) {
  switch (op_kind()) {
    case Token::kNEGATE: {
      __ Neg(locs()->out(0).reg(), locs()->in(0).reg());
      compiler->EmitDeopt(deopt_id(), ICData::kDeoptUnaryOp);
      break;
    }
    case Token::kBIT_NOT:
      __ BitNot(locs()->out(0).reg(), locs()->in(0).reg());
      break;
    default:
      UNREACHABLE();
  }
}

}  // namespace dart

#endif  // defined TARGET_ARCH_DBC
