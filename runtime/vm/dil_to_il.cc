// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler.h"
#include "vm/dil_to_il.h"
#include "vm/intermediate_language.h"

namespace dart {
namespace dil {

#define Z (zone())

Fragment operator+(const Fragment& first, const Fragment& second) {
  if (first.entry == NULL) return second;
  if (first.current == NULL || second.entry == NULL) return first;
  first.current->LinkTo(second.entry);
  return Fragment(first.entry, second.current);
}


Fragment operator<<(const Fragment& fragment, Instruction* next) {
  if (fragment.entry == NULL) return Fragment(next, next);
  if (fragment.current == NULL) return fragment;
  fragment.current->LinkTo(next);
  return Fragment(fragment.entry, next);
}


Fragment operator>>(Instruction* entry, const Fragment& fragment) {
  if (fragment.entry == NULL) return Fragment(entry, entry);
  entry->LinkTo(fragment.entry);
  return Fragment(entry, fragment.current);
}


Fragment operator!(const Fragment& fragment) {
  ASSERT(fragment.entry != NULL);
  return Fragment(fragment.entry, NULL);
}


void FlowGraphBuilder::Push(Definition* definition) {
  Value* value = new(Z) Value(definition);
  definition->set_temp_index(
      stack_ == NULL ? 0 : stack_->definition()->temp_index());
  Value::AddToList(value, &stack_);
}


Value* FlowGraphBuilder::Pop() {
  ASSERT(stack_ != NULL);
  Value* value = stack_;
  stack_ = value->next_use();
  return value;
}


void FlowGraphBuilder::Drop() {
  ASSERT(stack_ != NULL);
  stack_->definition()->set_temp_index(-1);
  stack_ = stack_->next_use();
}


FlowGraph* FlowGraphBuilder::BuildGraph() {
  TargetEntryInstr* normal_entry =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  GraphEntryInstr* graph_entry =
      new(Z) GraphEntryInstr(parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

  Fragment body =
    normal_entry >>
    (new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource, 0) >>
     VisitStatement(procedure()->function()->body()));

  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  null->set_temp_index(0);
  body = body << null <<
      new(Z) ReturnInstr(TokenPosition::kNoSource, new Value(null));

  return new(Z) FlowGraph(parsed_function_, graph_entry, next_block_id_ - 1);
}


void FlowGraphBuilder::VisitNullLiteral(NullLiteral* node) {
  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Push(null);
  fragment_ = null;
}


void FlowGraphBuilder::VisitStringLiteral(StringLiteral* node) {
  ConstantInstr* string =
      new(Z) ConstantInstr(dart::String::ZoneHandle(Z,
          dart::String::FromUTF8(node->value()->buffer(),
                                 node->value()->size(),
                                 Heap::kOld)));
  Push(string);
  fragment_ = string;
}


void FlowGraphBuilder::VisitStaticInvocation(StaticInvocation* node) {
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      TranslateArguments(node->arguments());
  Fragment instructions = fragment_;

  Name* target_name = node->procedure()->name();
  const dart::String& name_string = dart::String::Handle(
      dart::String::FromUTF8(target_name->string()->buffer(),
                             target_name->string()->size()));
  const Script& script =
      Script::Handle(parsed_function_.function().script());
  const dart::Library& library = dart::Library::Handle(script.FindLibrary());
  const Function& target = 
      Function::ZoneHandle(Z, library.LookupFunctionAllowPrivate(name_string));
  ZoneGrowableArray<const ICData*> ic_data_array(Z, 0);
  StaticCallInstr* call =
      new(Z) StaticCallInstr(TokenPosition::kNoSource,
                             target,
                             Object::null_array(),
                             arguments,
                             ic_data_array);
  Push(call);
  fragment_ = instructions << call;
}


ZoneGrowableArray<PushArgumentInstr*>* FlowGraphBuilder::TranslateArguments(
    Arguments* node) {
  if (node->types().length() != 0 || node->named().length() != 0) {
    UNIMPLEMENTED();
  }
  Fragment instructions;
  List<Expression>& positional = node->positional();
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
    new(Z) ZoneGrowableArray<PushArgumentInstr*>(positional.length());
  for (int i = 0; i < positional.length(); ++i) {
    instructions = instructions + VisitExpression(positional[i]);
    Value* argument = Pop();
    PushArgumentInstr* push = new(Z) PushArgumentInstr(argument);
    arguments->Add(push);
    instructions = instructions << push;
  }
  fragment_ = instructions;
  return arguments;
}


void FlowGraphBuilder::VisitBlock(Block* node) {
  Fragment instructions;
  List<Statement>& statements = node->statements();
  for (int i = 0; i < statements.length(); ++i) {
    instructions = instructions + VisitStatement(statements[i]);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitReturnStatement(ReturnStatement* node) {
  Fragment instructions;
  Value* result;
  if (node->expression() == NULL) {
    ConstantInstr* null =
        new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    instructions = null;
    result = new Value(null);
  } else {
    instructions = VisitExpression(node->expression());
    result = Pop();
  }
  instructions = instructions <<
      new(Z) ReturnInstr(TokenPosition::kNoSource, result);
  fragment_ = !instructions;
}


void FlowGraphBuilder::VisitExpressionStatement(ExpressionStatement* node) {
  Fragment instructions = VisitExpression(node->expression());
  Drop();
  fragment_ = instructions;  // Unnecessary.
}


}  // namespace dil
}  // namespace dart
