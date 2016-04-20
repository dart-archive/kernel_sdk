// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/compiler.h"
#include "vm/dil_to_il.h"
#include "vm/intermediate_language.h"
#include "vm/stack_frame.h"

namespace dart {
namespace dil {

#define Z (zone_)

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


void FlowGraphBuilder::AddVariable(VariableDeclaration* declaration,
				   LocalVariable* variable) {
  parsed_function_.node_sequence()->scope()->AddVariable(variable);
  variables_[declaration] = variable;
}

void FlowGraphBuilder::Push(Definition* definition) {
  Value* value = new(Z) Value(definition);
  definition->set_temp_index(
      stack_ == NULL ? 0 : stack_->definition()->temp_index() + 1);
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
     VisitStatement(procedure_->function()->body()));

  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  null->set_temp_index(0);
  body = body << null <<
      new(Z) ReturnInstr(TokenPosition::kNoSource, new Value(null));

  return new(Z) FlowGraph(parsed_function_, graph_entry, next_block_id_ - 1);
}


void FlowGraphBuilder::BuildConstant(const Object& value) {
  ConstantInstr* constant = new(Z) ConstantInstr(value);
  Push(constant);
  fragment_ = constant;
}


void FlowGraphBuilder::VisitNullLiteral(NullLiteral* node) {
  BuildConstant(Instance::ZoneHandle(Z, Instance::null()));
}


void FlowGraphBuilder::VisitBoolLiteral(BoolLiteral* node) {
  BuildConstant(Bool::Get(node->value()));
}


void FlowGraphBuilder::VisitIntLiteral(IntLiteral* node) {
  BuildConstant(
      Integer::ZoneHandle(Z, Integer::New(node->value(), Heap::kOld)));
}


void FlowGraphBuilder::VisitBigintLiteral(BigintLiteral* node) {
  const dart::String& value = dart::String::Handle(
      dart::String::FromUTF8(node->value()->buffer(), node->value()->size()));
  BuildConstant(
      Integer::ZoneHandle(Z, Integer::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitDoubleLiteral(DoubleLiteral* node) {
  const dart::String& value = dart::String::Handle(
      dart::String::FromUTF8(node->value()->buffer(), node->value()->size()));
  BuildConstant(
      Double::ZoneHandle(Z, Double::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitStringLiteral(StringLiteral* node) {
  BuildConstant(dart::String::ZoneHandle(Z,
      dart::String::FromUTF8(node->value()->buffer(), node->value()->size(),
	                     Heap::kOld)));
}


void FlowGraphBuilder::VisitVariableGet(VariableGet* node) {
  LocalVariable* local = variables_[node->variable()];
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*local, TokenPosition::kNoSource);
  Push(load);
  fragment_ = load;
}


void FlowGraphBuilder::VisitVariableSet(VariableSet* node) {
  LocalVariable* local = variables_[node->variable()];
  Fragment instructions = VisitExpression(node->expression());
  Value* value = Pop();
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*local, value, TokenPosition::kNoSource);
  Push(store);
  fragment_ = instructions << store;
}


void FlowGraphBuilder::VisitStaticGet(StaticGet* node) {
  Member* target = node->target();
  const dart::String& name_string = dart::String::Handle(
      dart::String::FromUTF8(target->name()->string()->buffer(),
	                     target->name()->string()->size()));
  const Script& script =
      Script::Handle(parsed_function_.function().script());
  const dart::Library& library = dart::Library::Handle(script.FindLibrary());
  if (target->IsField()) {
    const dart::Field& field =
	dart::Field::ZoneHandle(Z, library.LookupFieldAllowPrivate(name_string));
    const dart::Class& owner = dart::Class::Handle(field.Owner());
    const dart::String& getter_name =
	dart::String::Handle(dart::Field::GetterSymbol(name_string));
    const Function& getter =
	Function::ZoneHandle(Z, owner.LookupStaticFunction(getter_name));
    if (getter.IsNull() || field.is_const()) {
      ConstantInstr* constant = new(Z) ConstantInstr(field);
      constant->set_temp_index(
	  stack_ == NULL ? 0 : stack_->definition()->temp_index() + 1);
      Fragment instructions = constant;
      LoadStaticFieldInstr* load =
	  new(Z) LoadStaticFieldInstr(new Value(constant),
	                              TokenPosition::kNoSource);
      Push(load);
      fragment_ = instructions << load;
      return;
    }
    ZoneGrowableArray<PushArgumentInstr*>* arguments =
	new(Z) ZoneGrowableArray<PushArgumentInstr*>(0);
    ZoneGrowableArray<const ICData*> ic_data_array(Z, 0);
    StaticCallInstr* call =
	new(Z) StaticCallInstr(TokenPosition::kNoSource,
	                       getter,
             	               Object::null_array(),
	                       arguments,
	                       ic_data_array);
    Push(call);
    fragment_ = call;
    return;
  }
  ASSERT(target->IsProcedure());
  UNREACHABLE();
}


void FlowGraphBuilder::VisitStaticSet(StaticSet* node) {
  Member* target = node->target();
  const dart::String& name_string = dart::String::Handle(
      dart::String::FromUTF8(target->name()->string()->buffer(),
	                     target->name()->string()->size()));
  const Script& script =
      Script::Handle(parsed_function_.function().script());
  const dart::Library& library = dart::Library::Handle(script.FindLibrary());
  if (target->IsField()) {
    const dart::Field& field =
	dart::Field::ZoneHandle(Z, library.LookupFieldAllowPrivate(name_string));

    Fragment instructions = VisitExpression(node->expression());
    Value* value = Pop();
    PushTempInstr* temp = new(Z) PushTempInstr(value);
    instructions = instructions << temp;
    Push(temp);
    
    char name[64];
    int index = value->definition()->temp_index();
    OS::SNPrint(name, 64, ":temp%d", index);
    LocalVariable* variable =
	new(Z) LocalVariable(TokenPosition::kNoSource,
	                     dart::String::ZoneHandle(Z, Symbols::New(name)),
	                     *value->Type()->ToAbstractType());
    variable->set_index(
	kFirstLocalSlotFromFp - index - 2 - pending_argument_count_);

    LoadLocalInstr* load =
	new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
    instructions = instructions << load;
    Push(load);

    StoreStaticFieldInstr* store =
	new(Z) StoreStaticFieldInstr(field, Pop(), TokenPosition::kNoSource);
    instructions = instructions << store;

    load = new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
    instructions = instructions << load;
    Push(load);

    DropTempsInstr* drop = new(Z) DropTempsInstr(1, Pop());
    Drop();  // PushTemp.
    Push(drop);
    fragment_ = instructions << drop;
  } else {
    ASSERT(target->IsProcedure());
    UNREACHABLE();
  }
}


void FlowGraphBuilder::VisitPropertyGet(PropertyGet* node) {
  Fragment instructions = VisitExpression(node->receiver());
  Value* receiver = Pop();
  PushArgumentInstr* receiver_argument = new(Z) PushArgumentInstr(receiver);
  instructions = instructions << receiver_argument;

  // The pending argument is consumed immediately.
  String* name = node->name()->string();
  const dart::String& name_string = dart::String::Handle(
      dart::String::FromUTF8(name->buffer(), name->size()));
  const dart::String& getter_name = dart::String::ZoneHandle(Z,
    Symbols::New(dart::String::Handle(dart::Field::GetterSymbol(name_string))));
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(1);
  arguments->Add(receiver_argument);
  ZoneGrowableArray<const ICData*> ic_data_array(Z, 0);
  InstanceCallInstr* get =
      new(Z) InstanceCallInstr(TokenPosition::kNoSource,
	                       getter_name,
	                       Token::kGET,
          	               arguments,
	                       Object::null_array(),
	                       1,
	                       ic_data_array);
  Push(get);
  fragment_ = instructions << get;
}


void FlowGraphBuilder::VisitPropertySet(PropertySet* node) {
  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Fragment instructions = null;
  Push(null);
  PushTempInstr* temp = new(Z) PushTempInstr(Pop());
  instructions = instructions << temp;
  Push(temp);

  char variable_name[64];
  int index = null->temp_index();
  OS::SNPrint(variable_name, 64, ":temp%d", index);
  LocalVariable* variable =
      new(Z) LocalVariable(TokenPosition::kNoSource,
          dart::String::ZoneHandle(Z, Symbols::New(variable_name)),
	*null->Type()->ToAbstractType());
  variable->set_index(
      kFirstLocalSlotFromFp - index - 2 - pending_argument_count_);

  instructions = instructions + VisitExpression(node->receiver());
  Value* receiver = Pop();
  PushArgumentInstr* receiver_argument = new(Z) PushArgumentInstr(receiver);
  instructions = instructions << receiver_argument;
  ++pending_argument_count_;
  
  instructions = instructions + VisitExpression(node->value());
  Value* value = Pop();
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*variable, value, TokenPosition::kNoSource);
  instructions = instructions << store;
  Push(store);

  PushArgumentInstr* value_argument = new(Z) PushArgumentInstr(Pop());
  instructions = instructions << value_argument;
  // The pending argument is consumed immediately.

  String* name = node->name()->string();
  const dart::String& name_string = dart::String::Handle(
      dart::String::FromUTF8(name->buffer(), name->size()));
  const dart::String& setter_name = dart::String::ZoneHandle(Z,
    Symbols::New(dart::String::Handle(dart::Field::SetterSymbol(name_string))));
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(2);
  arguments->Add(receiver_argument);
  arguments->Add(value_argument);
  ZoneGrowableArray<const ICData*> ic_data_array(Z, 0);
  InstanceCallInstr* set =
      new(Z) InstanceCallInstr(TokenPosition::kNoSource,
	                       setter_name,
	                       Token::kSET,
          	               arguments,
	                       Object::null_array(),
	                       1,
	                       ic_data_array);
  --pending_argument_count_;
  ASSERT(pending_argument_count_ >= 0);
  instructions = instructions << set;

  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  instructions = instructions << load;
  Push(load);

  DropTempsInstr* drop = new(Z) DropTempsInstr(1, Pop());
  Drop();  // PushTemp.
  Push(drop);
  fragment_ = instructions << drop;
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
  pending_argument_count_ -= node->arguments()->positional().length();
  ASSERT(pending_argument_count_ >= 0);
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
    ++pending_argument_count_;
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


void FlowGraphBuilder::VisitVariableDeclaration(VariableDeclaration* node) {
  const dart::String& name =
    dart::String::Handle(dart::String::FromUTF8(node->name()->buffer(),
						node->name()->size()));
  const dart::String& symbol = dart::String::ZoneHandle(Z, Symbols::New(name));
  LocalVariable* local =
    new LocalVariable(TokenPosition::kNoSource, symbol,
		      Type::ZoneHandle(Z, Type::DynamicType()));

  AddVariable(node, local);

  Fragment instructions;
  Value* value;
  if (node->initializer() == NULL) {
    ConstantInstr* null =
	new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(
	stack_ == NULL ? 0 : stack_->definition()->temp_index() + 1);
    instructions = null;
    value = new Value(null);
  } else {
    instructions = VisitExpression(node->initializer());
    value = Pop();
  }
  fragment_ = instructions <<
      new(Z) StoreLocalInstr(*local, value, TokenPosition::kNoSource);
}


}  // namespace dil
}  // namespace dart
