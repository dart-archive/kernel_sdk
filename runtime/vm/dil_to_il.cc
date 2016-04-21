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

Fragment& Fragment::operator+=(const Fragment& other) {
  if (entry == NULL) {
    entry = other.entry;
    current = other.current;
  } else if (current != NULL && other.entry != NULL) {
    current->LinkTo(other.entry);
    current = other.current;
  }
  return *this;
}


Fragment& Fragment::operator<<=(Instruction* next) {
  if (entry == NULL) {
    entry = current = next;
  } else if (current != NULL) {
    current->LinkTo(next);
    current = next;
  }
  return *this;
}


Fragment Fragment::closed() {
  ASSERT(entry != NULL);
  return Fragment(entry, NULL);
}


Fragment operator+(const Fragment& first, const Fragment& second) {
  Fragment result = first;
  result += second;
  return result;
}


Fragment operator<<(const Fragment& fragment, Instruction* next) {
  Fragment result = fragment;
  result <<= next;
  return result;
}


FlowGraphBuilder::FlowGraphBuilder(Procedure* procedure,
                                   const ParsedFunction& parsed_function,
                                   int first_block_id)
  : zone_(Thread::Current()->zone()),
    procedure_(procedure),
    parsed_function_(parsed_function),
    library_(dart::Library::ZoneHandle(Z,
	dart::Class::Handle(parsed_function.function().Owner()).library())),
    ic_data_array_(Z, 0),
    next_block_id_(first_block_id),
    stack_(NULL),
    pending_argument_count_(0) {
}


dart::String& FlowGraphBuilder::DartString(String* string,
                                           Heap::Space space) {
  return dart::String::ZoneHandle(Z,
      dart::String::FromUTF8(string->buffer(), string->size(), space));
}


PushArgumentInstr* FlowGraphBuilder::MakeArgument(){
  ++pending_argument_count_;
  return new(Z) PushArgumentInstr(Pop());
}


Fragment FlowGraphBuilder::MakeTemporary(LocalVariable** variable) {
  Value* initial_value = Pop();
  PushTempInstr* temp = new(Z) PushTempInstr(initial_value);
  Push(temp);

  char name[64];
  intptr_t index = temp->temp_index();
  OS::SNPrint(name, 64, ":temp%" Pd, index);
  *variable =
      new(Z) LocalVariable(TokenPosition::kNoSource,
	                   dart::String::ZoneHandle(Z, Symbols::New(name)),
	                   *initial_value->Type()->ToAbstractType());
  // 2 = expression temporary and current context variables.
  (*variable)->set_index(kFirstLocalSlotFromFp - 2 - variables_.size() -
      pending_argument_count_ -
      index);
  return temp;
}


Fragment FlowGraphBuilder::DropTemporaries(intptr_t count) {
  DropTempsInstr* drop = new(Z) DropTempsInstr(count, Pop());
  while (count-- > 0) {
    ASSERT(stack_->definition()->IsPushTemp());
    Drop();
  }
  Push(drop);
  return drop;
}


void FlowGraphBuilder::AddVariable(VariableDeclaration* declaration,
				   LocalVariable* variable) {
  parsed_function_.node_sequence()->scope()->AddVariable(variable);
  variables_[declaration] = variable;
}


void FlowGraphBuilder::SetTempIndex(Definition* definition) {
  definition->set_temp_index(
      stack_ == NULL ? 0 : stack_->definition()->temp_index() + 1);
}


void FlowGraphBuilder::Push(Definition* definition) {
  SetTempIndex(definition);
  Value::AddToList(new(Z) Value(definition), &stack_);
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

  Fragment body = normal_entry;
  body <<= new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource, 0);
  body += VisitStatement(procedure_->function()->body());

  if (body.is_open()) {
    ASSERT(stack_ == NULL);
    ConstantInstr* null =
	new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    body <<= null;
    body <<= new(Z) ReturnInstr(TokenPosition::kNoSource, new Value(null));
  }

  return new(Z) FlowGraph(parsed_function_, graph_entry, next_block_id_ - 1);
}


Fragment FlowGraphBuilder::EmitConstant(const Object& value) {
  ConstantInstr* constant = new(Z) ConstantInstr(value);
  Push(constant);
  return constant;
}


Fragment FlowGraphBuilder::EmitStaticCall(const Function& target,
    ZoneGrowableArray<PushArgumentInstr*>* arguments) {
  pending_argument_count_ -= arguments->length();
  ASSERT(pending_argument_count_ >= 0);
  StaticCallInstr* call =
      new(Z) StaticCallInstr(TokenPosition::kNoSource,
	                     target,
	                     Object::null_array(),
                	     arguments,
  	                     ic_data_array_);
  Push(call);
  return call;
}


Fragment FlowGraphBuilder::EmitStaticCall(const Function& target) {
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  return EmitStaticCall(target, arguments);
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
    Token::Kind kind,
    ZoneGrowableArray<PushArgumentInstr*>* arguments) {
  pending_argument_count_ -= arguments->length();
  ASSERT(pending_argument_count_ >= 0);
  InstanceCallInstr* call =
      new(Z) InstanceCallInstr(TokenPosition::kNoSource,
	  name,
	  kind,
	  arguments,
	  Object::null_array(),
	  1,
	  ic_data_array_);
  Push(call);
  return call;
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
    Token::Kind kind,
    PushArgumentInstr* argument) {
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 1);
  arguments->Add(argument);
  return EmitInstanceCall(name, kind, arguments);
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
    Token::Kind kind,
    PushArgumentInstr* argument0,
    PushArgumentInstr* argument1) {
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 1);
  arguments->Add(argument0);
  arguments->Add(argument1);
  return EmitInstanceCall(name, kind, arguments);
}


void FlowGraphBuilder::VisitNullLiteral(NullLiteral* node) {
  fragment_ = EmitConstant(Instance::ZoneHandle(Z, Instance::null()));
}


void FlowGraphBuilder::VisitBoolLiteral(BoolLiteral* node) {
  fragment_ = EmitConstant(Bool::Get(node->value()));
}


void FlowGraphBuilder::VisitIntLiteral(IntLiteral* node) {
  fragment_ = EmitConstant(
      Integer::ZoneHandle(Z, Integer::New(node->value(), Heap::kOld)));
}


void FlowGraphBuilder::VisitBigintLiteral(BigintLiteral* node) {
  const dart::String& value = DartString(node->value());
  fragment_ = EmitConstant(
      Integer::ZoneHandle(Z, Integer::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitDoubleLiteral(DoubleLiteral* node) {
  const dart::String& value = DartString(node->value());
  fragment_ = EmitConstant(
      Double::ZoneHandle(Z, Double::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitStringLiteral(StringLiteral* node) {
  fragment_ = EmitConstant(DartString(node->value(), Heap::kOld));
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
  const dart::String& target_name = DartString(target->name()->string());
  if (target->IsField()) {
    const dart::Field& field = dart::Field::ZoneHandle(Z,
	library_.LookupFieldAllowPrivate(target_name));
    const dart::Class& owner = dart::Class::Handle(field.Owner());
    const dart::String& getter_name =
	dart::String::Handle(dart::Field::GetterSymbol(target_name));
    const Function& getter =
	Function::ZoneHandle(Z, owner.LookupStaticFunction(getter_name));
    if (getter.IsNull() || field.is_const()) {
      ConstantInstr* constant = new(Z) ConstantInstr(field);
      SetTempIndex(constant);
      Fragment instructions = constant;
      LoadStaticFieldInstr* load =
	  new(Z) LoadStaticFieldInstr(new Value(constant),
	                              TokenPosition::kNoSource);
      Push(load);
      fragment_ = instructions << load;
    } else {
      fragment_ = EmitStaticCall(getter);
    }
  } else {
    ASSERT(target->IsProcedure());
    UNREACHABLE();
  }
}


void FlowGraphBuilder::VisitStaticSet(StaticSet* node) {
  Member* target = node->target();
  const dart::String& target_name = DartString(target->name()->string());
  if (target->IsField()) {
    const dart::Field& field = dart::Field::ZoneHandle(Z,
	library_.LookupFieldAllowPrivate(target_name));

    Fragment instructions = VisitExpression(node->expression());
    LocalVariable* variable;
    instructions += MakeTemporary(&variable);

    LoadLocalInstr* load =
	new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
    instructions <<= load;
    Push(load);

    StoreStaticFieldInstr* store =
	new(Z) StoreStaticFieldInstr(field, Pop(), TokenPosition::kNoSource);
    instructions <<= store;

    load = new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
    instructions <<= load;
    Push(load);

    fragment_ = instructions + DropTemporaries(1);
  } else {
    ASSERT(target->IsProcedure());
    UNREACHABLE();
  }
}


void FlowGraphBuilder::VisitPropertyGet(PropertyGet* node) {
  Fragment instructions = VisitExpression(node->receiver());
  PushArgumentInstr* receiver_argument = MakeArgument();
  instructions <<= receiver_argument;

  const dart::String& name = DartString(node->name()->string());
  const dart::String& getter_name = dart::String::ZoneHandle(Z,
      Symbols::New(dart::String::Handle(dart::Field::GetterSymbol(name))));
  fragment_ = instructions + EmitInstanceCall(getter_name, Token::kGET,
                                              receiver_argument);
}


void FlowGraphBuilder::VisitPropertySet(PropertySet* node) {
  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Fragment instructions = null;
  Push(null);
  LocalVariable* variable;
  instructions += MakeTemporary(&variable);

  instructions += VisitExpression(node->receiver());
  PushArgumentInstr* receiver_argument = MakeArgument();
  instructions <<= receiver_argument;
  
  instructions += VisitExpression(node->value());
  Value* value = Pop();
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*variable, value, TokenPosition::kNoSource);
  instructions <<= store;
  Push(store);

  PushArgumentInstr* value_argument = MakeArgument();
  instructions <<= value_argument;

  const dart::String& name = DartString(node->name()->string());
  const dart::String& setter_name = dart::String::ZoneHandle(Z,
      Symbols::New(dart::String::Handle(dart::Field::SetterSymbol(name))));

  instructions += EmitInstanceCall(setter_name, Token::kSET,
                                   receiver_argument, value_argument);
  Drop();

  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  instructions <<= load;
  Push(load);

  fragment_ = instructions + DropTemporaries(1);
}


void FlowGraphBuilder::VisitStaticInvocation(StaticInvocation* node) {
  ZoneGrowableArray<PushArgumentInstr*>* arguments = NULL;
  Fragment instructions = TranslateArguments(node->arguments(), &arguments);
  const dart::String& name = DartString(node->procedure()->name()->string());
  const Function& target = 
      Function::ZoneHandle(Z, library_.LookupFunctionAllowPrivate(name));
  fragment_ = instructions + EmitStaticCall(target, arguments);
}


void FlowGraphBuilder::VisitMethodInvocation(MethodInvocation* node) {
  intptr_t length = node->arguments()->positional().length() + 1;
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, length);
  Fragment instructions = VisitExpression(node->receiver());
  PushArgumentInstr* receiver_argument = MakeArgument();
  instructions <<= receiver_argument;
  arguments->Add(receiver_argument);
  instructions += TranslateArguments(node->arguments(), &arguments);

  const dart::String& name = dart::String::ZoneHandle(Z,
      Symbols::New(DartString(node->name()->string())));
  fragment_ =
      instructions + EmitInstanceCall(name, Token::kILLEGAL, arguments);
}


void FlowGraphBuilder::VisitConstructorInvocation(ConstructorInvocation* node) {
  const dart::String& class_name =
      DartString(Class::Cast(node->target()->parent())->name());
  const dart::Class& owner = dart::Class::ZoneHandle(Z,
      library_.LookupClassAllowPrivate(class_name));
  ZoneGrowableArray<PushArgumentInstr*>* arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  AllocateObjectInstr* alloc =
      new(Z) AllocateObjectInstr(TokenPosition::kNoSource,
	                         owner,
	                         arguments);
  Fragment instructions = alloc;
  Push(alloc);
  LocalVariable* variable;
  instructions += MakeTemporary(&variable);

  intptr_t length = node->arguments()->positional().length() + 1;
  arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, length);
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  instructions <<= load;
  Push(load);
  PushArgumentInstr* object_argument = MakeArgument();
  instructions <<= object_argument;
  arguments->Add(object_argument);
  instructions += TranslateArguments(node->arguments(), &arguments);

  const dart::String& constructor_name =
      DartString(node->target()->name()->string());
  const dart::String& dot = dart::String::Handle(dart::String::New("."));
  const dart::String& name =
      dart::String::Handle(dart::String::Concat(
        dart::String::Handle(dart::String::Concat(class_name, dot)),
	constructor_name));
  const Function& target = Function::ZoneHandle(Z,
      owner.LookupConstructorAllowPrivate(name));
  instructions += EmitStaticCall(target, arguments);
  Drop();

  load = new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  instructions <<= load;
  Push(load);

  fragment_ = instructions + DropTemporaries(1);
}

Fragment FlowGraphBuilder::TranslateArguments(Arguments* node,
    ZoneGrowableArray<PushArgumentInstr*>** arguments) {
  if (node->types().length() != 0 || node->named().length() != 0) {
    UNIMPLEMENTED();
  }
  Fragment instructions;
  List<Expression>& positional = node->positional();
  if (*arguments == NULL) {
    *arguments =
	new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, positional.length());
  }
  for (int i = 0; i < positional.length(); ++i) {
    instructions += VisitExpression(positional[i]);
    PushArgumentInstr* push = MakeArgument();
    (*arguments)->Add(push);
    instructions <<= push;
  }
  return instructions;
}


void FlowGraphBuilder::VisitBlock(Block* node) {
  Fragment instructions;
  List<Statement>& statements = node->statements();
  for (int i = 0; i < statements.length(); ++i) {
    instructions += VisitStatement(statements[i]);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitReturnStatement(ReturnStatement* node) {
  Fragment instructions;
  Value* result;
  if (node->expression() == NULL) {
    ASSERT(stack_ == NULL);  // True for all statements.
    ConstantInstr* null =
        new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    instructions = null;
    result = new Value(null);
  } else {
    instructions = VisitExpression(node->expression());
    result = Pop();
  }
  instructions <<= new(Z) ReturnInstr(TokenPosition::kNoSource, result);
  fragment_ = instructions.closed();
}


void FlowGraphBuilder::VisitExpressionStatement(ExpressionStatement* node) {
  Fragment instructions = VisitExpression(node->expression());
  Drop();
  fragment_ = instructions;  // Unnecessary.
}


void FlowGraphBuilder::VisitVariableDeclaration(VariableDeclaration* node) {
  const dart::String& name = DartString(node->name());
  const dart::String& symbol = dart::String::ZoneHandle(Z, Symbols::New(name));
  LocalVariable* local =
    new LocalVariable(TokenPosition::kNoSource, symbol,
		      Type::ZoneHandle(Z, Type::DynamicType()));

  AddVariable(node, local);

  Fragment instructions;
  Value* value;
  if (node->initializer() == NULL) {
    ASSERT(stack_ == NULL);
    ConstantInstr* null =
	new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
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
