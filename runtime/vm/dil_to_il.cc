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
#define H (translation_helper_)

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


const dart::String& TranslationHelper::DartString(const char* content) {
  return dart::String::ZoneHandle(Z, dart::String::New(content));
}


dart::String& TranslationHelper::DartString(String* content,
                                            Heap::Space space) {
  return dart::String::ZoneHandle(Z,
      dart::String::FromUTF8(content->buffer(), content->size(), space));
}


const dart::String& TranslationHelper::DartSymbol(const char* content) {
  return dart::String::ZoneHandle(Z, Symbols::New(content));
}


const dart::String& TranslationHelper::DartSymbol(String* content) {
  return dart::String::ZoneHandle(Z,
      dart::Symbols::FromUTF8(content->buffer(), content->size()));
}


const dart::String& TranslationHelper::DartConstructorName(Constructor* node) {
  Class* klass = Class::Cast(node->parent());

  // We build a String which looks like <classname>.<constructor-name>.
  dart::String& temp = DartString(klass->name());
  temp ^= dart::String::Concat(temp, Symbols::Dot());
  temp ^= dart::String::Concat(
      temp, DartString(node->name()->string()));  // NOLINT
  return dart::String::ZoneHandle(Z, dart::Symbols::New(temp));
}


const dart::String& TranslationHelper::DartProcedureName(Procedure* procedure) {
  if (procedure->kind() == Procedure::kSetter) {
    return DartSetterName(procedure->name()->string());
  } else if (procedure->kind() == Procedure::kGetter) {
    return DartGetterName(procedure->name()->string());
  } else {
    return DartSymbol(procedure->name()->string());
  }
}


const dart::String& TranslationHelper::DartSetterName(String* content) {
  return dart::String::ZoneHandle(Z,
      dart::Field::SetterSymbol(DartString(content)));
}


const dart::String& TranslationHelper::DartGetterName(String* content) {
  return dart::String::ZoneHandle(Z,
      dart::Field::GetterSymbol(DartString(content)));
}


FlowGraphBuilder::FlowGraphBuilder(TreeNode* node,
                                   ParsedFunction* parsed_function,
                                   int first_block_id)
  : zone_(Thread::Current()->zone()),
    translation_helper_(zone_),
    node_(node),
    parsed_function_(parsed_function),
    library_(dart::Library::ZoneHandle(Z,
        dart::Class::Handle(parsed_function->function().Owner()).library())),
    ic_data_array_(Z, 0),
    next_block_id_(first_block_id),
    scope_(NULL),
    loop_depth_(0),
    stack_(NULL),
    pending_argument_count_(0) {
}


dart::RawLibrary* FlowGraphBuilder::LookupLibraryByDilLibrary(
    Library* dil_library) {
  const dart::String& library_name = H.DartSymbol(dil_library->import_uri());
  ASSERT(!library_name.IsNull());
  dart::RawLibrary* library = dart::Library::LookupLibrary(library_name);
  ASSERT(library != Object::null());
  return library;
}


dart::RawClass* FlowGraphBuilder::LookupClassByName(const dart::String& name) {
  dart::RawClass* klass = library_.LookupClassAllowPrivate(name);
  ASSERT(klass != Object::null());
  return klass;
}


dart::RawClass* FlowGraphBuilder::LookupClassByName(String* name) {
  dart::RawClass* klass = LookupClassByName(H.DartString(name));
  ASSERT(klass != Object::null());
  return klass;
}


dart::RawClass* FlowGraphBuilder::LookupClassByDilClass(Class* dil_klass) {
  dart::RawClass* klass = NULL;

  const dart::String& class_name = H.DartString(dil_klass->name());
  if (dil_klass->parent()->IsCorelibrary()) {
    Library* dil_library = Library::Cast(dil_klass->parent());
    dart::Library& library = dart::Library::Handle(
        LookupLibraryByDilLibrary(dil_library));
    klass = library.LookupClassAllowPrivate(class_name);
  } else {
    klass = LookupClassByName(class_name);
  }

  ASSERT(klass != Object::null());
  return klass;
}


dart::RawField* FlowGraphBuilder::LookupFieldByName(const dart::String& name) {
  dart::RawField* field = library_.LookupFieldAllowPrivate(name);
  ASSERT(field != Object::null());
  return field;
}


dart::RawField* FlowGraphBuilder::LookupFieldByName(String* name) {
  dart::RawField* field = LookupFieldByName(H.DartString(name));
  ASSERT(field != Object::null());
  return field;
}


dart::RawFunction* FlowGraphBuilder::LookupStaticMethodByDilProcedure(
    Procedure* procedure) {
  Library* dil_library = Library::Cast(procedure->parent());
  if (dil_library->IsCorelibrary()) {
    dart::Library& library = dart::Library::Handle(
        LookupLibraryByDilLibrary(dil_library));
    const dart::String& procedure_name =
        H.DartSymbol(procedure->name()->string());
    ASSERT(!procedure_name.IsNull());
    dart::RawFunction* function =
        library.LookupFunctionAllowPrivate(procedure_name);
    ASSERT(function != Object::null());
    return function;
  } else {
    dart::RawFunction* function = library_.LookupFunctionAllowPrivate(
        H.DartString(procedure->name()->string()));
    ASSERT(function != Object::null());
    return function;
  }
}


dart::RawFunction* FlowGraphBuilder::LookupStaticMethodByName(
    const dart::String& name) {
  dart::RawFunction* function = library_.LookupFunctionAllowPrivate(name);
  ASSERT(function != Object::null());
  return function;
}


dart::RawFunction* FlowGraphBuilder::LookupStaticMethodByName(String* name) {
  dart::RawFunction* function = LookupStaticMethodByName(
      H.DartString(name));
  ASSERT(function != Object::null());
  return function;
}


dart::RawFunction* FlowGraphBuilder::LookupConstructorByDilConstructor(
    const dart::Class& owner, Constructor* constructor) {
  dart::RawFunction* function =
      owner.LookupConstructorAllowPrivate(H.DartConstructorName(constructor));
  ASSERT(function != Object::null());
  return function;
}


PushArgumentInstr* FlowGraphBuilder::MakeArgument() {
  ++pending_argument_count_;
  return new(Z) PushArgumentInstr(Pop());
}


Fragment FlowGraphBuilder::AddArgumentToList(ArgumentArray arguments) {
  PushArgumentInstr* argument = MakeArgument();
  arguments->Add(argument);
  return Fragment(argument);
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
                           H.DartSymbol(name),
                           *initial_value->Type()->ToAbstractType());
  // Set the index relative to the base of the expression stack.  Later this
  // will be adjusted to be relative to the frame pointer.
  (*variable)->set_index(-(index + pending_argument_count_));
  temporaries_.push_back(*variable);
  return Fragment(temp);
}


Fragment FlowGraphBuilder::DropTemporaries(intptr_t count) {
  DropTempsInstr* drop = new(Z) DropTempsInstr(count, Pop());
  while (count-- > 0) {
    ASSERT(stack_->definition()->IsPushTemp());
    Drop();
  }
  Push(drop);
  return Fragment(drop);
}


void FlowGraphBuilder::AddVariable(VariableDeclaration* declaration,
                                   LocalVariable* variable) {
  ASSERT(variable != NULL);
  scope_->AddVariable(variable);
  locals_[declaration] = variable;
}


void FlowGraphBuilder::AddParameter(VariableDeclaration* declaration,
                                    LocalVariable* variable,
                                    int pos) {
  ASSERT(variable != NULL);
  scope_->InsertParameterAt(pos, variable);
  locals_[declaration] = variable;
}


dart::LocalVariable* FlowGraphBuilder::LookupVariable(
    VariableDeclaration* var) {
  LocalVariable* local = locals_[var];
  ASSERT(local != NULL);
  return local;
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
  const dart::Function& function = parsed_function_->function();
  switch (function.kind()) {
    case RawFunction::kClosureFunction:
    case RawFunction::kRegularFunction:
    case RawFunction::kGetterFunction:
    case RawFunction::kSetterFunction:
    case RawFunction::kConstructor: {
      if (function.IsImplicitClosureFunction()) return NULL;
      if (function.IsConstructorClosureFunction()) return NULL;

      // The IR builder will create its own local variables and scopes, and it
      // will not need an AST.  The code generator will assume that there is a
      // local variable stack slot allocated for the current context and (I
      // think) that the runtime will expect it to be at a fixed offset which
      // requires allocating an unused expression temporary variable.
      scope_ = new LocalScope(NULL, 0, 0);
      scope_->AddVariable(parsed_function_->EnsureExpressionTemp());
      scope_->AddVariable(parsed_function_->current_context_var());
      parsed_function_->SetNodeSequence(
          new SequenceNode(TokenPosition::kNoSource, scope_));

      if (node_->IsProcedure()) {
        return BuildGraphOfFunction(dil::Procedure::Cast(node_)->function());
      } else if (node_->IsConstructor()) {
        return BuildGraphOfFunction(dil::Constructor::Cast(node_)->function());
      } else {
        UNIMPLEMENTED();
      }
    }
    default:
      return NULL;
  }
}


FlowGraph* FlowGraphBuilder::BuildGraphOfFunction(FunctionNode* function) {
  TargetEntryInstr* normal_entry =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  GraphEntryInstr* graph_entry =
      new(Z) GraphEntryInstr(*parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

  // Populate stack with arguments
  {
    bool is_method = !parsed_function_->function().IsStaticFunction();

    dart::AbstractType& dynamic =
        dart::AbstractType::ZoneHandle(Type::DynamicType());
    int pos = 0;
    if (is_method) {
      dart::Class& klass =
          dart::Class::Handle(parsed_function_->function().Owner());
      Type& klass_type =
          Type::ZoneHandle(Type::NewNonParameterizedType(klass));
      LocalVariable* parameter = new(Z) LocalVariable(
          TokenPosition::kNoSource,
          H.DartSymbol("this"),
          klass_type);
      // FIXME: We should probably put it into an field for generating
      // expressions relying on this.
      AddParameter(NULL, parameter, pos);
      pos++;
    }
    for (int i = 0;
         i < function->positional_parameters().length();
         i++, pos++) {
      VariableDeclaration* var = function->positional_parameters()[i];
      LocalVariable* parameter = new(Z) LocalVariable(
          TokenPosition::kNoSource,
          H.DartSymbol(var->name()),
          dynamic);
      AddParameter(var, parameter, pos);
    }
    for (int i = 0; i < function->named_parameters().length(); i++, pos++) {
      VariableDeclaration* var = function->named_parameters()[i];
      LocalVariable* parameter = new(Z) LocalVariable(
          TokenPosition::kNoSource,
          H.DartSymbol(var->name()),
          dynamic);
      AddParameter(var, parameter, pos);
    }
  }

  // Setup default values for optional parameters.
  int num_optional_parameters =
      parsed_function_->function().NumOptionalParameters();
  if (num_optional_parameters > 0) {
    ZoneGrowableArray<const Instance*>* default_values =
        new ZoneGrowableArray<const Instance*>(Z, num_optional_parameters);

    if (parsed_function_->function().HasOptionalNamedParameters()) {
      ASSERT(!parsed_function_->function().HasOptionalPositionalParameters());
      for (int i = 0; i < num_optional_parameters; i++) {
        // FIXME(kustermann):
        // We should evaluate the constant expression:
        //
        //     VariableDeclaration* variable =
        //         procedure_->function()->named_parameters()[i];
        //     default_value =
        //         EvaluateConstantExpression(variable->initializer());
        const Instance* default_value = &Instance::ZoneHandle(Instance::null());
        default_values->Add(default_value);
      }
    } else {
      ASSERT(parsed_function_->function().HasOptionalPositionalParameters());
      // int required = procedure_->function()->required_parameter_count();
      for (int i = 0; i < num_optional_parameters; i++) {
        // FIXME(kustermann):
        // We should evaluate the constant expression:
        //
        //     VariableDeclaration* variable =
        //         procedure_->function()->positional_parameters()[i];
        //     default_value =
        //         EvaluateConstantExpression(variable->initializer());
        const Instance* default_value = &Instance::ZoneHandle(Instance::null());
        default_values->Add(default_value);
      }
    }
    parsed_function_->set_default_parameter_values(default_values);
  }

  Fragment body(normal_entry);
  body <<= new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource, 0);
  body += VisitStatement(function->body());

  if (body.is_open()) {
    ASSERT(stack_ == NULL);
    ConstantInstr* null =
        new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    body <<= null;
    body <<= new(Z) ReturnInstr(TokenPosition::kNoSource, new(Z) Value(null));
  }

  return new(Z) FlowGraph(*parsed_function_, graph_entry, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfFieldAccessor(Field* field) {
  UNIMPLEMENTED();
  return NULL;
}


void FlowGraphBuilder::AdjustTemporaries(int base) {
  for (std::vector<LocalVariable*>::iterator it = temporaries_.begin();
       it != temporaries_.end();
       ++it) {
    (*it)->AdjustIndex(base);
  }
}


Fragment FlowGraphBuilder::EmitConstant(const Object& value) {
  ConstantInstr* constant = new(Z) ConstantInstr(value);
  Push(constant);
  return Fragment(constant);
}


Fragment FlowGraphBuilder::EmitStaticCall(const Function& target,
                                          ArgumentArray arguments,
                                          const Array& argument_names) {
  pending_argument_count_ -= arguments->length();
  ASSERT(pending_argument_count_ >= 0);
  StaticCallInstr* call =
      new(Z) StaticCallInstr(TokenPosition::kNoSource,
                             target,
                             argument_names,
                             arguments,
                             ic_data_array_);
  Push(call);
  return Fragment(call);
}


Fragment FlowGraphBuilder::EmitStaticCall(const Function& target) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  return EmitStaticCall(target, arguments, Object::null_array());
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
                                            Token::Kind kind,
                                            ArgumentArray arguments,
                                            const Array& argument_names) {
  pending_argument_count_ -= arguments->length();
  ASSERT(pending_argument_count_ >= 0);
  InstanceCallInstr* call =
      new(Z) InstanceCallInstr(TokenPosition::kNoSource,
          name,
          kind,
          arguments,
          argument_names,
          1,
          ic_data_array_);
  Push(call);
  return Fragment(call);
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
                                            Token::Kind kind,
                                            PushArgumentInstr* argument) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 1);
  arguments->Add(argument);
  return EmitInstanceCall(name, kind, arguments, Array::null_array());
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
    Token::Kind kind,
    PushArgumentInstr* argument0,
    PushArgumentInstr* argument1) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 2);
  arguments->Add(argument0);
  arguments->Add(argument1);
  return EmitInstanceCall(name, kind, arguments, Array::null_array());
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
                                            Token::Kind kind,
                                            PushArgumentInstr* argument0,
                                            PushArgumentInstr* argument1,
                                            PushArgumentInstr* argument2) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 3);
  arguments->Add(argument0);
  arguments->Add(argument1);
  arguments->Add(argument2);
  return EmitInstanceCall(name, kind, arguments, Array::null_array());
}


Fragment FlowGraphBuilder::EmitInstanceCall(const dart::String& name,
                                            Token::Kind kind,
                                            PushArgumentInstr* argument0,
                                            PushArgumentInstr* argument1,
                                            PushArgumentInstr* argument2,
                                            PushArgumentInstr* argument3) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 4);
  arguments->Add(argument0);
  arguments->Add(argument1);
  arguments->Add(argument2);
  arguments->Add(argument3);
  return EmitInstanceCall(name, kind, arguments, Array::null_array());
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
  const dart::String& value = H.DartString(node->value());
  fragment_ = EmitConstant(
      Integer::ZoneHandle(Z, Integer::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitDoubleLiteral(DoubleLiteral* node) {
  const dart::String& value = H.DartString(node->value());
  fragment_ = EmitConstant(
      Double::ZoneHandle(Z, Double::New(value, Heap::kOld)));
}


void FlowGraphBuilder::VisitStringLiteral(StringLiteral* node) {
  fragment_ = EmitConstant(H.DartString(node->value(), Heap::kOld));
}


class DartTypeTranslator : public DartTypeVisitor {
 public:
    explicit DartTypeTranslator(FlowGraphBuilder* owner)
      : owner_(owner),
        result_(AbstractType::ZoneHandle(owner->zone_)) {
  }

  AbstractType& result() { return result_; }

  void VisitDefaultDartType(DartType* node) { UNREACHABLE(); }

  void VisitInterfaceType(InterfaceType* node);

 private:
  FlowGraphBuilder* owner_;
  AbstractType& result_;
};


void DartTypeTranslator::VisitInterfaceType(InterfaceType* node) {
  if (node->type_arguments().length() != 0) UNIMPLEMENTED();

  const dart::Class& klass =
      dart::Class::Handle(owner_->LookupClassByName(node->klass()->name()));
  result_ ^= klass.DeclarationType();
}


void FlowGraphBuilder::VisitTypeLiteral(TypeLiteral* node) {
  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  fragment_ = EmitConstant(translator.result());
}


void FlowGraphBuilder::VisitVariableGet(VariableGet* node) {
  LocalVariable* local = LookupVariable(node->variable());
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*local, TokenPosition::kNoSource);
  Push(load);
  fragment_ = Fragment(load);
}


void FlowGraphBuilder::VisitVariableSet(VariableSet* node) {
  LocalVariable* local = LookupVariable(node->variable());
  Fragment instructions = VisitExpression(node->expression());
  Value* value = Pop();
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*local, value, TokenPosition::kNoSource);
  Push(store);
  fragment_ = instructions << store;
}


void FlowGraphBuilder::VisitStaticGet(StaticGet* node) {
  Member* target = node->target();
  if (target->IsField()) {
    const dart::String& field_name = H.DartString(target->name()->string());
    const dart::Field& field =
        dart::Field::ZoneHandle(Z, LookupFieldByName(field_name));
    const dart::Class& owner = dart::Class::Handle(field.Owner());
    const dart::String& getter_name =
        dart::String::Handle(dart::Field::GetterName(field_name));
    const Function& getter =
        Function::ZoneHandle(Z, owner.LookupStaticFunction(getter_name));
    if (getter.IsNull() || field.is_const()) {
      ConstantInstr* constant = new(Z) ConstantInstr(field);
      SetTempIndex(constant);
      Fragment instructions(constant);
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
  if (target->IsField()) {
    const dart::Field& field =
        dart::Field::ZoneHandle(Z, LookupFieldByName(target->name()->string()));

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

    fragment_ = instructions + DropTemporaries(0);
  } else {
    ASSERT(target->IsProcedure());
    UNREACHABLE();
  }
}


void FlowGraphBuilder::VisitPropertyGet(PropertyGet* node) {
  Fragment instructions = VisitExpression(node->receiver());
  PushArgumentInstr* receiver_argument = MakeArgument();
  instructions <<= receiver_argument;

  const dart::String& getter_name = H.DartGetterName(node->name()->string());
  fragment_ = instructions + EmitInstanceCall(getter_name, Token::kGET,
                                              receiver_argument);
}


void FlowGraphBuilder::VisitPropertySet(PropertySet* node) {
  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Fragment instructions(null);
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

  const dart::String& setter_name = H.DartSetterName(node->name()->string());
  instructions += EmitInstanceCall(setter_name, Token::kSET,
                                   receiver_argument, value_argument);
  Drop();

  fragment_ = instructions + DropTemporaries(0);
}


void FlowGraphBuilder::VisitStaticInvocation(StaticInvocation* node) {
  ArgumentArray arguments = NULL;
  Array& argument_names = Array::ZoneHandle(Z);
  Fragment instructions = TranslateArguments(
      node->arguments(), &arguments, &argument_names);
  const Function& target = Function::ZoneHandle(Z,
      LookupStaticMethodByDilProcedure(node->procedure()));
  fragment_ = instructions + EmitStaticCall(target, arguments, argument_names);
}


void FlowGraphBuilder::VisitMethodInvocation(MethodInvocation* node) {
  intptr_t length = node->arguments()->positional().length() + 1;
  ArgumentArray arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, length);
  Array& argument_names = Array::ZoneHandle(Z);
  Fragment instructions = VisitExpression(node->receiver());
  instructions += AddArgumentToList(arguments);
  instructions += TranslateArguments(
      node->arguments(), &arguments, &argument_names);

  const dart::String& name = H.DartSymbol(node->name()->string());  // NOLINT
  fragment_ = instructions +
      EmitInstanceCall(name, Token::kILLEGAL, arguments, argument_names);
}


void FlowGraphBuilder::VisitConstructorInvocation(ConstructorInvocation* node) {
  const dart::Class& owner = dart::Class::ZoneHandle(
      Z, LookupClassByDilClass(Class::Cast(node->target()->parent())));
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  AllocateObjectInstr* alloc =
      new(Z) AllocateObjectInstr(TokenPosition::kNoSource,
                                 owner,
                                 arguments);
  Fragment instructions(alloc);
  Push(alloc);
  LocalVariable* variable;
  instructions += MakeTemporary(&variable);

  intptr_t length = node->arguments()->positional().length() + 1;
  arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, length);
  Array& argument_names = Array::ZoneHandle(Z);
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  instructions <<= load;
  Push(load);
  instructions += AddArgumentToList(arguments);
  instructions += TranslateArguments(
      node->arguments(), &arguments, &argument_names);

  const Function& target = Function::ZoneHandle(Z,
      LookupConstructorByDilConstructor(owner, node->target()));
  instructions += EmitStaticCall(target, arguments, argument_names);
  Drop();

  fragment_ = instructions + DropTemporaries(0);
}


void FlowGraphBuilder::VisitIsExpression(IsExpression* node) {
  Fragment instructions = VisitExpression(node->operand());
  PushArgumentInstr* operand_argument = MakeArgument();
  instructions <<= operand_argument;

  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Push(null);
  instructions <<= null;
  PushArgumentInstr* type_arguments_argument = MakeArgument();
  instructions <<= type_arguments_argument;

  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  ConstantInstr* type = new(Z) ConstantInstr(translator.result());
  Push(type);
  instructions <<= type;
  PushArgumentInstr* type_argument = MakeArgument();
  instructions <<= type_argument;

  ConstantInstr* negate = new(Z) ConstantInstr(Bool::False());
  instructions <<= negate;
  Push(negate);
  PushArgumentInstr* negate_argument = MakeArgument();
  instructions <<= negate_argument;

  fragment_ = instructions + EmitInstanceCall(
      dart::Library::PrivateCoreLibName(Symbols::_instanceOf()),
      Token::kIS,
      operand_argument,
      type_arguments_argument,
      type_argument,
      negate_argument);
}


void FlowGraphBuilder::VisitAsExpression(AsExpression* node) {
  Fragment instructions = VisitExpression(node->operand());
  PushArgumentInstr* operand_argument = MakeArgument();
  instructions <<= operand_argument;

  ConstantInstr* null =
      new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
  Push(null);
  instructions <<= null;
  PushArgumentInstr* type_arguments_argument = MakeArgument();
  instructions <<= type_arguments_argument;

  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  ConstantInstr* type = new(Z) ConstantInstr(translator.result());
  Push(type);
  instructions <<= type;
  PushArgumentInstr* type_argument = MakeArgument();
  instructions <<= type_argument;

  fragment_ = instructions + EmitInstanceCall(
      dart::Library::PrivateCoreLibName(Symbols::_as()),
      Token::kAS,
      operand_argument,
      type_arguments_argument,
      type_argument);
}


void FlowGraphBuilder::VisitConditionalExpression(ConditionalExpression* node) {
  Fragment instructions = VisitExpression(node->condition());
  ConstantInstr* true_constant = new(Z) ConstantInstr(Bool::True());
  Push(true_constant);
  instructions <<= true_constant;

  Value* right_value = Pop();
  Value* left_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                left_value,
                                right_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  instructions <<= branch;

  Value* top = stack_;
  TargetEntryInstr* then_entry = *branch->true_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment then_fragment(then_entry);
  then_fragment += VisitExpression(node->then());
  Value* result = Pop();
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*parsed_function_->expression_temp_var(),
                             result,
                             TokenPosition::kNoSource);
  then_fragment <<= store;

  ASSERT(top == stack_);
  TargetEntryInstr* otherwise_entry = *branch->false_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment otherwise_fragment(otherwise_entry);
  otherwise_fragment += VisitExpression(node->otherwise());
  result = Pop();
  store =
      new(Z) StoreLocalInstr(*parsed_function_->expression_temp_var(),
                             result,
                             TokenPosition::kNoSource);
  otherwise_fragment <<= store;

  JoinEntryInstr* join =
      new(Z) JoinEntryInstr(AllocateBlockId(),
                            CatchClauseNode::kInvalidTryIndex);
  then_fragment <<= new(Z) GotoInstr(join);
  otherwise_fragment <<= new(Z) GotoInstr(join);

  instructions = Fragment(instructions.entry, join);
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*parsed_function_->expression_temp_var(),
                            TokenPosition::kNoSource);
  Push(load);
  fragment_ = instructions << load;
}


void FlowGraphBuilder::VisitLogicalExpression(LogicalExpression* node) {
  if (node->op() == LogicalExpression::kAnd ||
      node->op() == LogicalExpression::kOr) {
    Fragment instructions = VisitExpression(node->left());
    ConstantInstr* true_constant = new(Z) ConstantInstr(Bool::True());
    Push(true_constant);
    instructions <<= true_constant;

    Value* right_value = Pop();
    Value* left_value = Pop();
    StrictCompareInstr* compare =
        new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                  Token::kEQ_STRICT,
                                  left_value,
                                  right_value,
                                  false);
    BranchInstr* branch = new(Z) BranchInstr(compare);
    instructions <<= branch;

    Value* top = stack_;
    TargetEntryInstr* right_entry =
        new(Z) TargetEntryInstr(AllocateBlockId(),
                                CatchClauseNode::kInvalidTryIndex);
    Fragment right_fragment(right_entry);
    right_fragment += VisitExpression(node->right());
    true_constant = new(Z) ConstantInstr(Bool::True());
    Push(true_constant);
    right_fragment <<= true_constant;

    right_value = Pop();
    left_value = Pop();
    compare =
        new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                  Token::kEQ_STRICT,
                                  left_value,
                                  right_value,
                                  false);
    Push(compare);
    right_fragment <<= compare;
    Value* result = Pop();
    StoreLocalInstr* store =
        new(Z) StoreLocalInstr(*parsed_function_->expression_temp_var(),
                               result,
                               TokenPosition::kNoSource);
    right_fragment <<= store;

    ASSERT(top == stack_);
    TargetEntryInstr* constant_entry =
        new(Z) TargetEntryInstr(AllocateBlockId(),
                                CatchClauseNode::kInvalidTryIndex);
    Fragment constant_fragment(constant_entry);
    ConstantInstr* constant =
        new(Z) ConstantInstr(Bool::Get(node->op() == LogicalExpression::kOr));
    Push(constant);
    constant_fragment <<= constant;
    result = Pop();
    store =
        new(Z) StoreLocalInstr(*parsed_function_->expression_temp_var(),
                               result,
                               TokenPosition::kNoSource);
    constant_fragment <<= store;

    if (node->op() == LogicalExpression::kAnd) {
      *branch->true_successor_address() = right_entry;
      *branch->false_successor_address() = constant_entry;
    } else {
      *branch->true_successor_address() = constant_entry;
      *branch->false_successor_address() = right_entry;
    }
    JoinEntryInstr* join =
        new(Z) JoinEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
    right_fragment <<= new(Z) GotoInstr(join);
    constant_fragment <<= new(Z) GotoInstr(join);

    instructions = Fragment(instructions.entry, join);
    LoadLocalInstr* load =
        new(Z) LoadLocalInstr(*parsed_function_->expression_temp_var(),
                              TokenPosition::kNoSource);
    Push(load);
    fragment_ = instructions << load;
  } else {
    UNIMPLEMENTED();
  }
}


void FlowGraphBuilder::VisitNot(Not* node) {
  Fragment instructions = VisitExpression(node->expression());
  BooleanNegateInstr* negate = new(Z) BooleanNegateInstr(Pop());
  Push(negate);
  fragment_ = instructions << negate;
}


Fragment FlowGraphBuilder::TranslateArguments(Arguments* node,
                                              ArgumentArray* arguments,
                                              Array* argument_names) {
  if (node->types().length() != 0) {
    UNIMPLEMENTED();
  }
  Fragment instructions;

  List<Expression>& positional = node->positional();
  List<NamedExpression>& named = node->named();
  if (*arguments == NULL) {
    int argument_count = positional.length() + named.length();
    *arguments =
        new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, argument_count);
  }
  if (named.length() == 0) {
    *argument_names ^= Array::null();
  } else {
    *argument_names ^= Array::New(named.length());
  }

  for (int i = 0; i < positional.length(); ++i) {
    instructions += VisitExpression(positional[i]);
    instructions += AddArgumentToList(*arguments);
  }
  for (int i = 0; i < named.length(); ++i) {
    NamedExpression* named_expression = named[i];
    instructions += VisitExpression(named_expression->expression());
    instructions += AddArgumentToList(*arguments);
    argument_names->SetAt(i, H.DartSymbol(named_expression->name()));
  }
  return instructions;
}


void FlowGraphBuilder::VisitEmptyStatement(EmptyStatement* node) {
  fragment_ = Fragment();
}


void FlowGraphBuilder::VisitBlock(Block* node) {
  scope_ = new LocalScope(scope_, 0, loop_depth_);
  Fragment instructions;
  List<Statement>& statements = node->statements();
  for (int i = 0; i < statements.length(); ++i) {
    instructions += VisitStatement(statements[i]);
  }
  fragment_ = instructions;
  scope_ = scope_->parent();
}


void FlowGraphBuilder::VisitReturnStatement(ReturnStatement* node) {
  Fragment instructions;
  Value* result;
  if (node->expression() == NULL) {
    ASSERT(stack_ == NULL);  // True for all statements.
    ConstantInstr* null =
        new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    instructions = Fragment(null);
    result = new(Z) Value(null);
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
  const dart::String& symbol = H.DartSymbol(node->name());
  LocalVariable* local =
    new(Z) LocalVariable(TokenPosition::kNoSource, symbol,
                         Type::ZoneHandle(Z, Type::DynamicType()));

  AddVariable(node, local);

  Fragment instructions;
  Value* value;
  if (node->initializer() == NULL) {
    ASSERT(stack_ == NULL);
    ConstantInstr* null =
        new(Z) ConstantInstr(Instance::ZoneHandle(Z, Instance::null()));
    null->set_temp_index(0);
    instructions = Fragment(null);
    value = new Value(null);
  } else {
    instructions = VisitExpression(node->initializer());
    value = Pop();
  }
  fragment_ = instructions <<
      new(Z) StoreLocalInstr(*local, value, TokenPosition::kNoSource);
}


void FlowGraphBuilder::VisitIfStatement(IfStatement* node) {
  Fragment instructions = VisitExpression(node->condition());
  ConstantInstr* true_constant = new(Z) ConstantInstr(Bool::True());
  Push(true_constant);
  instructions <<= true_constant;

  Value* right_value = Pop();
  Value* left_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                left_value,
                                right_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  instructions <<= branch;

  TargetEntryInstr* then_entry = *branch->true_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment then_fragment(then_entry);
  then_fragment += VisitStatement(node->then());

  TargetEntryInstr* otherwise_entry = *branch->false_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment otherwise_fragment(otherwise_entry);
  otherwise_fragment += VisitStatement(node->otherwise());

  if (then_fragment.is_open()) {
    if (otherwise_fragment.is_open()) {
      JoinEntryInstr* join =
          new(Z) JoinEntryInstr(AllocateBlockId(),
                                CatchClauseNode::kInvalidTryIndex);
      then_fragment <<= new(Z) GotoInstr(join);
      otherwise_fragment <<= new(Z) GotoInstr(join);
      fragment_ = Fragment(instructions.entry, join);
    } else {
      fragment_ = Fragment(instructions.entry, then_fragment.current);
    }
  } else if (otherwise_fragment.is_open()) {
    fragment_ = Fragment(instructions.entry, otherwise_fragment.current);
  } else {
    fragment_ = instructions.closed();
  }
}


void FlowGraphBuilder::VisitWhileStatement(WhileStatement* node) {
  ++loop_depth_;
  Fragment condition = VisitExpression(node->condition());
  ConstantInstr* true_constant = new(Z) ConstantInstr(Bool::True());
  Push(true_constant);
  condition <<= true_constant;

  Value* right_value = Pop();
  Value* left_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                left_value,
                                right_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  condition <<= branch;

  TargetEntryInstr* body_entry = *branch->true_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment body(body_entry);
  body += VisitStatement(node->body());

  Instruction* entry;
  if (body.is_open()) {
    JoinEntryInstr* join =
        new(Z) JoinEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
    body <<= new(Z) GotoInstr(join);

    Fragment loop(join);
    loop <<= new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource,
                                            loop_depth_);
    loop += condition;
    entry = new(Z) GotoInstr(join);
  } else {
    entry = condition.entry;
  }

  TargetEntryInstr* loop_exit = *branch->false_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);

  fragment_ = Fragment(entry, loop_exit);
  --loop_depth_;
}


void FlowGraphBuilder::VisitDoStatement(DoStatement* node) {
  ++loop_depth_;
  Fragment body = VisitStatement(node->body());

  if (body.is_closed()) {
    fragment_ = body;
    --loop_depth_;
    return;
  }

  JoinEntryInstr* join =
      new(Z) JoinEntryInstr(AllocateBlockId(),
                            CatchClauseNode::kInvalidTryIndex);
  Fragment loop(join);
  loop <<= new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource,
                                          loop_depth_);
  loop += body;
  loop += VisitExpression(node->condition());
  ConstantInstr* true_constant = new(Z) ConstantInstr(Bool::True());
  Push(true_constant);
  loop <<= true_constant;

  Value* right_value = Pop();
  Value* left_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                left_value,
                                right_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  loop <<= branch;

  TargetEntryInstr* loop_repeat = *branch->true_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);
  Fragment repeat(loop_repeat);
  repeat <<= new(Z) GotoInstr(join);

  TargetEntryInstr* loop_exit = *branch->false_successor_address() =
      new(Z) TargetEntryInstr(AllocateBlockId(),
                              CatchClauseNode::kInvalidTryIndex);

  fragment_ = Fragment(new(Z) GotoInstr(join), loop_exit);
  --loop_depth_;
}


}  // namespace dil
}  // namespace dart
