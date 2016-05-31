// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <map>
#include <set>

#include "vm/compiler.h"
#include "vm/dil_to_il.h"
#include "vm/intermediate_language.h"
#include "vm/object_store.h"
#include "vm/stack_frame.h"

namespace dart {
namespace dil {

#define Z (zone_)
#define H (translation_helper_)
#define I Isolate::Current()


class ScopeBuilder : public RecursiveVisitor {
 public:
  explicit ScopeBuilder(FlowGraphBuilder* builder)
      : builder_(builder),
        zone_(builder->zone_),
        translation_helper_(builder->translation_helper_),
        function_scope_(NULL),
        scope_(NULL),
        loop_depth_(0),
        function_depth_(0),
        handler_depth_(0),
        finally_depth_(0),
        name_index_(0),
        setter_value_(NULL) {
  }

  virtual ~ScopeBuilder() {}

  LocalVariable* setter_value() { return setter_value_; }

  void BuildScopes();

  virtual void VisitLet(Let* node);
  virtual void VisitBlock(Block* node);
  virtual void VisitVariableDeclaration(VariableDeclaration* node);
  virtual void VisitWhileStatement(WhileStatement* node);
  virtual void VisitDoStatement(DoStatement* node);
  virtual void VisitForStatement(ForStatement* node);
  virtual void VisitForInStatement(ForInStatement* node);
  virtual void VisitSwitchStatement(SwitchStatement* node);
  virtual void VisitReturnStatement(ReturnStatement* node);
  virtual void VisitTryCatch(TryCatch* node);
  virtual void VisitTryFinally(TryFinally* node);

  virtual void VisitFunctionNode(FunctionNode* node);

 private:
  void EnterScope();
  void ExitScope();

  LocalVariable* MakeVariable(const dart::String& name);
  LocalVariable* MakeVariable(const dart::String& name, const Type& type);

  void AddParameter(VariableDeclaration* declaration, int pos);
  void AddVariable(VariableDeclaration* declaration);
  void AddExceptionVariables();

  const dart::String& GenerateName();

  const dart::String& GenerateHandlerName(const char* base);

  FlowGraphBuilder* builder_;
  Zone* zone_;
  const TranslationHelper& translation_helper_;

  LocalScope* function_scope_;
  LocalScope* scope_;
  int loop_depth_;
  int function_depth_;
  unsigned handler_depth_;
  int finally_depth_;

  int name_index_;

  LocalVariable* setter_value_;
};


void ScopeBuilder::EnterScope() {
  scope_ = new LocalScope(scope_, function_depth_, loop_depth_);
}


void ScopeBuilder::ExitScope() {
  scope_ = scope_->parent();
}


LocalVariable* ScopeBuilder::MakeVariable(const dart::String& name) {
  return new(Z) LocalVariable(TokenPosition::kNoSource,
                              name,
                              Object::dynamic_type());
}


LocalVariable* ScopeBuilder::MakeVariable(const dart::String& name,
                                          const Type& type) {
  return new(Z) LocalVariable(TokenPosition::kNoSource, name, type);
}


void ScopeBuilder::AddParameter(VariableDeclaration* declaration, int pos) {
  LocalVariable* variable = MakeVariable(H.DartSymbol(declaration->name()));
  scope_->InsertParameterAt(pos, variable);
  builder_->locals_[declaration] = variable;
}


void ScopeBuilder::AddExceptionVariables() {
  if (builder_->exception_variables_.size() >= handler_depth_) return;

  ASSERT(builder_->exception_variables_.size() == handler_depth_ - 1);
  ASSERT(builder_->exception_variables_.size() ==
         builder_->stack_trace_variables_.size());
  LocalVariable* exception = MakeVariable(GenerateHandlerName(":exception"));
  LocalVariable* stack_trace =
      MakeVariable(GenerateHandlerName(":stack_trace"));
  function_scope_->AddVariable(exception);
  function_scope_->AddVariable(stack_trace);
  builder_->exception_variables_.push_back(exception);
  builder_->stack_trace_variables_.push_back(stack_trace);
}


const dart::String& ScopeBuilder::GenerateName() {
  char name[64];
  OS::SNPrint(name, 64, ":var%d", name_index_++);
  return H.DartSymbol(name);
}


const dart::String& ScopeBuilder::GenerateHandlerName(const char* base) {
  char name[64];
  OS::SNPrint(name, 64, "%s%d", base, handler_depth_ - 1);
  return H.DartSymbol(name);
}


void ScopeBuilder::AddVariable(VariableDeclaration* declaration) {
  const dart::String& name = declaration->name()->is_empty()
      ? GenerateName()
      : H.DartSymbol(declaration->name());
  LocalVariable* variable = MakeVariable(name);
  scope_->AddVariable(variable);
  builder_->locals_[declaration] = variable;
}


void ScopeBuilder::BuildScopes() {
  ASSERT(scope_ == NULL && loop_depth_ == 0 && function_depth_ == 0);

  ParsedFunction* parsed_function = builder_->parsed_function_;
  function_scope_ = scope_ = new LocalScope(NULL, 0, 0);
  scope_->AddVariable(parsed_function->EnsureExpressionTemp());
  scope_->AddVariable(parsed_function->current_context_var());
  parsed_function->SetNodeSequence(
      new SequenceNode(TokenPosition::kNoSource, scope_));

  const dart::Function& function = parsed_function->function();
  switch (function.kind()) {
    case RawFunction::kClosureFunction:
    case RawFunction::kRegularFunction:
    case RawFunction::kGetterFunction:
    case RawFunction::kSetterFunction:
    case RawFunction::kConstructor: {
      ASSERT(builder_->node_->IsProcedure() ||
             builder_->node_->IsConstructor());
      FunctionNode* node = builder_->node_->IsProcedure()
          ? Procedure::Cast(builder_->node_)->function()
          : Constructor::Cast(builder_->node_)->function();
      bool is_method = !function.IsStaticFunction();
      int pos = 0;
      if (is_method) {
        dart::Class& klass = dart::Class::Handle(Z, function.Owner());
        Type& klass_type =
            Type::ZoneHandle(Z, Type::NewNonParameterizedType(klass));
        LocalVariable* variable =
            MakeVariable(H.DartSymbol("this"), klass_type);
        scope_->InsertParameterAt(pos++, variable);
        builder_->this_variable_ = variable;
      }
      List<VariableDeclaration>& positional = node->positional_parameters();
      for (int i = 0; i < positional.length(); ++i) {
        AddParameter(positional[i], pos++);
      }
      List<VariableDeclaration>& named = node->named_parameters();
      for (int i = 0; i < named.length(); ++i) {
        AddParameter(named[i], pos++);
      }
      break;
    }
    case RawFunction::kImplicitGetter:
    case RawFunction::kImplicitSetter: {
      ASSERT(builder_->node_->IsField());
      bool is_setter = function.IsImplicitSetterFunction();
      bool is_method = !function.IsStaticFunction();
      int pos = 0;
      if (is_method) {
        dart::Class& klass = dart::Class::Handle(Z, function.Owner());
        Type& klass_type =
            Type::ZoneHandle(Z, Type::NewNonParameterizedType(klass));
        LocalVariable* variable =
            MakeVariable(H.DartSymbol("this"), klass_type);
        scope_->InsertParameterAt(pos++, variable);
        builder_->this_variable_ = variable;
      }
      if (is_setter) {
        setter_value_ = MakeVariable(H.DartSymbol("value"));
        scope_->InsertParameterAt(pos++, setter_value_);
      }
      break;
    }
    case RawFunction::kImplicitStaticFinalGetter:
      break;
    default:
      UNREACHABLE();
      break;
  }

  builder_->node_->AcceptVisitor(this);
  parsed_function->AllocateVariables();
}


void ScopeBuilder::VisitLet(Let* node) {
  EnterScope();
  node->VisitChildren(this);
  ExitScope();
}


void ScopeBuilder::VisitBlock(Block* node) {
  EnterScope();
  node->VisitChildren(this);
  ExitScope();
}


void ScopeBuilder::VisitVariableDeclaration(VariableDeclaration* node) {
  AddVariable(node);
  node->VisitChildren(this);
}


void ScopeBuilder::VisitWhileStatement(WhileStatement* node) {
  ++loop_depth_;
  node->VisitChildren(this);
  --loop_depth_;
}


void ScopeBuilder::VisitDoStatement(DoStatement* node) {
  ++loop_depth_;
  node->VisitChildren(this);
  --loop_depth_;
}


void ScopeBuilder::VisitForStatement(ForStatement* node) {
  EnterScope();
  List<VariableDeclaration>& variables = node->variables();
  for (int i = 0; i < variables.length(); ++i) {
    VisitVariableDeclaration(variables[i]);
  }
  ++loop_depth_;
  node->condition()->AcceptExpressionVisitor(this);
  node->body()->AcceptStatementVisitor(this);
  List<Expression>& updates = node->updates();
  for (int i = 0; i < updates.length(); ++i) {
    updates[i]->AcceptExpressionVisitor(this);
  }
  --loop_depth_;
  ExitScope();
}


void ScopeBuilder::VisitForInStatement(ForInStatement* node) {
  node->iterable()->AcceptExpressionVisitor(this);
  ++loop_depth_;
  EnterScope();
  VisitVariableDeclaration(node->variable());
  node->body()->AcceptStatementVisitor(this);
  ExitScope();
  --loop_depth_;
}


void ScopeBuilder::VisitSwitchStatement(SwitchStatement* node) {
  if (!function_scope_->LocalLookupVariable(Symbols::SwitchExpr())) {
    LocalVariable* variable = MakeVariable(Symbols::SwitchExpr());
    function_scope_->AddVariable(variable);
    builder_->switch_variable_ = variable;
  }
  node->VisitChildren(this);
}


void ScopeBuilder::VisitReturnStatement(ReturnStatement* node) {
  if (finally_depth_ > 0) {
    const dart::String& name = H.DartSymbol(":try_finally_return_value");
    if (!function_scope_->LocalLookupVariable(name)) {
      LocalVariable* variable = MakeVariable(name);
      function_scope_->AddVariable(variable);
      builder_->finally_return_variable_ = variable;
    }
  }
  node->VisitChildren(this);
}


void ScopeBuilder::VisitTryCatch(TryCatch* node) {
  node->body()->AcceptStatementVisitor(this);

  ++handler_depth_;
  AddExceptionVariables();
  List<Catch>& catches = node->catches();
  for (int i = 0; i < catches.length(); ++i) {
    EnterScope();
    Catch* ketch = catches[i];
    if (ketch->exception() != NULL) {
      VisitVariableDeclaration(ketch->exception());
    }
    if (ketch->stack_trace() != NULL) {
      VisitVariableDeclaration(ketch->stack_trace());
    }
    ketch->body()->AcceptStatementVisitor(this);
    ExitScope();
  }
  --handler_depth_;
}


void ScopeBuilder::VisitTryFinally(TryFinally* node) {
  ++finally_depth_;
  node->body()->AcceptStatementVisitor(this);
  --finally_depth_;

  ++handler_depth_;
  AddExceptionVariables();
  node->finalizer()->AcceptStatementVisitor(this);
  --handler_depth_;
}


void ScopeBuilder::VisitFunctionNode(FunctionNode* node) {
  List<TypeParameter>& type_parameters = node->type_parameters();
  for (int i = 0; i < type_parameters.length(); ++i) {
    VisitTypeParameter(type_parameters[i]);
  }
  // Do not visit the positional and named parameters, because they've
  // already been added to the scope.
  if (node->body() != NULL) {
    node->body()->AcceptStatementVisitor(this);
  }
}


class BreakableBlock {
 public:
  BreakableBlock(FlowGraphBuilder* builder,
                 LabeledStatement* statement)
      : builder_(builder),
        labeled_statement_(statement),
        destination_(NULL),
        outer_finally_(builder->try_finally_block_) {
    outer_ = builder_->breakable_block_;
    builder_->breakable_block_ = this;
  }
  ~BreakableBlock() {
    builder_->breakable_block_ = outer_;
  }

  bool HadJumper() { return destination_ != NULL; }

  JoinEntryInstr* destination() { return destination_; }

  JoinEntryInstr* BreakDestination(LabeledStatement* label,
                                   TryFinallyBlock** out_finally) {
    BreakableBlock* block = builder_->breakable_block_;
    while (block->labeled_statement_ != label) {
      block = block->outer_;
    }
    ASSERT(block != NULL);
    *out_finally = block->outer_finally_;
    return block->EnsureDestination();
  }

 private:
  JoinEntryInstr* EnsureDestination() {
    if (destination_ == NULL) {
      destination_ = builder_->BuildJoinEntry();
    }
    return destination_;
  }

  FlowGraphBuilder* builder_;
  LabeledStatement* labeled_statement_;
  BreakableBlock* outer_;
  JoinEntryInstr* destination_;
  TryFinallyBlock* outer_finally_;
};


class SwitchBlock {
 public:
  SwitchBlock(FlowGraphBuilder* builder, SwitchStatement* switch_stmt)
      : builder_(builder),
        outer_finally_(builder->try_finally_block_),
        switch_statement_(switch_stmt) {
    outer_ = builder_->switch_block_;
    builder_->switch_block_ = this;
  }
  ~SwitchBlock() {
    builder_->switch_block_ = outer_;
  }

  bool HadJumper(SwitchCase* switch_case) {
    return destinations_.find(switch_case) != destinations_.end();
  }

  JoinEntryInstr* Destination(SwitchCase* label,
                              TryFinallyBlock** outer_finally) {
    // Find corresponding [SwitchStatement].
    SwitchBlock* block = this;
    while (true) {
      block->EnsureSwitchCaseMapping();
      if (block->Contains(label)) break;
    }

    // Set the outer finally block.
    if (outer_finally != NULL) {
      *outer_finally = block->outer_finally_;
    }

    // Ensure there's [JoinEntryInstr] for that [SwitchCase].
    return block->EnsureDestination(label);
  }

 private:
  typedef std::map<SwitchCase*, JoinEntryInstr*> DestinationMap;
  typedef std::set<SwitchCase*> DestinationSwitches;

  JoinEntryInstr* EnsureDestination(SwitchCase* switch_case) {
    DestinationMap::iterator entry = destinations_.find(switch_case);
    if (entry == destinations_.end()) {
      JoinEntryInstr* inst = builder_->BuildJoinEntry();
      destinations_[switch_case] = inst;
      return inst;
    }
    return entry->second;
  }

  void EnsureSwitchCaseMapping() {
    if (destination_switches_.begin() == destination_switches_.end()) {
      List<SwitchCase>& cases = switch_statement_->cases();
      for (int i = 0; i < cases.length(); i++) {
        destination_switches_.insert(cases[i]);
      }
    }
  }

  bool Contains(SwitchCase* sc) {
    return destination_switches_.find(sc) != destination_switches_.end();
  }

  FlowGraphBuilder* builder_;
  SwitchBlock* outer_;

  DestinationMap destinations_;
  DestinationSwitches destination_switches_;

  TryFinallyBlock* outer_finally_;
  SwitchStatement* switch_statement_;
};


class TryFinallyBlock {
 public:
  TryFinallyBlock(FlowGraphBuilder* builder, Statement* finalizer)
      : builder_(builder), finalizer_(finalizer) {
    outer_ = builder_->try_finally_block_;
    builder_->try_finally_block_ = this;
  }
  ~TryFinallyBlock() {
    builder_->try_finally_block_ = outer_;
  }

  Statement* finalizer() { return finalizer_; }
  TryFinallyBlock* outer() { return outer_; }

 private:
  FlowGraphBuilder* builder_;
  TryFinallyBlock* outer_;
  Statement* finalizer_;
};


class TryCatchBlock {
 public:
  explicit TryCatchBlock(FlowGraphBuilder* builder, int try_handler_index = -1)
    : builder_(builder) {
    if (try_handler_index == -1) {
      try_handler_index = builder_->AllocateTryIndex();
    }
    outer_ = builder->try_catch_block_;
    builder->try_catch_block_ = this;
    try_index_ = try_handler_index;
  }
  ~TryCatchBlock() {
    builder_->try_catch_block_ = outer_;
  }

  intptr_t TryIndex() { return try_index_; }

 private:
  FlowGraphBuilder* builder_;
  TryCatchBlock* outer_;
  intptr_t try_index_;
};


class CatchBlock {
 public:
  CatchBlock(FlowGraphBuilder* builder,
             LocalVariable* exception_var,
             LocalVariable* stack_trace_var,
             intptr_t catch_try_index)
    : builder_(builder),
      exception_var_(exception_var),
      stack_trace_var_(stack_trace_var),
      catch_try_index_(catch_try_index) {
    outer_ = builder_->catch_block_;
    builder_->catch_block_ = this;
  }
  ~CatchBlock() {
    builder_->catch_block_ = outer_;
  }

  LocalVariable* exception_var() { return exception_var_; }
  LocalVariable* stack_trace_var() { return stack_trace_var_; }
  intptr_t catch_try_index() { return catch_try_index_; }

 private:
  FlowGraphBuilder* builder_;
  CatchBlock* outer_;
  LocalVariable* exception_var_;
  LocalVariable* stack_trace_var_;
  intptr_t catch_try_index_;
};


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


const dart::String& TranslationHelper::DartSymbol(const char* content) const {
  return dart::String::ZoneHandle(Z, Symbols::New(content));
}


const dart::String& TranslationHelper::DartSymbol(String* content) const {
  return dart::String::ZoneHandle(Z,
      dart::Symbols::FromUTF8(content->buffer(), content->size()));
}


const dart::String& TranslationHelper::DartClassName(dil::Class* dil_klass) {
  if (dil_klass->name() != NULL) {
    ASSERT(dil_klass->IsNormalClass());
    return DartSymbol(dil_klass->name());
  } else {
    ASSERT(dil_klass->IsMixinClass());

    // We construct the string from right to left:
    //     "Base&Mixin1&Mixin2&...&MixinN"
    dart::String& partial = dart::String::Handle(Z, dart::String::New(""));
    dart::String& amp = dart::String::Handle(Z, dart::String::New("&"));
    dart::String& tmp = dart::String::Handle(Z);
    while (dil_klass->name() == NULL) {
      ASSERT(dil_klass->IsMixinClass());

      MixinClass* dil_mixin_class = MixinClass::Cast(dil_klass);
      InterfaceType* base_type = dil_mixin_class->first();
      InterfaceType* mixin_type = dil_mixin_class->second();

      String* mixin_name = NormalClass::Cast(mixin_type->klass())->name();

      tmp ^= dart::String::FromUTF8(mixin_name->buffer(), mixin_name->size());

      partial ^= dart::String::Concat(amp, partial);
      partial ^= dart::String::Concat(tmp, partial);

      dil_klass = base_type->klass();
    }

    tmp ^= dart::String::FromUTF8(
        dil_klass->name()->buffer(), dil_klass->name()->size());

    partial ^= dart::String::Concat(amp, partial);
    partial ^= dart::String::Concat(tmp, partial);

    partial ^= dart::Symbols::New(partial);
    return partial;
  }
}


const dart::String& TranslationHelper::DartConstructorName(Constructor* node) {
  Class* klass = Class::Cast(node->parent());
  return DartFactoryName(klass->name(), node->name()->string());  // NOLINT
}


const dart::String& TranslationHelper::DartProcedureName(Procedure* procedure) {
  if (procedure->kind() == Procedure::kSetter) {
    return DartSetterName(procedure->name()->string());
  } else if (procedure->kind() == Procedure::kGetter) {
    return DartGetterName(procedure->name()->string());
  } else if (procedure->kind() == Procedure::kFactory) {
    return DartFactoryName(
        Class::Cast(procedure->parent())->name(),
        procedure->name()->string());
  } else {
    return DartSymbol(procedure->name()->string());
  }
}


const dart::String& TranslationHelper::DartSetterName(String* content) {
  // The names flowing into [content] are coming from the DILL file:
  //   * user-defined setters: `fieldname=`
  //   * property-set expressions:  `fieldname`
  //
  // The VM uses `get:fieldname` and `set:fieldname`.
  //
  // => In order to be consistent, we remove the `=` always and adopt the VM
  //    conventions.
  ASSERT(content->size() > 0);
  int skip = 0;
  if (content->buffer()[content->size() - 1] == '=') {
    skip = 1;
  }
  dart::String& name = dart::String::ZoneHandle(Z,
      dart::String::FromUTF8(content->buffer(), content->size() - skip));
  return dart::String::ZoneHandle(Z, dart::Field::SetterSymbol(name));
}


const dart::String& TranslationHelper::DartGetterName(String* content) {
  return dart::String::ZoneHandle(Z,
      dart::Field::GetterSymbol(DartString(content)));
}


const dart::String& TranslationHelper::DartInitializerName(String* content) {
  return dart::String::Handle(Z,
      Symbols::FromConcat(Symbols::InitPrefix(), DartSymbol(content)));
}


const dart::String& TranslationHelper::DartFactoryName(String* klass_name,
                                                       String* method_name) {
  // We build a String which looks like <classname>.<constructor-name>.
  dart::String& temp = DartString(klass_name);
  temp ^= dart::String::Concat(temp, Symbols::Dot());
  temp ^= dart::String::Concat(temp, DartString(method_name));
  return dart::String::ZoneHandle(Z, dart::Symbols::New(temp));
}


FlowGraphBuilder::FlowGraphBuilder(TreeNode* node,
                                   ParsedFunction* parsed_function,
                                   int first_block_id)
  : zone_(Thread::Current()->zone()),
    translation_helper_(zone_),
    node_(node),
    parsed_function_(parsed_function),
    ic_data_array_(Z, 0),
    next_block_id_(first_block_id),
    loop_depth_(0),
    handler_depth_(0),
    stack_(NULL),
    pending_argument_count_(0),
    graph_entry_(NULL),
    this_variable_(NULL),
    switch_variable_(NULL),
    finally_return_variable_(NULL),
    breakable_block_(NULL),
    switch_block_(NULL),
    try_finally_block_(NULL),
    try_catch_block_(NULL),
    next_used_try_index_(0),
    catch_block_(NULL) {
}


FlowGraphBuilder::~FlowGraphBuilder() { }


Fragment FlowGraphBuilder::TranslateFinallyFinalizers(
    TryFinallyBlock* outer_finally) {
  TryFinallyBlock* saved = try_finally_block_;

  Fragment instructions;

  // While translating the body of a finalizer we need to set the try-finally
  // block which is active when translating the body.
  while (try_finally_block_ != outer_finally) {
    Statement* finalizer = try_finally_block_->finalizer();
    try_finally_block_ = try_finally_block_->outer();

    // This will potentially have exceptional cases as described in
    // [VisitTryFinally] and will handle them.
    instructions += TranslateStatement(finalizer);

    // We only need to make sure that if the finalizer ended normally, we
    // continue towards the next outer try-finally.
    if (!instructions.is_open()) break;
  }

  try_finally_block_ = saved;

  return instructions;
}


Fragment FlowGraphBuilder::AllocateObject(const dart::Class& klass) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  AllocateObjectInstr* allocate =
      new(Z) AllocateObjectInstr(TokenPosition::kNoSource, klass, arguments);
  Push(allocate);
  return Fragment(allocate);
}


Fragment FlowGraphBuilder::BooleanNegate() {
  BooleanNegateInstr* negate = new(Z) BooleanNegateInstr(Pop());
  Push(negate);
  return Fragment(negate);
}


Fragment FlowGraphBuilder::Boolify() {
  Fragment instructions = Constant(Bool::True());
  Value* constant_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                Pop(),
                                constant_value,
                                false);
  Push(compare);
  return instructions << compare;
}


Fragment FlowGraphBuilder::Branch(TargetEntryInstr** then_entry,
                                  TargetEntryInstr** otherwise_entry) {
  Fragment instructions = Constant(Bool::True());
  Value* constant_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                Pop(),
                                constant_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  *then_entry = *branch->true_successor_address() = BuildTargetEntry();
  *otherwise_entry = *branch->false_successor_address() = BuildTargetEntry();
  return (instructions << branch).closed();
}


Fragment FlowGraphBuilder::BranchIfNull(TargetEntryInstr** then_entry,
                                        TargetEntryInstr** otherwise_entry) {
  Fragment instructions = NullConstant();
  Value* constant_value = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                Pop(),
                                constant_value,
                                false);
  BranchInstr* branch = new(Z) BranchInstr(compare);
  *then_entry = *branch->true_successor_address() = BuildTargetEntry();
  *otherwise_entry = *branch->false_successor_address() = BuildTargetEntry();
  return (instructions << branch).closed();
}


Fragment FlowGraphBuilder::CatchBlockEntry(const Array& handler_types,
                                           int handler_index) {
  CatchBlockEntryInstr* entry =
      new(Z) CatchBlockEntryInstr(AllocateBlockId(),
                                  CurrentTryIndex(),
                                  graph_entry_,
                                  handler_types,
                                  handler_index,
                                  *CurrentException(),
                                  *CurrentStackTrace(),
                                  true);
  graph_entry_->AddCatchEntry(entry);
  return Fragment(entry);
}


Fragment FlowGraphBuilder::TryCatch(int try_handler_index) {
  // The body of the try needs to have it's own block in order to get a new try
  // index.
  //
  // => We therefore create a block for the body (fresh try index) and another
  //    join block (with current try index).
  Fragment body;
  JoinEntryInstr* entry =
      new(Z) JoinEntryInstr(AllocateBlockId(), try_handler_index);
  // TODO(kustermann): With closure support, we need to save the context
  // variable before jumping.
  body += Goto(entry);
  return Fragment(body.entry, entry);
}


Fragment FlowGraphBuilder::CheckStackOverflow() {
  return Fragment(new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource,
                                                 loop_depth_));
}


Fragment FlowGraphBuilder::Constant(const Object& value) {
  ConstantInstr* constant = new(Z) ConstantInstr(value);
  Push(constant);
  return Fragment(constant);
}


Fragment FlowGraphBuilder::CreateArray() {
  Value* element_count = Pop();
  CreateArrayInstr* array =
      new(Z) CreateArrayInstr(TokenPosition::kNoSource,
                              Pop(),  // Element type.
                              element_count);
  Push(array);
  return Fragment(array);
}


Fragment FlowGraphBuilder::Goto(JoinEntryInstr* destination) {
  return Fragment(new(Z) GotoInstr(destination)).closed();
}


Fragment FlowGraphBuilder::IntConstant(int64_t value) {
  return Fragment(
      Constant(Integer::ZoneHandle(Z, Integer::New(value, Heap::kOld))));
}


Fragment FlowGraphBuilder::InstanceCall(const dart::String& name,
                                        Token::Kind kind,
                                        int argument_count) {
  return InstanceCall(name, kind, argument_count, Array::null_array());
}


Fragment FlowGraphBuilder::InstanceCall(const dart::String& name,
                                        Token::Kind kind,
                                        int argument_count,
                                        const Array& argument_names) {
  ArgumentArray arguments = GetArguments(argument_count);
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


Fragment FlowGraphBuilder::ThrowException() {
  Fragment instructions;
  instructions += Drop();
  instructions +=
      Fragment(new(Z) ThrowInstr(TokenPosition::kNoSource)).closed();
  // Use it's side effect of leaving a constant on the stack (does not change
  // the graph).
  NullConstant();
  return instructions;
}


Fragment FlowGraphBuilder::RethrowException(int catch_try_index) {
  Fragment instructions;
  instructions += Drop();
  instructions += Drop();
  instructions += Fragment(new(Z) ReThrowInstr(
      TokenPosition::kNoSource, catch_try_index)).closed();
  // Use it's side effect of leaving a constant on the stack (does not change
  // the graph).
  NullConstant();
  return instructions;
}


Fragment FlowGraphBuilder::LoadField(const dart::Field& field) {
  LoadFieldInstr* load =
      new(Z) LoadFieldInstr(Pop(),
                            &field,
                            AbstractType::ZoneHandle(Z, field.type()),
                            TokenPosition::kNoSource);
  Push(load);
  return Fragment(load);
}


Fragment FlowGraphBuilder::LoadLocal(LocalVariable* variable) {
  LoadLocalInstr* load =
      new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
  Push(load);
  return Fragment(load);
}


Fragment FlowGraphBuilder::InitStaticField(const dart::Field& field) {
  InitStaticFieldInstr* init = new(Z) InitStaticFieldInstr(Pop(), field);
  return Fragment(init);
}


Fragment FlowGraphBuilder::LoadStaticField() {
  LoadStaticFieldInstr* load =
      new(Z) LoadStaticFieldInstr(Pop(), TokenPosition::kNoSource);
  Push(load);
  return Fragment(load);
}


Fragment FlowGraphBuilder::NullConstant() {
  return Constant(Instance::ZoneHandle(Z, Instance::null()));
}


Fragment FlowGraphBuilder::PushArgument() {
  PushArgumentInstr* argument = new(Z) PushArgumentInstr(Pop());
  Push(argument);

  argument->set_temp_index(argument->temp_index() - 1);
  ++pending_argument_count_;

  return Fragment(argument);
}


Fragment FlowGraphBuilder::Return() {
  Value* value = Pop();
  ASSERT(stack_ == NULL);
  return Fragment(new(Z) ReturnInstr(TokenPosition::kNoSource, value)).closed();
}


Fragment FlowGraphBuilder::StaticCall(const Function& target,
                                      int argument_count) {
  return StaticCall(target, argument_count, Array::null_array());
}


Fragment FlowGraphBuilder::StaticCall(const Function& target,
                                      int argument_count,
                                      const Array& argument_names) {
  ArgumentArray arguments = GetArguments(argument_count);
  StaticCallInstr* call =
      new(Z) StaticCallInstr(TokenPosition::kNoSource,
                             target,
                             argument_names,
                             arguments,
                             ic_data_array_);
  Push(call);
  return Fragment(call);
}


Fragment FlowGraphBuilder::StoreIndexed(intptr_t class_id) {
  Value* value = Pop();
  Value* index = Pop();
  // TODO(kmillikin): Omit store barrier when possible (e.g., storing
  // constants).
  StoreIndexedInstr* store =
      new(Z) StoreIndexedInstr(Pop(),  // Array.
                               index,
                               value,
                               kEmitStoreBarrier,
                               Instance::ElementSizeFor(class_id),
                               class_id,
                               Thread::kNoDeoptId,
                               TokenPosition::kNoSource);
  Push(store);
  return Fragment(store);
}


Fragment FlowGraphBuilder::StoreInstanceField(const dart::Field& field) {
  Value* value = Pop();
  StoreInstanceFieldInstr* store =
      new(Z) StoreInstanceFieldInstr(field,
                                     Pop(),
                                     value,
                                     kEmitStoreBarrier,
                                     TokenPosition::kNoSource);
  return Fragment(store);
}


Fragment FlowGraphBuilder::StoreLocal(LocalVariable* variable) {
  StoreLocalInstr* store =
      new(Z) StoreLocalInstr(*variable, Pop(), TokenPosition::kNoSource);
  Push(store);
  return Fragment(store);
}


Fragment FlowGraphBuilder::StoreStaticField(const dart::Field& field) {
  return Fragment(
      new(Z) StoreStaticFieldInstr(field, Pop(), TokenPosition::kNoSource));
}


dart::RawLibrary* FlowGraphBuilder::LookupLibraryByDilLibrary(
    Library* dil_library) {
  const dart::String& library_name = H.DartSymbol(dil_library->import_uri());
  ASSERT(!library_name.IsNull());
  dart::RawLibrary* library = dart::Library::LookupLibrary(library_name);
  ASSERT(library != Object::null());
  return library;
}


dart::RawClass* FlowGraphBuilder::LookupClassByDilClass(Class* dil_klass) {
  dart::RawClass* klass = NULL;

  const dart::String& class_name = H.DartString(dil_klass->name());
  Library* dil_library = Library::Cast(dil_klass->parent());
  dart::Library& library = dart::Library::Handle(Z,
      LookupLibraryByDilLibrary(dil_library));
  klass = library.LookupClassAllowPrivate(class_name);

  ASSERT(klass != Object::null());
  return klass;
}


dart::RawField* FlowGraphBuilder::LookupFieldByDilField(Field* dil_field) {
  TreeNode* node = dil_field->parent();

  dart::Class& klass = dart::Class::Handle(Z);
  if (node->IsClass()) {
    klass ^= LookupClassByDilClass(Class::Cast(node));
  } else {
    ASSERT(node->IsLibrary());
    dart::Library& library = dart::Library::Handle(
        Z, LookupLibraryByDilLibrary(Library::Cast(node)));
    klass = library.toplevel_class();
  }
  dart::RawField* field = klass.LookupFieldAllowPrivate(
      H.DartSymbol(dil_field->name()->string()));
  ASSERT(field != Object::null());
  return field;
}


dart::RawFunction* FlowGraphBuilder::LookupStaticMethodByDilProcedure(
    Procedure* procedure) {
  ASSERT(procedure->IsStatic());
  const dart::String& procedure_name = H.DartProcedureName(procedure);

  // The parent is either a library or a class (in which case the procedure is a
  // static method).
  TreeNode* parent = procedure->parent();
  if (parent->IsClass()) {
    dart::Class& klass = dart::Class::Handle(Z,
        LookupClassByDilClass(Class::Cast(parent)));
    dart::RawFunction* function =
        klass.LookupFunctionAllowPrivate(procedure_name);
    ASSERT(function != Object::null());
    return function;
  } else {
    ASSERT(parent->IsLibrary());
    dart::Library& library = dart::Library::Handle(Z,
        LookupLibraryByDilLibrary(Library::Cast(parent)));
    dart::RawFunction* function =
        library.LookupFunctionAllowPrivate(procedure_name);
    ASSERT(function != Object::null());
    return function;
  }
}


dart::RawFunction* FlowGraphBuilder::LookupConstructorByDilConstructor(
    const dart::Class& owner, Constructor* constructor) {
  dart::RawFunction* function =
      owner.LookupConstructorAllowPrivate(H.DartConstructorName(constructor));
  ASSERT(function != Object::null());
  return function;
}


dart::RawFunction* FlowGraphBuilder::LookupConstructorByDilConstructor(
    Constructor* constructor) {
  Class* dil_klass = Class::Cast(constructor->parent());
  dart::Class& klass = dart::Class::Handle(Z, LookupClassByDilClass(dil_klass));
  return LookupConstructorByDilConstructor(klass, constructor);
}


dart::RawFunction* FlowGraphBuilder::LookupMethodByMember(
    Member* target, const dart::String& method_name) {
  Class* dil_klass = Class::Cast(target->parent());
  dart::Class& klass = dart::Class::Handle(Z, LookupClassByDilClass(dil_klass));

  dart::RawFunction* function = klass.LookupFunctionAllowPrivate(method_name);
  ASSERT(function != Object::null());
  return function;
}


LocalVariable* FlowGraphBuilder::MakeTemporary() {
  char name[64];
  intptr_t index = stack_->definition()->temp_index();
  OS::SNPrint(name, 64, ":temp%" Pd, index);
  LocalVariable* variable =
      new(Z) LocalVariable(TokenPosition::kNoSource,
                           H.DartSymbol(name),
                           *stack_->Type()->ToAbstractType());
  // Set the index relative to the base of the expression stack including
  // outgoing arguments.
  variable->set_index(parsed_function_->first_stack_local_index()
                      - parsed_function_->num_stack_locals()
                      - pending_argument_count_
                      - index);

  // The value has uses as if it were a local variable.  Mark the definition
  // as used so that its temp index will not be cleared (causing it to never
  // be materialized in the expression stack).
  stack_->definition()->set_ssa_temp_index(0);

  return variable;
}


intptr_t FlowGraphBuilder::CurrentTryIndex() {
  if (try_catch_block_ == NULL) {
    return CatchClauseNode::kInvalidTryIndex;
  } else {
    return try_catch_block_->TryIndex();
  }
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
  if (stack_ != NULL) stack_->set_previous_use(NULL);

  value->set_next_use(NULL);
  value->set_previous_use(NULL);
  value->definition()->ClearSSATempIndex();
  return value;
}


Fragment FlowGraphBuilder::Drop() {
  ASSERT(stack_ != NULL);
  Fragment instructions;
  Definition* definition = stack_->definition();
  // The SSA renaming implementation doesn't like [LoadLocal]s without a
  // tempindex.
  if (definition->HasSSATemp() || definition->IsLoadLocal()) {
    instructions <<= new(Z) DropTempsInstr(1, NULL);
  } else {
    definition->ClearTempIndex();
  }

  Pop();
  return instructions;
}


FlowGraph* FlowGraphBuilder::BuildGraph() {
  const dart::Function& function = parsed_function_->function();

  if (function.IsImplicitClosureFunction()) return NULL;
  if (function.IsConstructorClosureFunction()) return NULL;

  // The IR builder will create its own local variables and scopes, and it
  // will not need an AST.  The code generator will assume that there is a
  // local variable stack slot allocated for the current context and (I
  // think) that the runtime will expect it to be at a fixed offset which
  // requires allocating an unused expression temporary variable.
  ScopeBuilder scope_builder(this);
  scope_builder.BuildScopes();

  switch (function.kind()) {
    case RawFunction::kClosureFunction:
    case RawFunction::kRegularFunction:
    case RawFunction::kGetterFunction:
    case RawFunction::kSetterFunction: {
      ASSERT(node_->IsProcedure());
      return BuildGraphOfFunction(Procedure::Cast(node_)->function());
    }
    case RawFunction::kConstructor: {
      ASSERT(node_->IsConstructor());
      Constructor* constructor = Constructor::Cast(node_);
      return BuildGraphOfFunction(constructor->function(), constructor);
    }
    case RawFunction::kImplicitGetter:
    case RawFunction::kImplicitSetter: {
      ASSERT(node_->IsField());
      return BuildGraphOfFieldAccessor(Field::Cast(node_),
                                       scope_builder.setter_value());
    }
    case RawFunction::kImplicitStaticFinalGetter: {
      ASSERT(node_->IsField());
      return BuildGraphOfStaticFieldInitializer(Field::Cast(node_));
    }
    default: {
      UNREACHABLE();
      return NULL;
    }
  }
}


FlowGraph* FlowGraphBuilder::BuildGraphOfFunction(FunctionNode* function,
                                                  Constructor* constructor) {
  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ =
      new(Z) GraphEntryInstr(*parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

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
        const Instance* default_value =
            &Instance::ZoneHandle(Z, Instance::null());
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
        const Instance* default_value =
            &Instance::ZoneHandle(Z, Instance::null());
        default_values->Add(default_value);
      }
    }
    parsed_function_->set_default_parameter_values(default_values);
  }

  Fragment body(normal_entry);
  body += CheckStackOverflow();
  if (constructor != NULL) {
    Class* dil_klass = Class::Cast(constructor->parent());
    body += TranslateInitializers(dil_klass, &constructor->initializers());
  }
  if (function->body() != NULL) {
    body += TranslateStatement(function->body());
  }

  if (body.is_open()) {
    body += NullConstant();
    body += Return();
  }

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfFieldAccessor(
    Field* dil_field,
    LocalVariable* setter_value) {
  const dart::Function& function = parsed_function_->function();

  bool is_setter = function.IsImplicitSetterFunction();
  bool is_method = !function.IsStaticFunction();
  dart::Field& field =
      dart::Field::ZoneHandle(Z, LookupFieldByDilField(dil_field));

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ =
      new(Z) GraphEntryInstr(*parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

  // TODO(kustermann): Add support for FLAG_use_field_guards.
  Fragment body(normal_entry);
  if (is_setter) {
    if (is_method) {
      body += LoadLocal(this_variable_);
      body += LoadLocal(setter_value);
      body += StoreInstanceField(field);
    } else {
      body += LoadLocal(setter_value);
      body += StoreStaticField(field);
    }
    body += NullConstant();
    body += Return();
  } else {
    if (is_method) {
      body += LoadLocal(this_variable_);
      body += LoadField(field);
    } else {
      if (field.has_initializer()) {
        body += Constant(field);
        body += InitStaticField(field);
      }
      body += Constant(field);
      body += LoadStaticField();
    }
    body += Return();
  }

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfStaticFieldInitializer(
    Field* dil_field) {
  ASSERT(dil_field->IsStatic());

  Expression* initializer = dil_field->initializer();

  dart::Field& field =
      dart::Field::ZoneHandle(Z, LookupFieldByDilField(dil_field));

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ =
      new(Z) GraphEntryInstr(*parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

  Fragment body(normal_entry);
  body += TranslateExpression(initializer);

  LocalVariable* field_value = MakeTemporary();
  body += LoadLocal(field_value);
  body += StoreStaticField(field);

  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


TargetEntryInstr* FlowGraphBuilder::BuildTargetEntry() {
  return new(Z) TargetEntryInstr(AllocateBlockId(), CurrentTryIndex());
}


JoinEntryInstr* FlowGraphBuilder::BuildJoinEntry() {
  return new(Z) JoinEntryInstr(AllocateBlockId(), CurrentTryIndex());
}


Fragment FlowGraphBuilder::TranslateInitializers(
    Class* dil_klass, List<Initializer>* initializers) {
  Fragment instructions;

  // These come from:
  //   class A {
  //     var x = (expr);
  //   }
  for (int i = 0; i < dil_klass->fields().length(); i++) {
    Field* dil_field = dil_klass->fields()[i];
    Expression* init = dil_field->initializer();
    if (!dil_field->IsStatic() && init != NULL) {
      dart::Field& field =
          dart::Field::ZoneHandle(Z, LookupFieldByDilField(dil_field));

      // TODO(kustermann): Support FLAG_use_field_guards.
      instructions += LoadLocal(this_variable_);
      instructions += TranslateExpression(init);
      instructions += StoreInstanceField(field);
    }
  }

  // These to come from:
  //   class A {
  //     var x;
  //     var y;
  //     A(this.x) : super(expr), y = (expr);
  //   }
  for (int i = 0; i < initializers->length(); i++) {
    Initializer* initializer = (*initializers)[i];
    if (initializer->IsFieldInitializer()) {
      FieldInitializer* init = FieldInitializer::Cast(initializer);
      dart::Field& field =
          dart::Field::ZoneHandle(Z, LookupFieldByDilField(init->field()));

      // TODO(kustermann): Support FLAG_use_field_guards.
      instructions += LoadLocal(this_variable_);
      instructions += TranslateExpression(init->value());
      instructions += StoreInstanceField(field);
    } else if (initializer->IsSuperInitializer()) {
      SuperInitializer* init = SuperInitializer::Cast(initializer);

      instructions += LoadLocal(this_variable_);
      instructions += PushArgument();

      Array& argument_names = Array::ZoneHandle(Z);
      instructions += TranslateArguments(init->arguments(), &argument_names);

      const Function& target = Function::ZoneHandle(Z,
          LookupConstructorByDilConstructor(init->target()));
      int argument_count = init->arguments()->count() + 1;
      instructions += StaticCall(target, argument_count, argument_names);
      instructions += Drop();
    } else if (initializer->IsRedirectingInitializer()) {
      RedirectingInitializer* init = RedirectingInitializer::Cast(initializer);

      instructions += LoadLocal(this_variable_);
      instructions += PushArgument();

      Array& argument_names = Array::ZoneHandle(Z);
      instructions += TranslateArguments(init->arguments(), &argument_names);

      const Function& target = Function::ZoneHandle(Z,
          LookupConstructorByDilConstructor(init->target()));
      int argument_count = init->arguments()->count() + 1;
      instructions += StaticCall(target, argument_count, argument_names);
      instructions += Drop();
    } else {
      UNIMPLEMENTED();
    }
  }
  return instructions;
}


ArgumentArray FlowGraphBuilder::GetArguments(int count) {
  ArgumentArray arguments =
      new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, count);
  arguments->SetLength(count);
  for (int i = count - 1; i >= 0; --i) {
    ASSERT(stack_->definition()->IsPushArgument());
    ASSERT(!stack_->definition()->HasSSATemp());
    arguments->data()[i] = stack_->definition()->AsPushArgument();
    Drop();
  }
  pending_argument_count_ -= count;
  ASSERT(pending_argument_count_ >= 0);
  return arguments;
}


void FlowGraphBuilder::VisitNullLiteral(NullLiteral* node) {
  fragment_ = Fragment(Constant(Instance::ZoneHandle(Z, Instance::null())));
}


void FlowGraphBuilder::VisitBoolLiteral(BoolLiteral* node) {
  fragment_ = Fragment(Constant(Bool::Get(node->value())));
}


void FlowGraphBuilder::VisitIntLiteral(IntLiteral* node) {
  fragment_ = IntConstant(node->value());
}


void FlowGraphBuilder::VisitBigintLiteral(BigintLiteral* node) {
  const dart::String& value = H.DartString(node->value());
  fragment_ = Fragment(Constant(
      Integer::ZoneHandle(Z, Integer::New(value, Heap::kOld))));
}


void FlowGraphBuilder::VisitDoubleLiteral(DoubleLiteral* node) {
  const dart::String& value = H.DartString(node->value());
  fragment_ = Fragment(Constant(
      Double::ZoneHandle(Z, Double::New(value, Heap::kOld))));
}


void FlowGraphBuilder::VisitStringLiteral(StringLiteral* node) {
  fragment_ = Fragment(Constant(H.DartString(node->value(), Heap::kOld)));
}


void FlowGraphBuilder::VisitSymbolLiteral(SymbolLiteral* node) {
  const dart::String& symbol_value = H.DartSymbol(node->value());

  const dart::Class& symbol_class = dart::Class::ZoneHandle(Z,
      I->object_store()->symbol_class());
  ASSERT(!symbol_class.IsNull());
  const dart::Function& symbol_constructor = Function::ZoneHandle(Z,
      symbol_class.LookupConstructor(Symbols::SymbolCtor()));
  ASSERT(!symbol_constructor.IsNull());

  // TODO(kustermann): We should implement constant canonicalization.
  Fragment instructions;
  instructions += AllocateObject(symbol_class);
  LocalVariable* symbol = MakeTemporary();

  instructions += LoadLocal(symbol);
  instructions += PushArgument();
  instructions += Constant(symbol_value);
  instructions += PushArgument();
  instructions += StaticCall(symbol_constructor, 2);
  instructions += Drop();

  fragment_ = instructions;
}


class DartTypeTranslator : public DartTypeVisitor {
 public:
    explicit DartTypeTranslator(FlowGraphBuilder* owner)
      : owner_(owner),
        zone_(owner->zone_),
        result_(AbstractType::ZoneHandle(Z)) {
  }

  AbstractType& result() { return result_; }

  void VisitDefaultDartType(DartType* node) { UNREACHABLE(); }

  void VisitInterfaceType(InterfaceType* node);

 private:
  FlowGraphBuilder* owner_;
  Zone* zone_;
  AbstractType& result_;
};


void DartTypeTranslator::VisitInterfaceType(InterfaceType* node) {
  if (node->type_arguments().length() != 0) UNIMPLEMENTED();

  const dart::Class& klass =
      dart::Class::Handle(Z, owner_->LookupClassByDilClass(node->klass()));
  result_ ^= klass.DeclarationType();
}


void FlowGraphBuilder::VisitTypeLiteral(TypeLiteral* node) {
  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  fragment_ = Fragment(Constant(translator.result()));
}


void FlowGraphBuilder::VisitVariableGet(VariableGet* node) {
  fragment_ = LoadLocal(LookupVariable(node->variable()));
}


void FlowGraphBuilder::VisitVariableSet(VariableSet* node) {
  LocalVariable* local = LookupVariable(node->variable());
  Fragment instructions = TranslateExpression(node->expression());
  fragment_ = instructions + StoreLocal(local);
}


void FlowGraphBuilder::VisitStaticGet(StaticGet* node) {
  Member* target = node->target();
  if (target->IsField()) {
    Field* dil_field = Field::Cast(target);
    const dart::Field& field =
        dart::Field::ZoneHandle(Z, LookupFieldByDilField(dil_field));
    const dart::Class& owner = dart::Class::Handle(Z, field.Owner());
    const dart::String& getter_name =
        H.DartGetterName(dil_field->name()->string());
    const Function& getter =
        Function::ZoneHandle(Z, owner.LookupStaticFunction(getter_name));
    if (getter.IsNull() || !field.has_initializer()) {
      Fragment instructions = Constant(field);
      fragment_ = instructions + LoadStaticField();
    } else {
      // TODO(kmillikin): figure out how to trigger this case and add tests.
      fragment_ = StaticCall(getter, 0);
    }
  } else {
    ASSERT(target->IsProcedure());

    // Invoke the getter function
    Procedure* procedure = Procedure::Cast(target);
    const Function& target = Function::ZoneHandle(Z,
        LookupStaticMethodByDilProcedure(procedure));

    fragment_ = StaticCall(target, 0);
  }
}


void FlowGraphBuilder::VisitStaticSet(StaticSet* node) {
  Member* target = node->target();
  if (target->IsField()) {
    Field* dil_field = Field::Cast(target);
    const dart::Field& field =
        dart::Field::ZoneHandle(Z, LookupFieldByDilField(dil_field));
    Fragment instructions = TranslateExpression(node->expression());
    LocalVariable* variable = MakeTemporary();
    instructions += LoadLocal(variable);
    fragment_ = instructions + StoreStaticField(field);
  } else {
    ASSERT(target->IsProcedure());

    // Evaluate the expression on the right hand side.
    Fragment instructions = TranslateExpression(node->expression());
    LocalVariable* variable = MakeTemporary();

    // Prepare argument.
    instructions += LoadLocal(variable);
    instructions += PushArgument();

    // Invoke the setter function.
    Procedure* procedure = Procedure::Cast(target);
    const Function& target = Function::ZoneHandle(Z,
        LookupStaticMethodByDilProcedure(procedure));
    instructions += StaticCall(target, 1);

    // Drop the unused result & leave the stored value on the stack.
    fragment_ = instructions + Drop();
  }
}


void FlowGraphBuilder::VisitPropertyGet(PropertyGet* node) {
  Fragment instructions = TranslateExpression(node->receiver());
  instructions += PushArgument();
  const dart::String& getter_name = H.DartGetterName(node->name()->string());
  fragment_ = instructions + InstanceCall(getter_name, Token::kGET, 1);
}


void FlowGraphBuilder::VisitPropertySet(PropertySet* node) {
  Fragment instructions(NullConstant());
  LocalVariable* variable = MakeTemporary();
  instructions += TranslateExpression(node->receiver());
  instructions += PushArgument();
  instructions += TranslateExpression(node->value());
  instructions += StoreLocal(variable);
  instructions += PushArgument();

  const dart::String& setter_name = H.DartSetterName(node->name()->string());
  instructions += InstanceCall(setter_name, Token::kSET, 2);
  fragment_ = instructions + Drop();
}


void FlowGraphBuilder::VisitSuperPropertyGet(SuperPropertyGet* node) {
  const dart::String& method_name =
      H.DartGetterName(node->target()->name()->string());
  const Function& target = Function::ZoneHandle(Z,
      LookupMethodByMember(node->target(), method_name));
  ASSERT(target.IsGetterFunction() || target.IsImplicitGetterFunction());

  Fragment instructions = LoadLocal(this_variable_);
  instructions += PushArgument();
  fragment_ = instructions + StaticCall(target, 1);
}


void FlowGraphBuilder::VisitSuperPropertySet(SuperPropertySet* node) {
  const dart::String& method_name =
      H.DartSetterName(node->target()->name()->string());
  const Function& target = Function::ZoneHandle(Z,
      LookupMethodByMember(node->target(), method_name));
  ASSERT(target.IsSetterFunction() || target.IsImplicitSetterFunction());

  Fragment instructions = TranslateExpression(node->expression());
  LocalVariable* expression = MakeTemporary();
  instructions += LoadLocal(this_variable_);
  instructions += PushArgument();
  instructions += LoadLocal(expression);
  instructions += PushArgument();
  instructions += StaticCall(target, 2);
  instructions += Drop();

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitStaticInvocation(StaticInvocation* node) {
  const Function& target = Function::ZoneHandle(Z,
      LookupStaticMethodByDilProcedure(node->procedure()));
  int argument_count = node->arguments()->count();

  Array& argument_names = Array::ZoneHandle(Z);
  Fragment instructions;

  // The VM requires currently a TypeArguments object as first parameter for
  // every factory constructor :-/ !
  if (target.IsFactory()) {
    argument_count++;
    instructions += Constant(TypeArguments::ZoneHandle(Z));
    instructions += PushArgument();
  }
  instructions += TranslateArguments(node->arguments(), &argument_names);

  fragment_ = instructions +
      StaticCall(target, argument_count, argument_names);
}


void FlowGraphBuilder::VisitMethodInvocation(MethodInvocation* node) {
  Fragment instructions = TranslateExpression(node->receiver());
  instructions += PushArgument();

  Array& argument_names = Array::ZoneHandle(Z);
  instructions += TranslateArguments(node->arguments(), &argument_names);

  const dart::String& name = H.DartSymbol(node->name()->string());  // NOLINT
  int argument_count = node->arguments()->count() + 1;
  fragment_ = instructions +
      InstanceCall(name, Token::kILLEGAL, argument_count, argument_names);
}


void FlowGraphBuilder::VisitSuperMethodInvocation(SuperMethodInvocation* node) {
  const dart::String& method_name =
      H.DartSymbol(node->target()->name()->string());  // NOLINT
  const Function& target = Function::ZoneHandle(Z,
      LookupMethodByMember(node->target(), method_name));

  int argument_count = node->arguments()->count() + 1;
  Array& argument_names = Array::ZoneHandle(Z);

  Fragment instructions = LoadLocal(this_variable_);
  instructions += PushArgument();
  instructions += TranslateArguments(node->arguments(), &argument_names);
  fragment_ = instructions + StaticCall(target, argument_count, argument_names);
}


void FlowGraphBuilder::VisitConstructorInvocation(ConstructorInvocation* node) {
  const dart::Class& klass = dart::Class::ZoneHandle(
      Z, LookupClassByDilClass(Class::Cast(node->target()->parent())));
  Fragment instructions = AllocateObject(klass);
  LocalVariable* variable = MakeTemporary();

  instructions += LoadLocal(variable);
  instructions += PushArgument();

  Array& argument_names = Array::ZoneHandle(Z);
  instructions += TranslateArguments(node->arguments(), &argument_names);

  const Function& target = Function::ZoneHandle(Z,
      LookupConstructorByDilConstructor(klass, node->target()));
  int argument_count = node->arguments()->count() + 1;
  instructions += StaticCall(target, argument_count, argument_names);
  fragment_ = instructions + Drop();
}


void FlowGraphBuilder::VisitIsExpression(IsExpression* node) {
  Fragment instructions = TranslateExpression(node->operand());
  instructions += PushArgument();
  instructions += NullConstant();
  instructions += PushArgument();  // Type arguments.

  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  instructions += Constant(translator.result());
  instructions += PushArgument();  // Type.

  instructions += Constant(Bool::False());
  instructions += PushArgument();  // Negate?.

  fragment_ = instructions +
      InstanceCall(dart::Library::PrivateCoreLibName(Symbols::_instanceOf()),
                   Token::kIS,
                   4);
}


void FlowGraphBuilder::VisitAsExpression(AsExpression* node) {
  Fragment instructions = TranslateExpression(node->operand());
  instructions += PushArgument();
  instructions += NullConstant();
  instructions += PushArgument();  // Type arguments.

  DartTypeTranslator translator(this);
  node->type()->AcceptDartTypeVisitor(&translator);
  instructions += Constant(translator.result());
  instructions += PushArgument();  // Type.

  fragment_ = instructions +
      InstanceCall(dart::Library::PrivateCoreLibName(Symbols::_as()),
                   Token::kAS,
                   3);
}


void FlowGraphBuilder::VisitConditionalExpression(ConditionalExpression* node) {
  Fragment instructions = TranslateExpression(node->condition());
  TargetEntryInstr* then_entry;
  TargetEntryInstr* otherwise_entry;
  instructions += Branch(&then_entry, &otherwise_entry);

  Value* top = stack_;
  Fragment then_fragment(then_entry);
  then_fragment += TranslateExpression(node->then());
  then_fragment += StoreLocal(parsed_function_->expression_temp_var());
  then_fragment += Drop();

  ASSERT(stack_ == top);
  Fragment otherwise_fragment(otherwise_entry);
  otherwise_fragment += TranslateExpression(node->otherwise());
  otherwise_fragment += StoreLocal(parsed_function_->expression_temp_var());
  otherwise_fragment += Drop();

  JoinEntryInstr* join = BuildJoinEntry();
  then_fragment += Goto(join);
  otherwise_fragment += Goto(join);

  fragment_ = Fragment(instructions.entry, join) +
      LoadLocal(parsed_function_->expression_temp_var());
}


void FlowGraphBuilder::VisitLogicalExpression(LogicalExpression* node) {
  if (node->op() == LogicalExpression::kAnd ||
      node->op() == LogicalExpression::kOr) {
    Fragment instructions = TranslateExpression(node->left());
    TargetEntryInstr* right_entry;
    TargetEntryInstr* constant_entry;

    if (node->op() == LogicalExpression::kAnd) {
      instructions += Branch(&right_entry, &constant_entry);
    } else {
      instructions += Branch(&constant_entry, &right_entry);
    }

    Value* top = stack_;
    Fragment right_fragment(right_entry);
    right_fragment += TranslateExpression(node->right());
    right_fragment += Boolify();
    right_fragment += StoreLocal(parsed_function_->expression_temp_var());
    right_fragment += Drop();

    ASSERT(top == stack_);
    Fragment constant_fragment(constant_entry);
    constant_fragment +=
        Constant(Bool::Get(node->op() == LogicalExpression::kOr));
    constant_fragment += StoreLocal(parsed_function_->expression_temp_var());
    constant_fragment += Drop();

    JoinEntryInstr* join = BuildJoinEntry();
    right_fragment += Goto(join);
    constant_fragment += Goto(join);

    fragment_ = Fragment(instructions.entry, join)
        + LoadLocal(parsed_function_->expression_temp_var());
  } else {
    ASSERT(node->op() == LogicalExpression::kIfNull);

    TargetEntryInstr* null_entry;
    TargetEntryInstr* nonnull_entry;
    JoinEntryInstr* join = BuildJoinEntry();

    // Evaluate `left` of `left ?? right` and compare it to null.
    Fragment instructions = TranslateExpression(node->left());
    instructions += StoreLocal(parsed_function_->expression_temp_var());
    instructions += BranchIfNull(&null_entry, &nonnull_entry);

    // If `left` was non-null we are done.
    Fragment nonnull_fragment(nonnull_entry);
    nonnull_fragment += Goto(join);

    // If `left` was null we evaluate `right`.
    Fragment null_fragment(null_entry);
    null_fragment += TranslateExpression(node->right());
    null_fragment += StoreLocal(parsed_function_->expression_temp_var());
    null_fragment += Drop();
    null_fragment += Goto(join);

    fragment_ =
        Fragment(instructions.entry, join) +
        LoadLocal(parsed_function_->expression_temp_var());
  }
}


void FlowGraphBuilder::VisitNot(Not* node) {
  Fragment instructions = TranslateExpression(node->expression());
  fragment_ = instructions + BooleanNegate();
}


void FlowGraphBuilder::VisitThisExpression(ThisExpression* node) {
  fragment_ = LoadLocal(this_variable_);
}


void FlowGraphBuilder::VisitStringConcatenation(StringConcatenation* node) {
  dart::Library& core = dart::Library::Handle(Z,
      dart::Library::LookupLibrary(H.DartSymbol("dart:core")));
  dart::Class& sb_klass = dart::Class::Handle(Z,
      core.LookupClassAllowPrivate(H.DartSymbol("StringBuffer")));
  dart::Function& sb_const = dart::Function::ZoneHandle(Z,
      sb_klass.LookupConstructor(H.DartSymbol("StringBuffer.")));

  Fragment instructions;
  instructions += AllocateObject(sb_klass);
  LocalVariable* sb = MakeTemporary();

  instructions += LoadLocal(sb);
  instructions += PushArgument();
  instructions += StaticCall(sb_const, 1);
  instructions += Drop();

  for (int i = 0; i < node->expressions().length(); i++) {
    instructions += LoadLocal(sb);
    instructions += PushArgument();
    instructions += TranslateExpression(node->expressions()[i]);
    instructions += PushArgument();
    instructions += InstanceCall(H.DartSymbol("write"), Token::kILLEGAL, 2);
    instructions += Drop();
  }

  instructions += PushArgument();
  instructions += InstanceCall(H.DartSymbol("toString"), Token::kILLEGAL, 1);

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitListLiteral(ListLiteral* node) {
  if (node->is_const() || !node->type()->IsDynamicType()) {
    UNIMPLEMENTED();
  }
  // The type argument for the factory call.
  Fragment instructions = Constant(TypeArguments::ZoneHandle(Z));
  instructions += PushArgument();
  // The type arguments for CreateArray.
  instructions += Constant(TypeArguments::ZoneHandle(Z));
  List<Expression>& expressions = node->expressions();
  instructions += IntConstant(expressions.length());
  instructions += CreateArray();

  LocalVariable* array = MakeTemporary();
  for (int i = 0; i < expressions.length(); ++i) {
    instructions += LoadLocal(array);
    instructions += IntConstant(i);
    instructions += TranslateExpression(expressions[i]);
    instructions += StoreIndexed(kArrayCid);
    instructions += Drop();
  }

  const dart::Class& factory_class = dart::Class::Handle(Z,
      dart::Library::LookupCoreClass(Symbols::List()));
  const Function& factory_method = Function::ZoneHandle(Z,
      factory_class.LookupFactory(
          dart::Library::PrivateCoreLibName(Symbols::ListLiteralFactory())));
  instructions += PushArgument();  // The array.
  fragment_ = instructions + StaticCall(factory_method, 2);
}


void FlowGraphBuilder::VisitMapLiteral(MapLiteral* node) {
  if (node->is_const() ||
      !node->key_type()->IsDynamicType() ||
      !node->value_type()->IsDynamicType()) {
    UNIMPLEMENTED();
  }

  const dart::Class& map_class = dart::Class::Handle(Z,
      dart::Library::LookupCoreClass(Symbols::Map()));
  const Function& factory_method = Function::ZoneHandle(Z,
      map_class.LookupFactory(
          dart::Library::PrivateCoreLibName(Symbols::MapLiteralFactory())));

  // The type argument for the factory call `new Map<K, V>._fromLiteral(List)`.
  Fragment instructions = Constant(TypeArguments::ZoneHandle(Z));
  instructions += PushArgument();

  // The type arguments for `new List<X>(int len)`.
  instructions += Constant(TypeArguments::ZoneHandle(Z));
  List<MapEntry>& entries = node->entries();

  // We generate a list of tuples, i.e. [key1, value1, ..., keyN, valueN].
  instructions += IntConstant(2 * entries.length());
  instructions += CreateArray();

  LocalVariable* array = MakeTemporary();
  for (int i = 0; i < entries.length(); ++i) {
    instructions += LoadLocal(array);
    instructions += IntConstant(2 * i);
    instructions += TranslateExpression(entries[i]->key());
    instructions += StoreIndexed(kArrayCid);
    instructions += Drop();

    instructions += LoadLocal(array);
    instructions += IntConstant(2 * i + 1);
    instructions += TranslateExpression(entries[i]->value());
    instructions += StoreIndexed(kArrayCid);
    instructions += Drop();
  }

  instructions += PushArgument();  // The array.
  fragment_ = instructions + StaticCall(factory_method, 2);
}


void FlowGraphBuilder::VisitLet(Let* node) {
  Fragment instructions = TranslateStatement(node->variable());
  ASSERT(instructions.is_open());
  instructions += TranslateExpression(node->body());
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitThrow(Throw* node) {
  Fragment instructions;

  instructions += TranslateExpression(node->expression());
  instructions += PushArgument();
  instructions += ThrowException();
  ASSERT(instructions.is_closed());

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitRethrow(Rethrow* node) {
  Fragment instructions;

  instructions += LoadLocal(catch_block_->exception_var());
  instructions += PushArgument();
  instructions += LoadLocal(catch_block_->stack_trace_var());
  instructions += PushArgument();
  instructions += RethrowException(catch_block_->catch_try_index());

  fragment_ = instructions;
}


Fragment FlowGraphBuilder::TranslateArguments(Arguments* node,
                                              Array* argument_names) {
  if (node->types().length() != 0) {
    UNIMPLEMENTED();
  }
  Fragment instructions;

  List<Expression>& positional = node->positional();
  List<NamedExpression>& named = node->named();
  if (named.length() == 0) {
    *argument_names ^= Array::null();
  } else {
    *argument_names ^= Array::New(named.length());
  }

  for (int i = 0; i < positional.length(); ++i) {
    instructions += TranslateExpression(positional[i]);
    instructions += PushArgument();
  }
  for (int i = 0; i < named.length(); ++i) {
    NamedExpression* named_expression = named[i];
    instructions += TranslateExpression(named_expression->expression());
    instructions += PushArgument();
    argument_names->SetAt(i, H.DartSymbol(named_expression->name()));
  }
  return instructions;
}


void FlowGraphBuilder::VisitEmptyStatement(EmptyStatement* node) {
  fragment_ = Fragment();
}


void FlowGraphBuilder::VisitBlock(Block* node) {
  Fragment instructions;
  List<Statement>& statements = node->statements();
  for (int i = 0; i < statements.length(); ++i) {
    instructions += TranslateStatement(statements[i]);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitReturnStatement(ReturnStatement* node) {
  bool inside_try_finally = try_finally_block_ != NULL;

  Fragment instructions = node->expression() == NULL
      ? NullConstant()
      : TranslateExpression(node->expression());
  if (inside_try_finally) {
    ASSERT(finally_return_variable_ != NULL);
    instructions += StoreLocal(finally_return_variable_);
    instructions += Drop();
    instructions += TranslateFinallyFinalizers(NULL);
    if (instructions.is_open()) {
      instructions += LoadLocal(finally_return_variable_);
      instructions += Return();
    }
  } else {
    instructions += Return();
  }
  fragment_ = instructions.closed();
}


void FlowGraphBuilder::VisitExpressionStatement(ExpressionStatement* node) {
  Fragment instructions = TranslateExpression(node->expression());
  instructions += Drop();
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitVariableDeclaration(VariableDeclaration* node) {
  Fragment instructions = node->initializer() == NULL
      ? NullConstant()
      : TranslateExpression(node->initializer());
  instructions += StoreLocal(LookupVariable(node));
  instructions += Drop();
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitIfStatement(IfStatement* node) {
  Fragment instructions = TranslateExpression(node->condition());
  TargetEntryInstr* then_entry;
  TargetEntryInstr* otherwise_entry;
  instructions += Branch(&then_entry, &otherwise_entry);

  Fragment then_fragment(then_entry);
  then_fragment += TranslateStatement(node->then());

  Fragment otherwise_fragment(otherwise_entry);
  otherwise_fragment += TranslateStatement(node->otherwise());

  if (then_fragment.is_open()) {
    if (otherwise_fragment.is_open()) {
      JoinEntryInstr* join = BuildJoinEntry();
      then_fragment += Goto(join);
      otherwise_fragment += Goto(join);
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
  Fragment condition = TranslateExpression(node->condition());
  TargetEntryInstr* body_entry;
  TargetEntryInstr* loop_exit;
  condition += Branch(&body_entry, &loop_exit);

  Fragment body(body_entry);
  body += TranslateStatement(node->body());

  Instruction* entry;
  if (body.is_open()) {
    JoinEntryInstr* join = BuildJoinEntry();
    body += Goto(join);

    Fragment loop(join);
    loop += CheckStackOverflow();
    loop += condition;
    entry = new(Z) GotoInstr(join);
  } else {
    entry = condition.entry;
  }


  fragment_ = Fragment(entry, loop_exit);
  --loop_depth_;
}


void FlowGraphBuilder::VisitDoStatement(DoStatement* node) {
  ++loop_depth_;
  Fragment body = TranslateStatement(node->body());

  if (body.is_closed()) {
    fragment_ = body;
    --loop_depth_;
    return;
  }

  JoinEntryInstr* join = BuildJoinEntry();
  Fragment loop(join);
  loop += CheckStackOverflow();
  loop += body;
  loop += TranslateExpression(node->condition());
  TargetEntryInstr* loop_repeat;
  TargetEntryInstr* loop_exit;
  loop += Branch(&loop_repeat, &loop_exit);

  Fragment repeat(loop_repeat);
  repeat += Goto(join);

  fragment_ = Fragment(new(Z) GotoInstr(join), loop_exit);
  --loop_depth_;
}


void FlowGraphBuilder::VisitForStatement(ForStatement* node) {
  Fragment declarations;
  List<VariableDeclaration>& variables = node->variables();
  for (int i = 0; i < variables.length(); ++i) {
    declarations += TranslateStatement(variables[i]);
  }

  ++loop_depth_;
  Fragment condition = TranslateExpression(node->condition());
  TargetEntryInstr* body_entry;
  TargetEntryInstr* loop_exit;
  condition += Branch(&body_entry, &loop_exit);

  Fragment body(body_entry);
  body += TranslateStatement(node->body());

  if (body.is_open()) {
    List<Expression>& updates = node->updates();
    for (int i = 0; i < updates.length(); ++i) {
      body += TranslateExpression(updates[i]);
      body += Drop();
    }
    JoinEntryInstr* join = BuildJoinEntry();
    declarations += Goto(join);
    body += Goto(join);

    Fragment loop(join);
    loop += CheckStackOverflow();
    loop += condition;
  } else {
    declarations += condition;
  }

  fragment_ = Fragment(declarations.entry, loop_exit);
  --loop_depth_;
}


void FlowGraphBuilder::VisitForInStatement(ForInStatement* node) {
  Fragment instructions = TranslateExpression(node->iterable());
  instructions += PushArgument();

  const dart::String& iterator_getter = dart::String::ZoneHandle(Z,
      dart::Field::GetterSymbol(Symbols::Iterator()));
  instructions += InstanceCall(iterator_getter, Token::kGET, 1);
  LocalVariable* iterator = MakeTemporary();

  ++loop_depth_;
  Fragment condition = LoadLocal(iterator);
  condition += PushArgument();
  condition += InstanceCall(Symbols::MoveNext(), Token::kILLEGAL, 1);
  TargetEntryInstr* body_entry;
  TargetEntryInstr* loop_exit;
  condition += Branch(&body_entry, &loop_exit);

  Fragment body(body_entry);
  body += LoadLocal(iterator);
  body += PushArgument();
  const dart::String& current_getter = dart::String::ZoneHandle(Z,
      dart::Field::GetterSymbol(Symbols::Current()));
  body += InstanceCall(current_getter, Token::kGET, 1);
  body += StoreLocal(LookupVariable(node->variable()));
  body += Drop();
  body += TranslateStatement(node->body());

  if (body.is_open()) {
    JoinEntryInstr* join = BuildJoinEntry();
    instructions += Goto(join);
    body += Goto(join);

    Fragment loop(join);
    loop += CheckStackOverflow();
    loop += condition;
  } else {
    instructions += condition;
  }

  fragment_ = Fragment(instructions.entry, loop_exit) + Drop();
  --loop_depth_;
}


void FlowGraphBuilder::VisitLabeledStatement(LabeledStatement* node) {
  // There can be serveral cases:
  //
  //   * the body contains a break
  //   * the body doesn't contain a break
  //
  //   * translating the body results in a closed fragment
  //   * translating the body results in a open fragment
  //
  // => We will only know which case we are in after the body has been
  //    traversed.

  BreakableBlock block(this, node);
  Fragment instructions = TranslateStatement(node->body());
  if (block.HadJumper()) {
    if (instructions.is_open()) {
      instructions += Goto(block.destination());
    }
    fragment_ = Fragment(instructions.entry, block.destination());
  } else {
    fragment_ = instructions;
  }
}


void FlowGraphBuilder::VisitBreakStatement(BreakStatement* node) {
  TryFinallyBlock* outer_finally = NULL;
  JoinEntryInstr* destination =
      breakable_block_->BreakDestination(node->target(), &outer_finally);

  Fragment instructions;
  instructions += TranslateFinallyFinalizers(outer_finally);
  if (instructions.is_open()) {
    instructions += Goto(destination);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitSwitchStatement(SwitchStatement* node) {
  SwitchBlock block(this, node);

  // Instead of using a variable we should reuse the expression on the stack,
  // since it won't be assigned again, we don't need phi nodes.
  Fragment head_instructions = TranslateExpression(node->condition());
  head_instructions += StoreLocal(switch_variable_);
  head_instructions += Drop();

  // Phase 1: Generate bodies and try to find out whether a body will be target
  // of a jump due to:
  //   * `continue case_label`
  //   * `case e1: case e2: body`
  Fragment* body_fragments = new Fragment[node->cases().length()];

  for (int i = 0; i < node->cases().length(); i++) {
    SwitchCase* switch_case = node->cases()[i];
    body_fragments[i] = TranslateStatement(switch_case->body());
    // If there is an implicit fall-through we have one [SwitchCase] and
    // multiple expressions, e.g.
    //
    //    switch(expr) {
    //      case a:
    //      case b:
    //        <stmt-body>
    //    }
    //
    // This means that the <stmt-body> will have more than 1 incoming edge (one
    // from `a == expr` and one from `a != expr && b == expr`). The
    // `block.Destination()` records the additional jump.
    if (switch_case->expressions().length() > 1) {
      block.Destination(switch_case, NULL);
    }

    ASSERT(i == (node->cases().length() - 1) || body_fragments[i].is_closed());
  }

  // Phase 2: Generate everything except the real bodies:
  //   * jump directly to a body (if there is no jumper)
  //   * jump to a wrapper block which jumps to the body (if there is a jumper)
  Fragment current_instructions = head_instructions;
  for (int i = 0; i < node->cases().length(); i++) {
    SwitchCase* switch_case = node->cases()[i];

    if (switch_case->is_default()) {
      ASSERT(i == (node->cases().length() - 1));
      ASSERT(switch_case->expressions().length() == 0);

      if (block.HadJumper(switch_case)) {
        // There are several branches to the body, so we will make a goto to
        // the join block (and prepend a join instruction to the real body).
        JoinEntryInstr* join = block.Destination(switch_case, NULL);
        current_instructions += Goto(join);

        current_instructions = Fragment(current_instructions.entry, join);
        current_instructions += body_fragments[i];
      } else {
        current_instructions += body_fragments[i];
      }
    } else {
      for (int j = 0; j < switch_case->expressions().length(); j++) {
        TargetEntryInstr* then;
        TargetEntryInstr* otherwise;

        current_instructions +=
            TranslateExpression(switch_case->expressions()[j]);
        current_instructions += PushArgument();
        current_instructions += LoadLocal(switch_variable_);
        current_instructions += PushArgument();
        current_instructions +=
            InstanceCall(Symbols::EqualOperator(), Token::kILLEGAL, 2);
        current_instructions += Branch(&then, &otherwise);

        Fragment then_fragment(then);

        if (block.HadJumper(switch_case)) {
          // There are several branches to the body, so we will make a goto to
          // the join block (and prepend a join instruction to the real body).
          JoinEntryInstr* join = block.Destination(switch_case, NULL);
          then_fragment += Goto(join);
          Fragment real_body(join);
          real_body += body_fragments[i];
        } else {
          // There is only a signle branch to the body, so we will just append
          // the body fragment.
          then_fragment += body_fragments[i];
        }

        current_instructions = Fragment(otherwise);
      }
    }
  }

  delete[] body_fragments;

  fragment_ = Fragment(head_instructions.entry, current_instructions.current);
}


void FlowGraphBuilder::VisitContinueSwitchStatement(
    ContinueSwitchStatement* node) {
  TryFinallyBlock* outer_finally = NULL;
  JoinEntryInstr* entry =
      switch_block_->Destination(node->target(), &outer_finally);

  Fragment instructions;
  instructions += TranslateFinallyFinalizers(outer_finally);
  if (instructions.is_open()) {
    instructions += Goto(entry);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitTryFinally(TryFinally* node) {
  // There are 5 different cases where we need to execute the finally block:
  //
  //  a) 1/2/3th case: Special control flow going out of `node->body()`:
  //
  //   * [BreakStatement] transfers control to a [LabledStatement]
  //   * [ContinueSwitchStatement] transfers control to a [SwitchCase]
  //   * [ReturnStatement] returns a value
  //
  //   => All three cases will automatically append all finally blocks
  //      between the branching point and the destination (so we don't need to
  //      do anything here).
  //
  //  b) 4th case: Translating the body resulted in an open fragment (i.e. body
  //               executes without any control flow out of it)
  //
  //   => We are responsible for jumping out of the body to a new block (with
  //      different try index) and execute the finalizer.
  //
  //  c) 5th case: An exception occured inside the body.
  //
  //   => We are responsible for catching it, executing the finally block and
  //      rethrowing the exception.
  int try_handler_index = AllocateTryIndex();
  Fragment try_body = TryCatch(try_handler_index);

  // TODO(kustermann): With closure support, we need to restore the context
  // variable here.
  JoinEntryInstr* after_try = BuildJoinEntry();

  // Fill in the body of the try.
  {
    TryCatchBlock tcb(this, try_handler_index);
    TryFinallyBlock tfb(this, node->finalizer());
    try_body += TranslateStatement(node->body());
  }

  if (try_body.is_open()) {
    // Please note: The try index will be on level out of this block,
    // thereby ensuring if there's an exception in the finally block we
    // won't run it twice.
    JoinEntryInstr* finally_entry = BuildJoinEntry();

    try_body += Goto(finally_entry);

    Fragment finally_body(finally_entry);
    finally_body += TranslateStatement(node->finalizer());
    finally_body += Goto(after_try);
  }

  // Fill in the body of the catch.
  ++handler_depth_;
  const Array& handler_types = Array::ZoneHandle(Z, Array::New(1, Heap::kOld));
  handler_types.SetAt(0, Object::dynamic_type());
  Fragment finally_body = CatchBlockEntry(handler_types, try_handler_index);
  finally_body += TranslateStatement(node->finalizer());
  if (finally_body.is_open()) {
    finally_body += LoadLocal(CurrentException());
    finally_body += PushArgument();
    finally_body += LoadLocal(CurrentStackTrace());
    finally_body += PushArgument();
    finally_body += RethrowException(try_handler_index);
    Drop();
  }
  --handler_depth_;

  fragment_ = Fragment(try_body.entry, after_try);
}


void FlowGraphBuilder::VisitTryCatch(class TryCatch* node) {
  int try_handler_index = AllocateTryIndex();
  Fragment try_body = TryCatch(try_handler_index);

  // TODO(kustermann): With closure support, we need to restore the context
  // variable here.
  JoinEntryInstr* after_try = BuildJoinEntry();

  // Fill in the body of the try.
  {
    TryCatchBlock block(this, try_handler_index);
    try_body += TranslateStatement(node->body());
    try_body += Goto(after_try);
  }

  ++handler_depth_;
  const Array& handler_types =
      Array::ZoneHandle(Z, Array::New(node->catches().length(), Heap::kOld));
  Fragment catch_body = CatchBlockEntry(handler_types, try_handler_index);
  // Fill in the body of the catch.
  for (int i = 0; i < node->catches().length(); i++) {
    Catch* catch_clause = node->catches()[i];

    Fragment catch_handler_body;
    if (catch_clause->exception() != NULL) {
      catch_handler_body += LoadLocal(CurrentException());
      catch_handler_body +=
          StoreLocal(LookupVariable(catch_clause->exception()));
      catch_handler_body += Drop();
    }
    if (catch_clause->stack_trace() != NULL) {
      catch_handler_body += LoadLocal(CurrentStackTrace());
      catch_handler_body +=
          StoreLocal(LookupVariable(catch_clause->stack_trace()));
      catch_handler_body += Drop();
    }
    DartTypeTranslator type_translator(this);
    if (catch_clause->guard() != NULL) {
      catch_clause->guard()->AcceptDartTypeVisitor(&type_translator);
      handler_types.SetAt(i, type_translator.result());
    } else {
      handler_types.SetAt(i, Object::dynamic_type());
    }

    {
      CatchBlock block(this, CurrentException(), CurrentStackTrace(),
                       try_handler_index);

      catch_handler_body += TranslateStatement(catch_clause->body());
      if (catch_handler_body.is_open()) {
        catch_handler_body += Goto(after_try);
      }
    }

    if (catch_clause->guard() != NULL) {
      catch_body += LoadLocal(CurrentException());
      catch_body += PushArgument();  // exception
      catch_body += NullConstant();
      catch_body += PushArgument();  // type arguments
      catch_body += Constant(type_translator.result());
      catch_body += PushArgument();  // guard type
      catch_body += Constant(Object::bool_false());
      catch_body += PushArgument();  // negate
      catch_body += InstanceCall(
          dart::Library::PrivateCoreLibName(Symbols::_instanceOf()),
          Token::kIS,
          4);

      TargetEntryInstr* catch_entry;
      TargetEntryInstr* next_catch_entry;
      catch_body += Branch(&catch_entry, &next_catch_entry);

      Fragment(catch_entry) + catch_handler_body;
      catch_body = Fragment(next_catch_entry);
    } else {
      catch_body += catch_handler_body;
    }
  }

  // In case the last catch body was not handling the exception and branching to
  // after the try block, we will rethrow the exception (i.e. no default catch
  // handler).
  if (catch_body.is_open()) {
    catch_body += LoadLocal(CurrentException());
    catch_body += PushArgument();
    catch_body += LoadLocal(CurrentStackTrace());
    catch_body += PushArgument();
    catch_body += RethrowException(try_handler_index);
    Drop();
  }
  --handler_depth_;

  fragment_ = Fragment(try_body.entry, after_try);
}


}  // namespace dil
}  // namespace dart
