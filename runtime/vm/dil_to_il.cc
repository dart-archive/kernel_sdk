// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <map>
#include <set>

#include "vm/dil_to_il.h"

#include "vm/compiler.h"
#include "vm/dil_reader.h"
#include "vm/intermediate_language.h"
#include "vm/object_store.h"
#include "vm/report.h"
#include "vm/resolver.h"
#include "vm/stack_frame.h"

namespace dart {
namespace dil {

#define Z (zone_)
#define H (translation_helper_)
#define T (type_translator_)
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
        for_in_depth_(0),
        name_index_(0),
        setter_value_(NULL) {
  }

  virtual ~ScopeBuilder() {}

  LocalVariable* setter_value() { return setter_value_; }

  void BuildScopes();

  virtual void VisitName(Name* node) { /* NOP */ }

  virtual void VisitThisExpression(ThisExpression* node);
  virtual void VisitTypeParameterType(TypeParameterType* node);
  virtual void VisitVariableGet(VariableGet* node);
  virtual void VisitVariableSet(VariableSet* node);
  virtual void VisitSuperPropertyGet(SuperPropertyGet* node);
  virtual void VisitSuperPropertySet(SuperPropertySet* node);
  virtual void VisitSuperMethodInvocation(SuperMethodInvocation* node);
  virtual void VisitFunctionExpression(FunctionExpression* node);
  virtual void VisitLet(Let* node);
  virtual void VisitBlock(Block* node);
  virtual void VisitVariableDeclaration(VariableDeclaration* node);
  virtual void VisitFunctionDeclaration(FunctionDeclaration* node);
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
  void EnterScope(TreeNode* node);
  void ExitScope();

  LocalVariable* MakeVariable(const dart::String& name);
  LocalVariable* MakeVariable(const dart::String& name, const Type& type);

  void AddParameters(FunctionNode* function, int pos = 0);
  void AddParameter(VariableDeclaration* declaration, int pos);
  void AddVariable(VariableDeclaration* declaration);
  void AddExceptionVariables();
  void AddIteratorVariable();

  // Record an assignment or reference to a variable.  If the occurrence is
  // in a nested function, ensure that the variable is handled properly as a
  // captured variable.
  void LookupVariable(VariableDeclaration* declaration);

  const dart::String& GenerateName();
  const dart::String& GenerateHandlerName(const char* base);
  const dart::String& GenerateIteratorName();

  void HandleLocalFunction(TreeNode* parent, FunctionNode* function);
  void HandleSpecialLoad(LocalVariable** variable, const dart::String& symbol);

  FlowGraphBuilder* builder_;
  Zone* zone_;
  const TranslationHelper& translation_helper_;

  LocalScope* function_scope_;
  LocalScope* scope_;
  int loop_depth_;
  int function_depth_;
  unsigned handler_depth_;
  int finally_depth_;
  unsigned for_in_depth_;

  int name_index_;

  LocalVariable* setter_value_;
};


void ScopeBuilder::EnterScope(TreeNode* node) {
  scope_ = new(Z) LocalScope(scope_, function_depth_, loop_depth_);
  builder_->scopes_[node] = scope_;
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


void ScopeBuilder::AddParameters(FunctionNode* function, int pos) {
  List<VariableDeclaration>& positional = function->positional_parameters();
  for (int i = 0; i < positional.length(); ++i) {
    AddParameter(positional[i], pos++);
  }
  List<VariableDeclaration>& named = function->named_parameters();
  for (int i = 0; i < named.length(); ++i) {
    AddParameter(named[i], pos++);
  }
}


void ScopeBuilder::AddParameter(VariableDeclaration* declaration, int pos) {
  // TODO(kmillikin): Handle final.
  LocalVariable* variable = MakeVariable(H.DartSymbol(declaration->name()));
  scope_->InsertParameterAt(pos, variable);
  builder_->locals_[declaration] = variable;
}


void ScopeBuilder::AddExceptionVariables() {
  if (builder_->exception_variables_.size() >= handler_depth_) return;

  ASSERT(builder_->exception_variables_.size() == handler_depth_ - 1);
  ASSERT(builder_->exception_variables_.size() ==
         builder_->stack_trace_variables_.size());
  ASSERT(builder_->exception_variables_.size() ==
         builder_->catch_context_variables_.size());
  LocalVariable* exception = MakeVariable(GenerateHandlerName(":exception"));
  LocalVariable* stack_trace =
      MakeVariable(GenerateHandlerName(":stack_trace"));
  LocalVariable* catch_context =
      MakeVariable(GenerateHandlerName(":saved_try_context_var"));
  function_scope_->AddVariable(exception);
  function_scope_->AddVariable(stack_trace);
  function_scope_->AddVariable(catch_context);
  builder_->exception_variables_.push_back(exception);
  builder_->stack_trace_variables_.push_back(stack_trace);
  builder_->catch_context_variables_.push_back(catch_context);
}


void ScopeBuilder::AddIteratorVariable() {
  if (builder_->iterator_variables_.size() >= for_in_depth_) return;

  ASSERT(builder_->iterator_variables_.size() == for_in_depth_ - 1);
  LocalVariable* iterator = MakeVariable(GenerateIteratorName());
  function_scope_->AddVariable(iterator);
  builder_->iterator_variables_.push_back(iterator);
}


void ScopeBuilder::LookupVariable(VariableDeclaration* declaration) {
  LocalVariable* variable = builder_->locals_[declaration];
  if (variable == NULL) {
    // We have not seen a declaration of the variable, so it must be the
    // case that we are compiling a nested function and the variable is
    // declared in an outer scope.  In that case, look it up in the scope by
    // name and add it to the variable map to simplify later lookup.
    ASSERT(function_scope_->parent() != NULL);
    const dart::String& name = H.DartSymbol(declaration->name());
    variable = function_scope_->parent()->LookupVariable(name, true);
    ASSERT(variable != NULL);
    builder_->locals_[declaration] = variable;
  }
  if (variable->owner()->function_level() < scope_->function_level()) {
    // We call `LocalScope->CaptureVariable(variable)` in two scenarios for two
    // different reasons:
    //   Scenario 1:
    //       We need to know which variables defined in this function
    //       are closed over by nested closures in order to ensure we will
    //       create a [Context] object of appropriate size and store captured
    //       variables there instead of the stack.
    //   Scenario 2:
    //       We need to find out which variables defined in enclosing functions
    //       are closed over by this function/closure or nested closures. This
    //       is necessary in order to build a fat flattened [ContextScope]
    //       object.
    scope_->CaptureVariable(variable);
  } else {
    ASSERT(variable->owner()->function_level() == scope_->function_level());
  }
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


const dart::String& ScopeBuilder::GenerateIteratorName() {
  char name[64];
  OS::SNPrint(name, 64, ":iterator%d", for_in_depth_ - 1);
  return H.DartSymbol(name);
}


void ScopeBuilder::AddVariable(VariableDeclaration* declaration) {
  // TODO(kmillikin): Handle final and const, including function declarations.
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
  const dart::Function& function = parsed_function->function();

  LocalScope* enclosing_scope = NULL;
  if (function.IsLocalFunction()) {
    enclosing_scope = LocalScope::RestoreOuterScope(
        ContextScope::Handle(Z, function.context_scope()));
  }
  function_scope_ = scope_ = new(Z) LocalScope(enclosing_scope, 0, 0);
  scope_->AddVariable(parsed_function->EnsureExpressionTemp());
  scope_->AddVariable(parsed_function->current_context_var());
  parsed_function->SetNodeSequence(
      new SequenceNode(TokenPosition::kNoSource, scope_));

  switch (function.kind()) {
    case RawFunction::kClosureFunction:
    case RawFunction::kRegularFunction:
    case RawFunction::kGetterFunction:
    case RawFunction::kSetterFunction:
    case RawFunction::kConstructor: {
      FunctionNode* node;
      if (builder_->node_->IsProcedure()) {
        node = Procedure::Cast(builder_->node_)->function();
      } else if (builder_->node_->IsConstructor()) {
        node = Constructor::Cast(builder_->node_)->function();
      } else {
        node = FunctionNode::Cast(builder_->node_);
      }
      int pos = 0;
      if (function.IsClosureFunction()) {
        LocalVariable* variable = MakeVariable(Symbols::ClosureParameter());
        scope_->InsertParameterAt(pos++, variable);
      } else if (!function.is_static()) {
        // We use [is_static] instead of [IsStaticFunction] because the latter
        // returns `false` for constructors.
        dart::Class& klass = dart::Class::Handle(Z, function.Owner());
        Type& klass_type =
            Type::ZoneHandle(Z, Type::NewNonParameterizedType(klass));
        LocalVariable* variable = MakeVariable(Symbols::This(), klass_type);
        scope_->InsertParameterAt(pos++, variable);
        builder_->this_variable_ = variable;
      } else if (function.IsFactory()) {
        LocalVariable* variable = MakeVariable(
            Symbols::TypeArgumentsParameter(), AbstractType::dynamic_type());
        scope_->InsertParameterAt(pos++, variable);
        builder_->type_arguments_variable_ = variable;
      }
      AddParameters(node, pos);

      // We generate a syntethic body for implicit closure functions - which
      // will forward the call to the real function.
      //     -> see BuildGraphOfImplicitClosureFunction
      if (!function.IsImplicitClosureFunction()) {
        builder_->node_->AcceptVisitor(this);
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
        LocalVariable* variable = MakeVariable(Symbols::This(), klass_type);
        scope_->InsertParameterAt(pos++, variable);
        builder_->this_variable_ = variable;
      }
      if (is_setter) {
        setter_value_ = MakeVariable(Symbols::Value());
        scope_->InsertParameterAt(pos++, setter_value_);
      }
      break;
    }
    case RawFunction::kImplicitStaticFinalGetter:
      builder_->node_->AcceptVisitor(this);
      break;
    case RawFunction::kMethodExtractor: {
      // Add a receiver parameter.  Though it is captured, we emit code to
      // explicitly copy it to a fixed offset in a freshly-allocated context
      // instead of using the generic code for regular functions.
      // Therefore, it isn't necessary to mark it as captured here.
      dart::Class& klass = dart::Class::Handle(Z, function.Owner());
      Type& klass_type =
          Type::ZoneHandle(Z, Type::NewNonParameterizedType(klass));
      LocalVariable* variable = MakeVariable(Symbols::This(), klass_type);
      scope_->InsertParameterAt(0, variable);
      builder_->this_variable_ = variable;
      break;
    }
    case RawFunction::kNoSuchMethodDispatcher:
    case RawFunction::kInvokeFieldDispatcher:
      for (int i = 0; i < function.NumParameters(); ++i) {
        LocalVariable* variable = MakeVariable(
            dart::String::ZoneHandle(Z, function.ParameterNameAt(i)));
        scope_->InsertParameterAt(i, variable);
      }
      break;
    default:
      UNREACHABLE();
      break;
  }

  parsed_function->AllocateVariables();
}


void ScopeBuilder::VisitThisExpression(ThisExpression* node) {
  HandleSpecialLoad(&builder_->this_variable_, Symbols::This());
}


void ScopeBuilder::VisitTypeParameterType(TypeParameterType* node) {
  if (builder_->parsed_function_->function().IsFactory()) {
    // The type argument vector is passed as the very first argument to the
    // factory constructor function.
    HandleSpecialLoad(&builder_->type_arguments_variable_,
                      Symbols::TypeArgumentsParameter());
  } else {
    // The type argument vector is stored on the instance object. We therefore
    // need to capture `this`.
    HandleSpecialLoad(&builder_->this_variable_, Symbols::This());
  }
}


void ScopeBuilder::VisitVariableGet(VariableGet* node) {
  LookupVariable(node->variable());
}


void ScopeBuilder::VisitVariableSet(VariableSet* node) {
  LookupVariable(node->variable());
  node->VisitChildren(this);
}


void ScopeBuilder::VisitSuperPropertyGet(SuperPropertyGet* node) {
  HandleSpecialLoad(&builder_->this_variable_, Symbols::This());
  node->VisitChildren(this);
}


void ScopeBuilder::VisitSuperPropertySet(SuperPropertySet* node) {
  HandleSpecialLoad(&builder_->this_variable_, Symbols::This());
  node->VisitChildren(this);
}


void ScopeBuilder::VisitSuperMethodInvocation(SuperMethodInvocation* node) {
  HandleSpecialLoad(&builder_->this_variable_, Symbols::This());
  node->VisitChildren(this);
}


void ScopeBuilder::HandleLocalFunction(TreeNode* parent,
                                       FunctionNode* function) {
  int saved_loop_depth = loop_depth_;
  int saved_handler_depth = handler_depth_;
  int saved_finally_depth = finally_depth_;
  loop_depth_ = handler_depth_ = finally_depth_ = 0;
  ++function_depth_;
  EnterScope(parent);
  if (function_depth_ == 1) {
    FunctionScope function_scope = { function, scope_ };
    builder_->function_scopes_.push_back(function_scope);
  }
  AddParameters(function);
  VisitFunctionNode(function);
  ExitScope();
  --function_depth_;
  loop_depth_ = saved_loop_depth;
  handler_depth_ = saved_handler_depth;
  finally_depth_ = saved_finally_depth;
}


void ScopeBuilder::HandleSpecialLoad(LocalVariable** variable,
                                     const dart::String& symbol) {
  if (function_scope_->parent() != NULL) {
    // We are building the scope tree of a closure function and saw [node]. We
    // lazily populate the variable using the parent function scope.
    if (*variable == NULL) {
      *variable = function_scope_->parent()->LookupVariable(symbol, true);
      ASSERT(*variable != NULL);
      scope_->CaptureVariable(*variable);
    }
  } else if (scope_->function_level() > 0) {
    // We are building the scope tree of the outermost function/method (which
    // includes traversing all the nested closures as well) and [node]
    // appears inside a closure, so we capture it.
    scope_->CaptureVariable(*variable);
  }
}


void ScopeBuilder::VisitFunctionExpression(FunctionExpression* node) {
  HandleLocalFunction(node, node->function());
}


void ScopeBuilder::VisitLet(Let* node) {
  EnterScope(node);
  node->VisitChildren(this);
  ExitScope();
}


void ScopeBuilder::VisitBlock(Block* node) {
  EnterScope(node);
  node->VisitChildren(this);
  ExitScope();
}


void ScopeBuilder::VisitVariableDeclaration(VariableDeclaration* node) {
  AddVariable(node);
  node->VisitChildren(this);
}


void ScopeBuilder::VisitFunctionDeclaration(FunctionDeclaration* node) {
  VisitVariableDeclaration(node->variable());
  HandleLocalFunction(node, node->function());
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
  EnterScope(node);
  List<VariableDeclaration>& variables = node->variables();
  for (int i = 0; i < variables.length(); ++i) {
    VisitVariableDeclaration(variables[i]);
  }
  ++loop_depth_;
  if (node->condition() != NULL) {
    node->condition()->AcceptExpressionVisitor(this);
  }
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
  ++for_in_depth_;
  AddIteratorVariable();
  ++loop_depth_;
  EnterScope(node);
  VisitVariableDeclaration(node->variable());
  node->body()->AcceptStatementVisitor(this);
  ExitScope();
  --loop_depth_;
  --for_in_depth_;
}


void ScopeBuilder::VisitSwitchStatement(SwitchStatement* node) {
  if (builder_->switch_variable_ == NULL) {
    LocalVariable* variable = MakeVariable(Symbols::SwitchExpr());
    function_scope_->AddVariable(variable);
    builder_->switch_variable_ = variable;
  }
  node->VisitChildren(this);
}


void ScopeBuilder::VisitReturnStatement(ReturnStatement* node) {
  if (finally_depth_ > 0 && builder_->finally_return_variable_ == NULL) {
    const dart::String& name = H.DartSymbol(":try_finally_return_value");
    LocalVariable* variable = MakeVariable(name);
    function_scope_->AddVariable(variable);
    builder_->finally_return_variable_ = variable;
  }
  node->VisitChildren(this);
}


void ScopeBuilder::VisitTryCatch(TryCatch* node) {
  node->body()->AcceptStatementVisitor(this);

  ++handler_depth_;
  AddExceptionVariables();
  List<Catch>& catches = node->catches();
  for (int i = 0; i < catches.length(); ++i) {
    Catch* ketch = catches[i];
    EnterScope(ketch);
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
        outer_(builder->breakable_block_),
        destination_(NULL),
        outer_finally_(builder->try_finally_block_),
        context_depth_(builder->context_depth_) {
    builder_->breakable_block_ = this;
  }
  ~BreakableBlock() {
    builder_->breakable_block_ = outer_;
  }

  bool HadJumper() { return destination_ != NULL; }

  JoinEntryInstr* destination() { return destination_; }

  JoinEntryInstr* BreakDestination(LabeledStatement* label,
                                   TryFinallyBlock** outer_finally,
                                   int* context_depth) {
    BreakableBlock* block = builder_->breakable_block_;
    while (block->labeled_statement_ != label) {
      block = block->outer_;
    }
    ASSERT(block != NULL);
    *outer_finally = block->outer_finally_;
    *context_depth = block->context_depth_;
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
  int context_depth_;
};


class SwitchBlock {
 public:
  SwitchBlock(FlowGraphBuilder* builder, SwitchStatement* switch_stmt)
      : builder_(builder),
        outer_(builder->switch_block_),
        outer_finally_(builder->try_finally_block_),
        switch_statement_(switch_stmt),
        context_depth_(builder->context_depth_) {
    builder_->switch_block_ = this;
  }
  ~SwitchBlock() {
    builder_->switch_block_ = outer_;
  }

  bool HadJumper(SwitchCase* switch_case) {
    return destinations_.find(switch_case) != destinations_.end();
  }

  JoinEntryInstr* Destination(SwitchCase* label,
                              TryFinallyBlock** outer_finally = NULL,
                              int* context_depth = NULL) {
    // Find corresponding [SwitchStatement].
    SwitchBlock* block = this;
    while (true) {
      block->EnsureSwitchCaseMapping();
      if (block->Contains(label)) break;
    }

    // Set the outer finally block.
    if (outer_finally != NULL) {
      *outer_finally = block->outer_finally_;
      *context_depth = block->context_depth_;
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
  int context_depth_;
};


class TryFinallyBlock {
 public:
  TryFinallyBlock(FlowGraphBuilder* builder, Statement* finalizer)
      : builder_(builder),
        outer_(builder->try_finally_block_),
        finalizer_(finalizer),
        context_depth_(builder->context_depth_) {
    builder_->try_finally_block_ = this;
  }
  ~TryFinallyBlock() {
    builder_->try_finally_block_ = outer_;
  }

  Statement* finalizer() { return finalizer_; }
  int context_depth() { return context_depth_; }
  TryFinallyBlock* outer() { return outer_; }

 private:
  FlowGraphBuilder* builder_;
  TryFinallyBlock* outer_;
  Statement* finalizer_;
  int context_depth_;
};


class TryCatchBlock {
 public:
  explicit TryCatchBlock(FlowGraphBuilder* builder, int try_handler_index = -1)
      : builder_(builder),
        outer_(builder->try_catch_block_),
        try_index_(try_handler_index) {
    if (try_index_ == -1) try_index_ = builder->AllocateTryIndex();
    builder->try_catch_block_ = this;
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
      outer_(builder->catch_block_),
      exception_var_(exception_var),
      stack_trace_var_(stack_trace_var),
      catch_try_index_(catch_try_index) {
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


const dart::String& TranslationHelper::DartString(const char* content,
                                                  Heap::Space space) {
  return dart::String::ZoneHandle(Z, dart::String::New(content, space));
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

      tmp = dart::String::FromUTF8(mixin_name->buffer(), mixin_name->size());

      partial = dart::String::Concat(amp, partial);
      partial = dart::String::Concat(tmp, partial);

      dil_klass = base_type->klass();
    }

    tmp = dart::String::FromUTF8(
        dil_klass->name()->buffer(), dil_klass->name()->size());

    partial = dart::String::Concat(amp, partial);
    partial = dart::String::Concat(tmp, partial);

    partial = dart::Symbols::New(partial);
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
  temp = dart::String::Concat(temp, Symbols::Dot());
  temp = dart::String::Concat(temp, DartString(method_name));
  return dart::String::ZoneHandle(Z, dart::Symbols::New(temp));
}


dart::RawLibrary* TranslationHelper::LookupLibraryByDilLibrary(
    Library* dil_library) {
  const dart::String& library_name = DartSymbol(dil_library->import_uri());
  ASSERT(!library_name.IsNull());
  dart::RawLibrary* library = dart::Library::LookupLibrary(library_name);
  ASSERT(library != Object::null());
  return library;
}


dart::RawClass* TranslationHelper::LookupClassByDilClass(Class* dil_klass) {
  dart::RawClass* klass = NULL;

  const dart::String& class_name = DartString(dil_klass->name());
  Library* dil_library = Library::Cast(dil_klass->parent());
  dart::Library& library = dart::Library::Handle(Z,
      LookupLibraryByDilLibrary(dil_library));
  klass = library.LookupClassAllowPrivate(class_name);

  ASSERT(klass != Object::null());
  return klass;
}


dart::RawField* TranslationHelper::LookupFieldByDilField(Field* dil_field) {
  TreeNode* node = dil_field->parent();

  dart::Class& klass = dart::Class::Handle(Z);
  if (node->IsClass()) {
    klass = LookupClassByDilClass(Class::Cast(node));
  } else {
    ASSERT(node->IsLibrary());
    dart::Library& library = dart::Library::Handle(
        Z, LookupLibraryByDilLibrary(Library::Cast(node)));
    klass = library.toplevel_class();
  }
  dart::RawField* field = klass.LookupFieldAllowPrivate(
      DartSymbol(dil_field->name()->string()));
  ASSERT(field != Object::null());
  return field;
}


dart::RawFunction* TranslationHelper::LookupStaticMethodByDilProcedure(
    Procedure* procedure) {
  ASSERT(procedure->IsStatic());
  const dart::String& procedure_name = DartProcedureName(procedure);

  // The parent is either a library or a class (in which case the procedure is a
  // static method).
  TreeNode* parent = procedure->parent();
  if (parent->IsClass()) {
    dart::Class& klass = dart::Class::Handle(Z,
        LookupClassByDilClass(Class::Cast(parent)));
    dart::RawFunction* raw_function =
        klass.LookupFunctionAllowPrivate(procedure_name);
    ASSERT(raw_function != Object::null());

    // TODO(kustermann): We can probably get rid of this after no longer using
    // the built-in core libraries.
    dart::Function& function = dart::Function::ZoneHandle(Z, raw_function);
    if (function.IsRedirectingFactory()) {
      ClassFinalizer::ResolveRedirectingFactory(klass, function);
      function = function.RedirectionTarget();
    }
    return function.raw();
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


dart::RawFunction* TranslationHelper::LookupConstructorByDilConstructor(
    Constructor* constructor) {
  Class* dil_klass = Class::Cast(constructor->parent());
  dart::Class& klass = dart::Class::Handle(Z, LookupClassByDilClass(dil_klass));
  return LookupConstructorByDilConstructor(klass, constructor);
}


dart::RawFunction* TranslationHelper::LookupConstructorByDilConstructor(
    const dart::Class& owner, Constructor* constructor) {
  dart::RawFunction* function =
      owner.LookupConstructorAllowPrivate(DartConstructorName(constructor));
  ASSERT(function != Object::null());
  return function;
}


void TranslationHelper::ReportError(const char* format, ...) {
  const Script& null_script = Script::Handle(Z);

  va_list args;
  va_start(args, format);
  Report::MessageV(
      Report::kError, null_script, TokenPosition::kNoSource,
      Report::AtLocation, format, args);
  va_end(args);
  UNREACHABLE();
}


void TranslationHelper::ReportError(const Error& prev_error,
                                    const char* format, ...) {
  const Script& null_script = Script::Handle(Z);

  va_list args;
  va_start(args, format);
  Report::LongJumpV(
      prev_error, null_script, TokenPosition::kNoSource, format, args);
  va_end(args);
  UNREACHABLE();
}


Instance& ConstantEvaluator::EvaluateExpression(Expression* expression) {
  expression->AcceptExpressionVisitor(this);
  // We return a new `ZoneHandle` here on purpose: The intermediate language
  // instructions do not make a copy of the handle, so we do it.
  return dart::Instance::ZoneHandle(Z, result_.raw());
}


Instance& ConstantEvaluator::EvaluateConstructorInvocation(
    ConstructorInvocation* node) {
  VisitConstructorInvocation(node);
  // We return a new `ZoneHandle` here on purpose: The intermediate language
  // instructions do not make a copy of the handle, so we do it.
  return dart::Instance::ZoneHandle(Z, result_.raw());
}


Instance& ConstantEvaluator::EvaluateListLiteral(ListLiteral* node) {
  VisitListLiteral(node);
  // We return a new `ZoneHandle` here on purpose: The intermediate language
  // instructions do not make a copy of the handle, so we do it.
  return dart::Instance::ZoneHandle(Z, result_.raw());
}


Instance& ConstantEvaluator::EvaluateMapLiteral(MapLiteral* node) {
  VisitMapLiteral(node);
  // We return a new `ZoneHandle` here on purpose: The intermediate language
  // instructions do not make a copy of the handle, so we do it.
  return dart::Instance::ZoneHandle(Z, result_.raw());
}


void ConstantEvaluator::VisitBigintLiteral(BigintLiteral* node) {
  const dart::String& value = H.DartString(node->value());
  result_ = Integer::New(value, Heap::kOld);
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitBoolLiteral(BoolLiteral* node) {
  result_ = dart::Bool::Get(node->value()).raw();
}


void ConstantEvaluator::VisitDoubleLiteral(DoubleLiteral* node) {
  result_ = dart::Double::New(H.DartString(node->value()), Heap::kOld);
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitIntLiteral(IntLiteral* node) {
  result_ = dart::Integer::New(node->value(), Heap::kOld);
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitNullLiteral(NullLiteral* node) {
  result_ = dart::Instance::null();
}


void ConstantEvaluator::VisitStringLiteral(StringLiteral* node) {
  result_ = H.DartString(node->value(), Heap::kOld).raw();
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitTypeLiteral(TypeLiteral* node) {
  result_ = T.TranslateType(node->type()).raw();
}


void ConstantEvaluator::VisitSymbolLiteral(SymbolLiteral* node) {
  result_ = H.DartSymbol(node->value()).raw();
}


void ConstantEvaluator::VisitListLiteral(ListLiteral* node) {
  DartType* types[] = { node->type() };
  TypeArguments& type_arguments = *T.TranslateTypeArguments(types, 1);

  int length = node->expressions().length();
  const Array& const_list =
      Array::ZoneHandle(Z, Array::New(length, Heap::kOld));
  const_list.SetTypeArguments(type_arguments);
  for (int i = 0; i < length; i++) {
    const Instance& expression = EvaluateExpression(node->expressions()[i]);
    const_list.SetAt(i, expression);
  }
  const_list.MakeImmutable();
  result_ = Canonicalize(const_list);
}


void ConstantEvaluator::VisitMapLiteral(MapLiteral* node) {
  DartType* types[] = { node->key_type(), node->value_type() };
  TypeArguments& type_arguments = *T.TranslateTypeArguments(types, 2);

  int length = node->entries().length();

  Array& const_kv_array =
      Array::ZoneHandle(Z, Array::New(2 * length, Heap::kOld));
  for (int i = 0; i < length; i++) {
    const_kv_array.SetAt(
        2*i + 0, EvaluateExpression(node->entries()[i]->key()));
    const_kv_array.SetAt(
        2*i + 1, EvaluateExpression(node->entries()[i]->value()));
  }

  const_kv_array.MakeImmutable();
  const_kv_array ^= Canonicalize(const_kv_array);

  const dart::Class& map_class = dart::Class::Handle(Z,
      dart::Library::LookupCoreClass(Symbols::ImmutableMap()));
  ASSERT(!map_class.IsNull());
  ASSERT(map_class.NumTypeArguments() == 2);

  const dart::Field& field = dart::Field::Handle(Z,
      map_class.LookupInstanceFieldAllowPrivate(H.DartSymbol("_kvPairs")));
  ASSERT(!field.IsNull());

  // NOTE: This needs to be kept in sync with `runtime/lib/immutable_map.dart`!
  result_ = Instance::New(map_class, Heap::kOld);
  ASSERT(!result_.IsNull());
  result_.SetTypeArguments(type_arguments);
  result_.SetField(field, const_kv_array);
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitConstructorInvocation(
    ConstructorInvocation* node) {
  // TODO(kustermann): Set correct type/type-arguments
  Arguments* dil_arguments = node->arguments();

  const Function& constructor =
      Function::Handle(Z, H.LookupConstructorByDilConstructor(node->target()));

  Instance* receiver = NULL;
  const TypeArguments* type_arguments = NULL;
  if (constructor.IsFactory()) {
    type_arguments = &TypeArguments::ZoneHandle(Z, TypeArguments::null());
  } else {
    const dart::Class& klass = dart::Class::Handle(Z, constructor.Owner());
    receiver = &Instance::ZoneHandle(Z, Instance::New(klass, Heap::kOld));
  }

  const Object& result = RunFunction(
      constructor, dil_arguments, receiver, type_arguments);
  if (constructor.IsFactory()) {
    // Factories return the new object.
    result_ ^= result.raw();
    result_ = Canonicalize(result_);
  } else {
    ASSERT(!receiver->IsNull());
    result_ = Canonicalize(*receiver);
  }
}


void ConstantEvaluator::VisitMethodInvocation(MethodInvocation* node) {
  // TODO(kustermann): Set correct type/type-arguments
  Arguments* dil_arguments = node->arguments();

  const dart::Instance& receiver = EvaluateExpression(node->receiver());
  dart::Class& klass = dart::Class::Handle(Z,
      isolate_->class_table()->At(receiver.GetClassId()));
  ASSERT(!klass.IsNull());

  // Search the superclass chain for the selector.
  // TODO(kustermann): Are there convenience function in the VM which do this?
  // TODO(kustermann): Can we assume this will never be a no-such-method error?
  dart::Function& function = dart::Function::Handle(Z);
  while (!klass.IsNull()) {
    function = klass.LookupDynamicFunctionAllowPrivate(
        H.DartSymbol(node->name()->string()));
    if (!function.IsNull()) break;
    klass = klass.SuperClass();
  }
  ASSERT(!function.IsNull());

  // Run the method and canonicalize the result.
  const Object& result = RunFunction(function, dil_arguments, &receiver);
  result_ ^= result.raw();
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitStaticGet(StaticGet* node) {
  Member* member = node->target();
  if (member->IsField()) {
    Field* dil_field = Field::Cast(member);
    const dart::Field& field = dart::Field::Handle(Z,
        H.LookupFieldByDilField(dil_field));
    if (field.StaticValue() == Object::sentinel().raw() ||
        field.StaticValue() == Object::transition_sentinel().raw()) {
      field.EvaluateInitializer();
      result_ = field.StaticValue();
      result_ = Canonicalize(result_);
      field.SetStaticValue(result_, true);
    } else {
      result_ = field.StaticValue();
    }
  } else if (member->IsProcedure()) {
    Procedure* procedure = Procedure::Cast(member);
    const Function& target = Function::ZoneHandle(Z,
        H.LookupStaticMethodByDilProcedure(procedure));

    if (procedure->kind() == Procedure::kMethod) {
      ASSERT(procedure->IsStatic());
      Function& closure_function =
          Function::ZoneHandle(Z, target.ImplicitClosureFunction());
      closure_function.set_dil_function(target.dil_function());
      result_ = closure_function.ImplicitStaticClosure();
      result_ = Canonicalize(result_);
    } else if (procedure->kind() == Procedure::kGetter) {
      UNIMPLEMENTED();
    } else {
      UNIMPLEMENTED();
    }
  }
}


void ConstantEvaluator::VisitVariableGet(VariableGet* node) {
  // When we see a [VariableGet] the corresponding [VariableDeclaration] must've
  // been executed already. It therefore must have a constant object associated
  // with it.
  LocalVariable* variable = builder_->LookupVariable(node->variable());
  ASSERT(variable->IsConst());
  result_ = variable->ConstValue()->raw();
}


void ConstantEvaluator::VisitStaticInvocation(StaticInvocation* node) {
  const Function& function = Function::ZoneHandle(Z,
      H.LookupStaticMethodByDilProcedure(node->procedure()));
  const Object& result = RunFunction(function, node->arguments());
  result_ ^= result.raw();
  result_ = Canonicalize(result_);
}


void ConstantEvaluator::VisitStringConcatenation(StringConcatenation* node) {
  int length = node->expressions().length();

  bool all_string = true;
  const Array& strings = Array::Handle(Z, Array::New(length));
  for (int i = 0; i < length; i++) {
    EvaluateExpression(node->expressions()[i]);
    strings.SetAt(i, result_);
    all_string = all_string && result_.IsString();
  }
  if (all_string) {
    result_ = dart::String::ConcatAll(strings, Heap::kOld);
    result_ = Canonicalize(result_);
  } else {
    // Get string interpolation function.
    const dart::Class& cls = dart::Class::Handle(
        Z, dart::Library::LookupCoreClass(Symbols::StringBase()));
    ASSERT(!cls.IsNull());
    const Function& func = Function::Handle(Z, cls.LookupStaticFunction(
        dart::Library::PrivateCoreLibName(Symbols::Interpolate())));
    ASSERT(!func.IsNull());

    // Build argument array to pass to the interpolation function.
    const Array& interpolate_arg = Array::Handle(Z, Array::New(1, Heap::kOld));
    interpolate_arg.SetAt(0, strings);

    // Run and canonicalize.
    const Object& result =
        RunFunction(func, interpolate_arg, Array::null_array());
    result_ = Canonicalize(dart::String::Cast(result));
  }
}


void ConstantEvaluator::VisitConditionalExpression(
    ConditionalExpression* node) {
  EvaluateExpression(node->condition());
  if (Bool::Cast(result_).value()) {
    EvaluateExpression(node->then());
  } else {
    EvaluateExpression(node->otherwise());
  }
}


void ConstantEvaluator::VisitLogicalExpression(LogicalExpression* node) {
  if (node->op() == LogicalExpression::kAnd) {
    EvaluateExpression(node->left());
    if (Bool::Cast(result_).value()) {
      EvaluateExpression(node->right());
    }
  } else if (node->op() == LogicalExpression::kOr) {
    EvaluateExpression(node->left());
    if (!Bool::Cast(result_).value()) {
      EvaluateExpression(node->right());
    }
  } else {
    ASSERT(node->op() == LogicalExpression::kIfNull);
    EvaluateExpression(node->left());
    if (result_.IsNull()) {
      EvaluateExpression(node->right());
    }
  }
}


void ConstantEvaluator::VisitNot(Not* node) {
  EvaluateExpression(node->expression());
  ASSERT(result_.IsBool());
  result_ = Bool::Cast(result_).value()
      ? Bool::False().raw() : Bool::True().raw();
}


RawInstance* ConstantEvaluator::Canonicalize(const Instance& instance) {
  ASSERT(instance.IsInstance() || instance.IsNull());
  if (instance.IsNull()) {
    return instance.raw();
  } else {
    const char* error_str = NULL;
    RawInstance* result = instance.CheckAndCanonicalize(&error_str);
    if (result == Object::null()) {
      H.ReportError("Invalid const object %s", error_str);
    }
    return result;
  }
}


const Object& ConstantEvaluator::RunFunction(const Function& function,
                                             Arguments* dil_arguments,
                                             const Instance* receiver,
                                             const TypeArguments* type_args) {
  ASSERT(!((receiver != NULL) && (type_args != NULL)));
  intptr_t extra_arguments =
      (receiver != NULL ? 1 : 0) + (type_args != NULL ? 1 : 0);

  // Build up arguments.
  const Array& arguments = Array::ZoneHandle(Z,
      Array::New(extra_arguments + dil_arguments->count()));
  const Array& names = Array::ZoneHandle(Z,
      Array::New(dil_arguments->named().length()));
  intptr_t pos = 0;
  if (receiver != NULL) {
    arguments.SetAt(pos++, *receiver);
  }
  if (type_args != NULL) {
    arguments.SetAt(pos++, *type_args);
  }
  for (intptr_t i = 0; i < dil_arguments->positional().length(); i++) {
    EvaluateExpression(dil_arguments->positional()[i]);
    arguments.SetAt(pos++, result_);
  }
  for (intptr_t i = 0; i < dil_arguments->named().length(); i++) {
    NamedExpression* named_expression = dil_arguments->named()[i];
    EvaluateExpression(named_expression->expression());
    arguments.SetAt(pos++, result_);
    names.SetAt(i, H.DartSymbol(named_expression->name()));
  }
  return RunFunction(function, arguments, names);
}


const Object& ConstantEvaluator::RunFunction(const Function& function,
                                             const Array& arguments,
                                             const Array& names) {
  const Array& args_descriptor = Array::Handle(Z,
      ArgumentsDescriptor::New(arguments.Length(), names));
  const Object& result = Object::Handle(Z,
      DartEntry::InvokeFunction(function, arguments, args_descriptor));
  if (result.IsError()) {
    H.ReportError(Error::Cast(result), "error evaluating constant constructor");
  }
  return result;
}


FlowGraphBuilder::FlowGraphBuilder(TreeNode* node,
                                   ParsedFunction* parsed_function,
                                   int first_block_id)
  : zone_(Thread::Current()->zone()),
    translation_helper_(zone_, Thread::Current()->isolate()),
    node_(node),
    parsed_function_(parsed_function),
    ic_data_array_(Z, 0),
    next_block_id_(first_block_id),
    next_function_id_(0),
    context_depth_(0),
    loop_depth_(0),
    handler_depth_(0),
    for_in_depth_(0),
    stack_(NULL),
    pending_argument_count_(0),
    graph_entry_(NULL),
    this_variable_(NULL),
    type_arguments_variable_(NULL),
    switch_variable_(NULL),
    finally_return_variable_(NULL),
    breakable_block_(NULL),
    switch_block_(NULL),
    try_finally_block_(NULL),
    try_catch_block_(NULL),
    next_used_try_index_(0),
    catch_block_(NULL),
    type_translator_(&translation_helper_, &active_class_),
    constant_evaluator_(this, zone_, &translation_helper_, &type_translator_) {}


FlowGraphBuilder::~FlowGraphBuilder() { }


Fragment FlowGraphBuilder::TranslateFinallyFinalizers(
    TryFinallyBlock* outer_finally,
    int target_context_depth) {
  TryFinallyBlock* saved_block = try_finally_block_;
  int saved_depth = context_depth_;

  Fragment instructions;

  // While translating the body of a finalizer we need to set the try-finally
  // block which is active when translating the body.
  while (try_finally_block_ != outer_finally) {
    // Potentially restore the context to what is expected for the finally
    // block.
    instructions += AdjustContextTo(try_finally_block_->context_depth());

    Statement* finalizer = try_finally_block_->finalizer();
    try_finally_block_ = try_finally_block_->outer();

    // This will potentially have exceptional cases as described in
    // [VisitTryFinally] and will handle them.
    instructions += TranslateStatement(finalizer);

    // We only need to make sure that if the finalizer ended normally, we
    // continue towards the next outer try-finally.
    if (!instructions.is_open()) break;
  }

  if (instructions.is_open() && target_context_depth != -1) {
    // A target context depth of -1 indicates that we the code after this
    // will not care about the context chain so we can leave it any way we
    // want after the last finalizer.  That is used when returning.
    instructions += AdjustContextTo(target_context_depth);
  }

  try_finally_block_ = saved_block;
  context_depth_ = saved_depth;

  return instructions;
}


Fragment FlowGraphBuilder::LoadContextAt(int depth) {
  int delta = context_depth_ - depth;
  ASSERT(delta >= 0);
  Fragment instructions = LoadLocal(parsed_function_->current_context_var());
  while (delta-- > 0) {
    instructions += LoadField(Context::parent_offset());
  }
  return instructions;
}


Fragment FlowGraphBuilder::AdjustContextTo(int depth) {
  ASSERT(depth <= context_depth_ && depth >= 0);
  Fragment instructions;
  if (depth < context_depth_) {
    instructions += LoadContextAt(depth);
    instructions += StoreLocal(parsed_function_->current_context_var());
    instructions += Drop();
    context_depth_ = depth;
  }
  return instructions;
}


Fragment FlowGraphBuilder::PushContext(int size) {
  ASSERT(size > 0);
  Fragment instructions = AllocateContext(size);
  LocalVariable* context = MakeTemporary();
  instructions += LoadLocal(context);
  instructions += LoadLocal(parsed_function_->current_context_var());
  instructions += StoreInstanceField(Context::parent_offset());
  instructions += StoreLocal(parsed_function_->current_context_var());
  ++context_depth_;
  return instructions;
}


Fragment FlowGraphBuilder::PopContext() {
  return AdjustContextTo(context_depth_ - 1);
}


Fragment FlowGraphBuilder::AllocateContext(int size) {
  AllocateContextInstr* allocate =
      new(Z) AllocateContextInstr(TokenPosition::kNoSource, size);
  Push(allocate);
  return Fragment(allocate);
}


Fragment FlowGraphBuilder::AllocateObject(const dart::Class& klass,
                                          int argument_count) {
  ArgumentArray arguments = GetArguments(argument_count);
  AllocateObjectInstr* allocate =
      new(Z) AllocateObjectInstr(TokenPosition::kNoSource, klass, arguments);
  Push(allocate);
  return Fragment(allocate);
}


Fragment FlowGraphBuilder::AllocateObject(const dart::Class& klass,
                                          const Function& closure_function) {
  ArgumentArray arguments = new(Z) ZoneGrowableArray<PushArgumentInstr*>(Z, 0);
  AllocateObjectInstr* allocate =
      new(Z) AllocateObjectInstr(TokenPosition::kNoSource, klass, arguments);
  allocate->set_closure_function(closure_function);
  Push(allocate);
  return Fragment(allocate);
}


Fragment FlowGraphBuilder::BooleanNegate() {
  BooleanNegateInstr* negate = new(Z) BooleanNegateInstr(Pop());
  Push(negate);
  return Fragment(negate);
}


Fragment FlowGraphBuilder::StrictCompare() {
  Value* right = Pop();
  Value* left = Pop();
  StrictCompareInstr* compare =
      new(Z) StrictCompareInstr(TokenPosition::kNoSource,
                                Token::kEQ_STRICT,
                                left,
                                right,
                                false);
  Push(compare);
  return Fragment(compare);
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
  Fragment instructions(entry);
  instructions += LoadLocal(CurrentCatchContext());
  instructions += StoreLocal(parsed_function_->current_context_var());
  instructions += Drop();
  return instructions;
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
  body += LoadLocal(parsed_function_->current_context_var());
  body += StoreLocal(catch_context_variables_[handler_depth_]);
  body += Drop();
  body += Goto(entry);
  return Fragment(body.entry, entry);
}


Fragment FlowGraphBuilder::CheckStackOverflow() {
  return Fragment(new(Z) CheckStackOverflowInstr(TokenPosition::kNoSource,
                                                 loop_depth_));
}


Fragment FlowGraphBuilder::CloneContext() {
  Fragment instructions = LoadLocal(parsed_function_->current_context_var());
  instructions <<= new(Z) CloneContextInstr(TokenPosition::kNoSource, Pop());
  return instructions;
}


Fragment FlowGraphBuilder::Constant(const Object& value) {
  ASSERT(value.IsNotTemporaryScopedHandle());
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


Fragment FlowGraphBuilder::ClosureCall(int argument_count,
                                       const Array& argument_names) {
  Value* function = Pop();
  ArgumentArray arguments = GetArguments(argument_count);
  ClosureCallInstr* call = new(Z) ClosureCallInstr(
      function, arguments, argument_names, TokenPosition::kNoSource);
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

  pending_argument_count_ -= 1;

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

  pending_argument_count_ -= 2;

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


Fragment FlowGraphBuilder::LoadField(intptr_t offset) {
  LoadFieldInstr* load =
      new(Z) LoadFieldInstr(Pop(),
                            offset,
                            AbstractType::ZoneHandle(Z),
                            TokenPosition::kNoSource);
  Push(load);
  return Fragment(load);
}


Fragment FlowGraphBuilder::LoadLocal(LocalVariable* variable) {
  Fragment instructions;
  if (variable->is_captured()) {
    instructions += LoadContextAt(variable->owner()->context_level());
    instructions += LoadField(Context::variable_offset(variable->index()));
  } else {
    LoadLocalInstr* load =
        new(Z) LoadLocalInstr(*variable, TokenPosition::kNoSource);
    instructions <<= load;
    Push(load);
  }
  return instructions;
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
  // some constants).
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
  // TODO(kmillikin): Omit store barrier when possible (e.g., storing
  // some constants).
  StoreInstanceFieldInstr* store =
      new(Z) StoreInstanceFieldInstr(field,
                                     Pop(),
                                     value,
                                     kEmitStoreBarrier,
                                     TokenPosition::kNoSource);
  return Fragment(store);
}


Fragment FlowGraphBuilder::StoreInstanceField(intptr_t offset) {
  Value* value = Pop();
  StoreInstanceFieldInstr* store =
      new(Z) StoreInstanceFieldInstr(offset,
                                     Pop(),
                                     value,
                                     kEmitStoreBarrier,
                                     TokenPosition::kNoSource);
  return Fragment(store);
}


Fragment FlowGraphBuilder::StoreLocal(LocalVariable* variable) {
  Fragment instructions;
  if (variable->is_captured()) {
    LocalVariable* value = MakeTemporary();
    instructions += LoadContextAt(variable->owner()->context_level());
    instructions += LoadLocal(value);
    instructions +=
        StoreInstanceField(Context::variable_offset(variable->index()));
  } else {
    StoreLocalInstr* store =
        new(Z) StoreLocalInstr(*variable, Pop(), TokenPosition::kNoSource);
    instructions <<= store;
    Push(store);
  }
  return instructions;
}


Fragment FlowGraphBuilder::StoreStaticField(const dart::Field& field) {
  return Fragment(
      new(Z) StoreStaticFieldInstr(field, Pop(), TokenPosition::kNoSource));
}


Fragment FlowGraphBuilder::StringInterpolate() {
  Value* array = Pop();
  StringInterpolateInstr* interpolate =
      new (Z) StringInterpolateInstr(array, TokenPosition::kNoSource);
  Push(interpolate);
  return Fragment(interpolate);
}


dart::RawFunction* FlowGraphBuilder::LookupMethodByMember(
    Member* target, const dart::String& method_name) {
  Class* dil_klass = Class::Cast(target->parent());
  dart::Class& klass = dart::Class::Handle(Z,
      H.LookupClassByDilClass(dil_klass));

  dart::RawFunction* function = klass.LookupFunctionAllowPrivate(method_name);
  ASSERT(function != Object::null());
  return function;
}


LocalVariable* FlowGraphBuilder::MakeTemporary() {
  char name[64];
  intptr_t index = stack_->definition()->temp_index();
  OS::SNPrint(name, 64, ":temp%" Pd, index);
  // TODO(kmillikin): When types are supported we will need to support
  // closure signature types.
  LocalVariable* variable =
      new(Z) LocalVariable(TokenPosition::kNoSource,
                           H.DartSymbol(name),
                           Object::dynamic_type());
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

  if (function.IsConstructorClosureFunction()) return NULL;

  dart::Class& klass = dart::Class::Handle(zone_,
      parsed_function_->function().Owner());

  // Find out if there is an enclosing dil class (which will be used to resolve
  // type parameters).
  Class* dil_klass = NULL;
  dart::Function& topmost = dart::Function::Handle(Z, function.raw());
  while (topmost.parent_function() != Object::null()) {
    topmost = topmost.parent_function();
  }
  TreeNode* topmost_node = reinterpret_cast<TreeNode*>(topmost.dil_function());
  if (topmost_node != NULL) {
    // Going up the closure->parent chain needs to result in a Procedure or
    // Constructor.
    TreeNode* parent = NULL;
    if (topmost_node->IsProcedure()) {
      parent = Procedure::Cast(topmost_node)->parent();
    } else if (topmost_node->IsConstructor()) {
      parent = Constructor::Cast(topmost_node)->parent();
    } else if (topmost_node->IsField()) {
      parent = Field::Cast(topmost_node)->parent();
    }
    if (parent != NULL && parent->IsClass()) dil_klass = Class::Cast(parent);
  }

  // Mark that we are using [klass]/[dill_klass] as active class.  Resolving of
  // type parameters will get resolved via [dill_klass].
  ActiveClassScope active_class_scope(&active_class_, dil_klass, &klass);

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
      FunctionNode* dil_function = node_->IsProcedure()
          ? Procedure::Cast(node_)->function()
          : FunctionNode::Cast(node_);
      return function.IsImplicitClosureFunction()
          ? BuildGraphOfImplicitClosureFunction(dil_function, function)
          : BuildGraphOfFunction(dil_function);
    }
    case RawFunction::kConstructor: {
      bool is_factory = function.IsFactory();
      if (is_factory) {
        Procedure* procedure = Procedure::Cast(node_);
        return BuildGraphOfFunction(procedure->function(), NULL);
      } else {
        Constructor* constructor = Constructor::Cast(node_);
        return BuildGraphOfFunction(constructor->function(), constructor);
      }
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
    case RawFunction::kMethodExtractor:
      return BuildGraphOfMethodExtractor(function);
    case RawFunction::kNoSuchMethodDispatcher:
      return BuildGraphOfNoSuchMethodDispatcher(function);
    case RawFunction::kInvokeFieldDispatcher:
      return BuildGraphOfInvokeFieldDispatcher(function);
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

  SetupDefaultParameterValues(function);

  Fragment body(normal_entry);
  body += CheckStackOverflow();
  int context_size =
      parsed_function_->node_sequence()->scope()->num_context_variables();
  if (context_size > 0) {
    body += PushContext(context_size);
    LocalVariable* context = MakeTemporary();

    // Copy captured parameters from the stack into the context.
    LocalScope* scope = parsed_function_->node_sequence()->scope();
    int parameter_count = parsed_function_->function().NumParameters();
    int parameter_index = parsed_function_->first_parameter_index();
    for (int i = 0; i < parameter_count; ++i, --parameter_index) {
      LocalVariable* variable = scope->VariableAt(i);
      if (variable->is_captured()) {
        // There is no LocalVariable describing the on-stack parameter so
        // create one directly.
        LocalVariable* parameter =
            new(Z) LocalVariable(TokenPosition::kNoSource,
                                 Symbols::TempParam(),
                                 Object::dynamic_type());
        parameter->set_index(parameter_index);
        // Mark the stack variable so it will be ignored by the code for
        // try/catch.
        parameter->set_is_captured_parameter(true);

        // Copy the parameter from the stack to the context.  Overwrite it
        // with a null constant on the stack so the original value is
        // eligible for garbage collection.
        body += LoadLocal(context);
        body += LoadLocal(parameter);
        body += StoreInstanceField(Context::variable_offset(variable->index()));
        body += NullConstant();
        body += StoreLocal(parameter);
        body += Drop();
      }
    }
    body += Drop();  // The context.
  }
  if (constructor != NULL) {
    Class* dil_klass = Class::Cast(constructor->parent());
    body += TranslateInitializers(dil_klass, &constructor->initializers());
  }

  // The specificaion defines the result of `a == b` to be:
  //
  //   a) if either side is `null` then the result is `identical(a, b)`.
  //   b) else the result is `a.operator==(b)`
  //
  // For user-defined implementations of `operator==` we need therefore
  // implement the handling of a).
  //
  // The default `operator==` implementation in `Object` is implemented in terms
  // of identical (which we assume here!) which means that case a) is actually
  // included in b).  So we just use the normal implementation in the body.
  const Function& f = parsed_function_->function();
  if ((f.NumParameters() == 2) &&
      (f.name() == Symbols::EqualOperator().raw()) &&
      (f.Owner() != I->object_store()->object_class())) {
    LocalVariable* parameter =
        LookupVariable(function->positional_parameters()[0]);

    TargetEntryInstr* null_entry;
    TargetEntryInstr* non_null_entry;

    body += LoadLocal(parameter);
    body += BranchIfNull(&null_entry, &non_null_entry);

    // The argument was `null` and the receiver is not the null class (we only
    // go into this branch for user-defined == operators) so we can return
    // false.
    Fragment null_fragment(null_entry);
    null_fragment += Constant(Bool::False());
    null_fragment += Return();

    body = Fragment(non_null_entry);
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
      dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(dil_field));

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
      dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(dil_field));

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ =
      new(Z) GraphEntryInstr(*parsed_function_, normal_entry,
                             Compiler::kNoOSRDeoptId);

  Fragment body(normal_entry);
  if (dil_field->IsConst()) {
    body += Constant(constant_evaluator_.EvaluateExpression(initializer));
  } else {
    body += TranslateExpression(initializer);
  }

  LocalVariable* field_value = MakeTemporary();
  body += LoadLocal(field_value);
  body += StoreStaticField(field);

  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfMethodExtractor(
    const Function& method) {
  // A method extractor is the implicit getter for a method.
  const Function& function = Function::ZoneHandle(Z,
      method.extracted_method_closure());

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ = new(Z) GraphEntryInstr(*parsed_function_,
                                        normal_entry,
                                        Compiler::kNoOSRDeoptId);
  Fragment body(normal_entry);
  body += CheckStackOverflow();
  // Allocate a closure.
  const dart::Class& closure_class =
      dart::Class::ZoneHandle(Z, I->object_store()->closure_class());
  body += AllocateObject(closure_class, function);
  LocalVariable* closure = MakeTemporary();

  // Allocate a context that closes over `this`.
  body += AllocateContext(1);
  LocalVariable* context = MakeTemporary();

  // Store the function and the context in the closure.
  body += LoadLocal(closure);
  body += Constant(function);
  body += StoreInstanceField(Closure::function_offset());

  body += LoadLocal(closure);
  body += LoadLocal(context);
  body += StoreInstanceField(Closure::context_offset());

  // The context is on top of the operand stack.  Store `this`.  The context
  // doesn't need a parent pointer because it doesn't close over anything
  // else.
  body += LoadLocal(this_variable_);
  body += StoreInstanceField(Context::variable_offset(0));

  // The closure is on top of the operand stack.
  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfImplicitClosureFunction(
    FunctionNode* dil_function,
    const Function& function) {
  const Function& target = Function::ZoneHandle(Z, function.parent_function());

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ = new(Z) GraphEntryInstr(*parsed_function_,
                                        normal_entry,
                                        Compiler::kNoOSRDeoptId);
  SetupDefaultParameterValues(dil_function);

  Fragment body(normal_entry);
  body += CheckStackOverflow();

  // Load all the arguments.
  if (!target.is_static()) {
    // The context has a fixed shape: a single variable which is the
    // closed-over receiver.
    body += LoadLocal(parsed_function_->current_context_var());
    body += LoadField(Context::variable_offset(0));
    body += PushArgument();
  }
  int positional_argument_count =
      dil_function->positional_parameters().length();
  for (int i = 0; i < positional_argument_count; i++) {
    body += LoadLocal(LookupVariable(dil_function->positional_parameters()[i]));
    body += PushArgument();
  }
  int named_argument_count = dil_function->named_parameters().length();
  Array& argument_names = Array::ZoneHandle(Z);
  if (named_argument_count > 0) {
    argument_names = Array::New(named_argument_count);
    for (int i = 0; i < named_argument_count; i++) {
      VariableDeclaration* variable = dil_function->named_parameters()[i];
      body += LoadLocal(LookupVariable(variable));
      body += PushArgument();
      argument_names.SetAt(i, H.DartSymbol(variable->name()));
    }
  }
  // Forward them to the target.
  int argument_count = positional_argument_count + named_argument_count;
  if (!target.is_static()) ++argument_count;
  body += StaticCall(target, argument_count, argument_names);

  // Return the result.
  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfNoSuchMethodDispatcher(
    const Function& function) {
  // This function is specialized for a receiver class, a method name, and
  // the arguments descriptor at a call site.

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ = new(Z) GraphEntryInstr(*parsed_function_,
                                        normal_entry,
                                        Compiler::kNoOSRDeoptId);

  // The backend will expect an array of default values for all the named
  // parameters, even if they are all known to be passed at the call site
  // because the call site matches the arguments descriptor.  Use null for
  // the default values.
  const Array& descriptor_array =
      Array::ZoneHandle(Z, function.saved_args_desc());
  ArgumentsDescriptor descriptor(descriptor_array);
  ZoneGrowableArray<const Instance*>* default_values =
      new ZoneGrowableArray<const Instance*>(Z, descriptor.NamedCount());
  for (int i = 0; i < descriptor.NamedCount(); ++i) {
    default_values->Add(&Object::null_instance());
  }
  parsed_function_->set_default_parameter_values(default_values);

  Fragment body(normal_entry);
  body += CheckStackOverflow();

  // The receiver is the first argument to noSuchMethod, and it is the first
  // argument passed to the dispatcher function.
  LocalScope* scope = parsed_function_->node_sequence()->scope();
  body += LoadLocal(scope->VariableAt(0));
  body += PushArgument();

  // The second argument to noSuchMethod is an invocation mirror.  Push the
  // arguments for allocating the invocation mirror.  First, the name.
  body += Constant(dart::String::ZoneHandle(Z, function.name()));
  body += PushArgument();

  // Second, the arguments descriptor.
  body += Constant(descriptor_array);
  body += PushArgument();

  // Third, an array containing the original arguments.  Create it and fill
  // it in.
  body += Constant(TypeArguments::ZoneHandle(Z, TypeArguments::null()));
  body += IntConstant(descriptor.Count());
  body += CreateArray();
  LocalVariable* array = MakeTemporary();
  for (int i = 0; i < descriptor.Count(); ++i) {
    body += LoadLocal(array);
    body += IntConstant(i);
    body += LoadLocal(scope->VariableAt(i));
    body += StoreIndexed(kArrayCid);
    body += Drop();
  }
  body += PushArgument();

  // Fourth, false indicating this is not a super NoSuchMethod.
  body += Constant(Bool::False());
  body += PushArgument();

  const dart::Class& mirror_class = dart::Class::Handle(Z,
      dart::Library::LookupCoreClass(Symbols::InvocationMirror()));
  ASSERT(!mirror_class.IsNull());
  const Function& allocation_function = Function::ZoneHandle(Z,
      mirror_class.LookupStaticFunction(
          dart::Library::PrivateCoreLibName(
              Symbols::AllocateInvocationMirror())));
  ASSERT(!allocation_function.IsNull());
  body += StaticCall(allocation_function, 4);
  body += PushArgument();  // For the call to noSuchMethod.

  ArgumentsDescriptor two_arguments(
      Array::Handle(Z, ArgumentsDescriptor::New(2)));
  Function& no_such_method = Function::ZoneHandle(Z,
      Resolver::ResolveDynamicForReceiverClass(
          dart::Class::Handle(Z, function.Owner()),
          Symbols::NoSuchMethod(),
          two_arguments));
  if (no_such_method.IsNull()) {
    // If noSuchMethod is not found on the receiver class, call
    // Object.noSuchMethod.
    no_such_method = Resolver::ResolveDynamicForReceiverClass(
        dart::Class::Handle(Z, I->object_store()->object_class()),
        Symbols::NoSuchMethod(),
        two_arguments);
  }
  body += StaticCall(no_such_method, 2);
  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


FlowGraph* FlowGraphBuilder::BuildGraphOfInvokeFieldDispatcher(
    const Function& function) {
  // Find the name of the field we should dispatch to.
  const dart::Class& owner = dart::Class::Handle(Z, function.Owner());
  ASSERT(!owner.IsNull());
  const dart::String& field_name = dart::String::Handle(Z, function.name());
  const dart::String& getter_name = dart::String::ZoneHandle(Z,
      Symbols::New(dart::String::Handle(Z,
          dart::Field::GetterSymbol(field_name))));

  // Determine if this is `class Closure { get call => this; }`
  const dart::Class& closure_class =
      dart::Class::Handle(I->object_store()->closure_class());
  const bool is_closure_call =
      (owner.raw() == closure_class.raw()) &&
      field_name.Equals(Symbols::Call());

  // Set default parameters & construct argument names array.
  //
  // The backend will expect an array of default values for all the named
  // parameters, even if they are all known to be passed at the call site
  // because the call site matches the arguments descriptor.  Use null for
  // the default values.
  const Array& descriptor_array =
      Array::ZoneHandle(Z, function.saved_args_desc());
  ArgumentsDescriptor descriptor(descriptor_array);
  const Array& argument_names =
      Array::ZoneHandle(Z, Array::New(descriptor.NamedCount()));
  ZoneGrowableArray<const Instance*>* default_values =
      new ZoneGrowableArray<const Instance*>(Z, descriptor.NamedCount());
  dart::String& string_handle = dart::String::Handle(Z);
  for (int i = 0; i < descriptor.NamedCount(); ++i) {
    default_values->Add(&Object::null_instance());
    string_handle = descriptor.NameAt(i);
    argument_names.SetAt(i, string_handle);
  }
  parsed_function_->set_default_parameter_values(default_values);

  TargetEntryInstr* normal_entry = BuildTargetEntry();
  graph_entry_ = new(Z) GraphEntryInstr(*parsed_function_,
                                        normal_entry,
                                        Compiler::kNoOSRDeoptId);

  Fragment body(normal_entry);
  body += CheckStackOverflow();

  LocalScope* scope = parsed_function_->node_sequence()->scope();

  LocalVariable* closure = NULL;
  if (is_closure_call) {
    closure = scope->VariableAt(0);

    // The closure itself is the first argument.
    body += LoadLocal(closure);
  } else {
    // Invoke the getter to get the field value.
    body += LoadLocal(scope->VariableAt(0));
    body += PushArgument();
    body += InstanceCall(getter_name, Token::kILLEGAL, 1);
  }

  body += PushArgument();

  // Push all arguments onto the stack.
  intptr_t pos = 1;
  for (; pos < descriptor.PositionalCount(); pos++) {
    body += LoadLocal(scope->VariableAt(pos));
    body += PushArgument();
  }
  for (; pos < descriptor.Count(); pos++) {
    body += LoadLocal(scope->VariableAt(pos));
    body += PushArgument();
  }

  if (is_closure_call) {
    // Lookup the function in the closure.
    body += LoadLocal(closure);
    body += LoadField(Closure::function_offset());

    body += ClosureCall(descriptor.Count(), argument_names);
  } else {
    body += InstanceCall(Symbols::Call(),
                         Token::kILLEGAL,
                         descriptor.Count(),
                         argument_names);
  }

  body += Return();

  return new(Z) FlowGraph(*parsed_function_, graph_entry_, next_block_id_ - 1);
}


void FlowGraphBuilder::SetupDefaultParameterValues(FunctionNode* function) {
  int num_optional_parameters =
      parsed_function_->function().NumOptionalParameters();
  if (num_optional_parameters > 0) {
    ZoneGrowableArray<const Instance*>* default_values =
        new ZoneGrowableArray<const Instance*>(Z, num_optional_parameters);

    if (parsed_function_->function().HasOptionalNamedParameters()) {
      ASSERT(!parsed_function_->function().HasOptionalPositionalParameters());
      for (int i = 0; i < num_optional_parameters; i++) {
        VariableDeclaration* variable = function->named_parameters()[i];
        Instance* default_value;
        if (variable->initializer() != NULL) {
           default_value =
             &constant_evaluator_.EvaluateExpression(variable->initializer());
        } else {
           default_value = &Instance::ZoneHandle(Z, Instance::null());
        }
        default_values->Add(default_value);
      }
    } else {
      ASSERT(parsed_function_->function().HasOptionalPositionalParameters());
      int required = function->required_parameter_count();
      for (int i = 0; i < num_optional_parameters; i++) {
        VariableDeclaration* variable =
            function->positional_parameters()[required + i];
        Instance* default_value;
        if (variable->initializer() != NULL) {
           default_value =
               &constant_evaluator_.EvaluateExpression(variable->initializer());
        } else {
           default_value = &Instance::ZoneHandle(Z, Instance::null());
        }
        default_values->Add(default_value);
      }
    }
    parsed_function_->set_default_parameter_values(default_values);
  }
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
          dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(dil_field));

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
          dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(init->field()));

      // TODO(kustermann): Support FLAG_use_field_guards.
      instructions += LoadLocal(this_variable_);
      instructions += TranslateExpression(init->value());
      instructions += StoreInstanceField(field);
    } else if (initializer->IsSuperInitializer()) {
      SuperInitializer* init = SuperInitializer::Cast(initializer);

      instructions += LoadLocal(this_variable_);
      instructions += PushArgument();

      ASSERT(init->arguments()->types().length() == 0);
      Array& argument_names = Array::ZoneHandle(Z);
      instructions += TranslateArguments(init->arguments(), &argument_names);

      const Function& target = Function::ZoneHandle(Z,
          H.LookupConstructorByDilConstructor(init->target()));
      int argument_count = init->arguments()->count() + 1;
      instructions += StaticCall(target, argument_count, argument_names);
      instructions += Drop();
    } else if (initializer->IsRedirectingInitializer()) {
      RedirectingInitializer* init = RedirectingInitializer::Cast(initializer);

      instructions += LoadLocal(this_variable_);
      instructions += PushArgument();

      ASSERT(init->arguments()->types().length() == 0);
      Array& argument_names = Array::ZoneHandle(Z);
      instructions += TranslateArguments(init->arguments(), &argument_names);

      const Function& target = Function::ZoneHandle(Z,
          H.LookupConstructorByDilConstructor(init->target()));
      int argument_count = init->arguments()->count() + 1;
      instructions += StaticCall(target, argument_count, argument_names);
      instructions += Drop();
    } else {
      UNIMPLEMENTED();
    }
  }
  return instructions;
}


Fragment FlowGraphBuilder::TranslateStatement(Statement* statement) {
#ifdef DEBUG
  int original_context_depth = context_depth_;
#endif
  statement->AcceptStatementVisitor(this);
  DEBUG_ASSERT(context_depth_ == original_context_depth);
  return fragment_;
}


Fragment FlowGraphBuilder::TranslateExpression(Expression* expression)  {
  expression->AcceptExpressionVisitor(this);
  return fragment_;
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
  instructions += AllocateObject(symbol_class, 0);
  LocalVariable* symbol = MakeTemporary();

  instructions += LoadLocal(symbol);
  instructions += PushArgument();
  instructions += Constant(symbol_value);
  instructions += PushArgument();
  instructions += StaticCall(symbol_constructor, 2);
  instructions += Drop();

  fragment_ = instructions;
}


AbstractType& DartTypeTranslator::TranslateType(DartType* node) {
  node->AcceptDartTypeVisitor(this);
  // We return a new `ZoneHandle` here on purpose: The intermediate language
  // instructions do not make a copy of the handle, so we do it.
  return dart::AbstractType::ZoneHandle(Z, result_.raw());
}


AbstractType& DartTypeTranslator::TranslateTypeWithoutFinalization(
    DartType* node) {
  bool saved_finalize = finalize_;
  finalize_ = false;
  AbstractType& result = TranslateType(node);
  finalize_ = saved_finalize;
  return result;
}


void DartTypeTranslator::VisitTypeParameterType(TypeParameterType* node) {
  ASSERT(active_class_->dil_class != NULL);

  List<TypeParameter>& parameters = active_class_->dil_class->type_parameters();
  for (int i = 0; i < parameters.length(); i++) {
    TypeParameter* type_parameter = parameters[i];
    if (node->parameter() == type_parameter) {
      result_ = dart::TypeParameter::New(
          *active_class_->klass,
          i,
          H.DartSymbol(type_parameter->name()),
          Type::Handle(Z, H.isolate()->object_store()->object_type()),
          TokenPosition::kNoSource);  // bound
      result_.SetIsFinalized();
      result_ = result_.Canonicalize();
      return;
    }
  }
  UNREACHABLE();
}


void DartTypeTranslator::VisitInterfaceType(InterfaceType* node) {
  TypeArguments& type_arguments = *TranslateTypeArguments(
      node->type_arguments().raw_array(), node->type_arguments().length());

  const dart::Class& klass = dart::Class::Handle(Z,
      H.LookupClassByDilClass(node->klass()));

  result_ = Type::New(klass, type_arguments, TokenPosition::kNoSource);
  result_.SetIsFinalized();
  result_ = result_.Canonicalize();
}


void DartTypeTranslator::VisitDynamicType(DynamicType* node) {
  result_ = Object::dynamic_type().raw();
}


void DartTypeTranslator::VisitVoidType(VoidType* node) {
  result_ = Object::void_type().raw();
}


TypeArguments* DartTypeTranslator::TranslateTypeArguments(
    DartType** dart_types, int length) {
  bool only_dynamic = true;
  for (int i = 0; i < length; i++) {
    if (!dart_types[i]->IsDynamicType()) {
      only_dynamic = false;
      break;
    }
  }
  TypeArguments& type_arguments =
      TypeArguments::ZoneHandle(Z, TypeArguments::null());
  if (!only_dynamic) {
    type_arguments = TypeArguments::New(length);
    for (int i = 0; i < length; i++) {
      dart_types[i]->AcceptDartTypeVisitor(this);
      type_arguments.SetTypeAt(i, result_);
    }
    type_arguments = type_arguments.Canonicalize();
  }
  return &type_arguments;
}


void FlowGraphBuilder::VisitTypeLiteral(TypeLiteral* node) {
  fragment_ = Fragment(Constant(T.TranslateType(node->type())));
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
        dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(dil_field));
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
    Procedure* procedure = Procedure::Cast(target);
    const Function& target = Function::ZoneHandle(Z,
        H.LookupStaticMethodByDilProcedure(procedure));

    if (procedure->kind() == Procedure::kGetter) {
      fragment_ = StaticCall(target, 0);
    } else if (procedure->kind() == Procedure::kMethod) {
      ASSERT(procedure->IsStatic());
      Function& closure_function =
          Function::ZoneHandle(Z, target.ImplicitClosureFunction());
      closure_function.set_dil_function(target.dil_function());
      const Instance& closure =
          Instance::ZoneHandle(Z, closure_function.ImplicitStaticClosure());
      fragment_ = Constant(closure);
    } else {
      UNIMPLEMENTED();
    }
  }
}


void FlowGraphBuilder::VisitStaticSet(StaticSet* node) {
  Member* target = node->target();
  if (target->IsField()) {
    Field* dil_field = Field::Cast(target);
    const dart::Field& field =
        dart::Field::ZoneHandle(Z, H.LookupFieldByDilField(dil_field));
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
        H.LookupStaticMethodByDilProcedure(procedure));
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
  Function& target = Function::ZoneHandle(Z);
  if (node->target()->IsProcedure()) {
    // This will create a "method extractor" for the for the super method.
    dart::Class& klass = dart::Class::Handle(Z,
        H.LookupClassByDilClass(Class::Cast(node->target()->parent())));
    const dart::String& getter_name = H.DartGetterName(
        node->target()->name()->string());
    target = Resolver::ResolveDynamicAnyArgs(klass, getter_name);
  } else {
    ASSERT(node->target()->IsField());
    const dart::String& getter_name = H.DartGetterName(
        node->target()->name()->string());
    target = LookupMethodByMember(node->target(), getter_name);
    ASSERT(target.IsGetterFunction() || target.IsImplicitGetterFunction());
  }

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
      H.LookupStaticMethodByDilProcedure(node->procedure()));
  int argument_count = node->arguments()->count();

  Array& argument_names = Array::ZoneHandle(Z);
  Fragment instructions;

  bool is_constructor =
      target.kind() == RawFunction::kConstructor &&
      !target.IsFactory();
  LocalVariable* instance_variable = NULL;

  // If we cross the Dil -> VM core library boundary, a [StaticInvocation]
  // can appear, but the thing we're calling is not a static method, but a
  // factory constructor.
  // The `H.LookupStaticmethodByDilProcedure` will potentially resolve to the
  // forwarded constructor.
  // In that case we'll make an instance and pass it as first argument.
  //
  // TODO(kustermann): Get rid of this after we're using our own core
  // libraries.
  if (is_constructor) {
    const dart::Class& klass = dart::Class::ZoneHandle(Z, target.Owner());

    if (klass.NumTypeParameters() > 0) {
      List<DartType>& dil_type_arguments = node->arguments()->types();
      TypeArguments& type_arguments = *T.TranslateTypeArguments(
          dil_type_arguments.raw_array(), dil_type_arguments.length());

      instructions += Constant(type_arguments);
      instructions += PushArgument();
      instructions += AllocateObject(klass, 1);
    } else {
      instructions += AllocateObject(klass, 0);
    }

    instance_variable = MakeTemporary();

    instructions += LoadLocal(instance_variable);
    instructions += PushArgument();
    argument_count++;
  } else {
    // The VM requires currently a TypeArguments object as first parameter for
    // every factory constructor :-/ !
    //
    // TODO(kustermann): Get rid of this after we're using our own core
    // libraries.
    if (target.IsFactory()) {
      argument_count++;

      List<DartType>& dil_type_arguments = node->arguments()->types();

      TypeArguments& type_arguments = *T.TranslateTypeArguments(
          dil_type_arguments.raw_array(), dil_type_arguments.length());

      instructions += Constant(type_arguments);
      instructions += PushArgument();
    } else {
      ASSERT(node->arguments()->types().length() == 0);
    }
  }
  instructions += TranslateArguments(node->arguments(), &argument_names);
  instructions += StaticCall(target, argument_count, argument_names);

  if (is_constructor) {
    // Drop the result of the constructor call and leave [instance_variable] on
    // top-of-stack.
    instructions += Drop();
  }

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitMethodInvocation(MethodInvocation* node) {
  Fragment instructions = TranslateExpression(node->receiver());
  instructions += PushArgument();

  ASSERT(node->arguments()->types().length() == 0);
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

  ASSERT(node->arguments()->types().length() == 0);
  Fragment instructions = LoadLocal(this_variable_);
  instructions += PushArgument();
  instructions += TranslateArguments(node->arguments(), &argument_names);
  fragment_ = instructions + StaticCall(target, argument_count, argument_names);
}


void FlowGraphBuilder::VisitConstructorInvocation(ConstructorInvocation* node) {
  if (node->is_const()) {
    fragment_ =
        Constant(constant_evaluator_.EvaluateConstructorInvocation(node));
    return;
  }

  const dart::Class& klass = dart::Class::ZoneHandle(
      Z, H.LookupClassByDilClass(Class::Cast(node->target()->parent())));

  Fragment instructions;
  if (klass.NumTypeParameters() > 0) {
    List<DartType>& dil_type_arguments = node->arguments()->types();
    TypeArguments& type_arguments = *T.TranslateTypeArguments(
        dil_type_arguments.raw_array(), dil_type_arguments.length());

    instructions += Constant(type_arguments);
    instructions += PushArgument();
    instructions += AllocateObject(klass, 1);
  } else {
    instructions += AllocateObject(klass, 0);
  }
  LocalVariable* variable = MakeTemporary();

  instructions += LoadLocal(variable);
  instructions += PushArgument();

  Array& argument_names = Array::ZoneHandle(Z);
  instructions += TranslateArguments(node->arguments(), &argument_names);

  const Function& target = Function::ZoneHandle(Z,
      H.LookupConstructorByDilConstructor(klass, node->target()));
  int argument_count = node->arguments()->count() + 1;
  instructions += StaticCall(target, argument_count, argument_names);
  fragment_ = instructions + Drop();
}


void FlowGraphBuilder::VisitIsExpression(IsExpression* node) {
  Fragment instructions = TranslateExpression(node->operand());

  // The VM does not like an instanceOf call with a dynamic type. We need to
  // special case this situation.
  const Type& object_type = Type::Handle(Z, Type::ObjectType());
  const AbstractType& type = T.TranslateType(node->type());
  if (type.IsInstantiated() &&
      object_type.IsSubtypeOf(type, NULL, NULL, Heap::kOld)) {
    // Evaluate the expression on the left but igore it's result.
    instructions += Drop();

    // Let condition be always true.
    instructions += Constant(Bool::True());
  } else {
    instructions += PushArgument();

    // FIXME(kustermann):
    instructions += NullConstant();
    instructions += PushArgument();  // Type arguments.

    instructions += Constant(type);
    instructions += PushArgument();  // Type.

    instructions += Constant(Bool::False());
    instructions += PushArgument();  // Negate?.

    instructions +=
        InstanceCall(dart::Library::PrivateCoreLibName(Symbols::_instanceOf()),
                     Token::kIS,
                     4);
  }

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitAsExpression(AsExpression* node) {
  Fragment instructions = TranslateExpression(node->operand());
  instructions += PushArgument();

  // FIXME(kustermann):
  instructions += NullConstant();
  instructions += PushArgument();  // Type arguments.

  instructions += Constant(T.TranslateType(node->type()));
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
    right_fragment += Constant(Bool::True());
    right_fragment += StrictCompare();
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
  List<Expression>& expressions = node->expressions();

  Fragment instructions;

  // The type arguments for CreateArray.
  instructions += Constant(TypeArguments::ZoneHandle(Z));
  instructions += IntConstant(expressions.length());
  instructions += CreateArray();
  LocalVariable* array = MakeTemporary();

  for (int i = 0; i < node->expressions().length(); i++) {
    instructions += LoadLocal(array);
    instructions += IntConstant(i);
    instructions += TranslateExpression(node->expressions()[i]);
    instructions += StoreIndexed(kArrayCid);
    instructions += Drop();
  }

  instructions += StringInterpolate();

  fragment_ = instructions;
}


void FlowGraphBuilder::VisitListLiteral(ListLiteral* node) {
  if (node->is_const()) {
    fragment_ = Constant(constant_evaluator_.EvaluateListLiteral(node));
    return;
  }

  DartType* types[] = { node->type() };
  TypeArguments& type_arguments = *T.TranslateTypeArguments(types, 1);

  // The type argument for the factory call.
  Fragment instructions = Constant(type_arguments);
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
  if (node->is_const()) {
    fragment_ = Constant(constant_evaluator_.EvaluateMapLiteral(node));
    return;
  }

  const dart::Class& map_class = dart::Class::Handle(Z,
      dart::Library::LookupCoreClass(Symbols::Map()));
  const Function& factory_method = Function::ZoneHandle(Z,
      map_class.LookupFactory(
          dart::Library::PrivateCoreLibName(Symbols::MapLiteralFactory())));

  DartType* types[] = { node->key_type(), node->value_type() };
  TypeArguments& type_arguments = *T.TranslateTypeArguments(types, 2);

  // The type argument for the factory call `new Map<K, V>._fromLiteral(List)`.
  Fragment instructions = Constant(type_arguments);
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


void FlowGraphBuilder::VisitFunctionExpression(FunctionExpression* node) {
  fragment_ = TranslateFunctionNode(node->function(), node);
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
  Fragment instructions;

  List<Expression>& positional = node->positional();
  List<NamedExpression>& named = node->named();
  if (named.length() == 0) {
    *argument_names = Array::null();
  } else {
    *argument_names = Array::New(named.length());
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

  int context_size = scopes_[node]->num_context_variables();
  if (context_size > 0) {
    instructions += PushContext(context_size);
    instructions += Drop();
  }

  List<Statement>& statements = node->statements();
  for (int i = 0; i < statements.length(); ++i) {
    instructions += TranslateStatement(statements[i]);
  }

  if (context_size > 0) {
    if (instructions.is_open()) {
      instructions += PopContext();
    } else {
      --context_depth_;
    }
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
    instructions += TranslateFinallyFinalizers(NULL, -1);
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
  LocalVariable* variable = LookupVariable(node);
  Expression* initializer = node->initializer();

  Fragment instructions;
  if (initializer == NULL) {
    instructions += NullConstant();
  } else {
    if (node->IsConst()) {
      const Instance& constant_value =
          constant_evaluator_.EvaluateExpression(initializer);
      variable->SetConstValue(constant_value);
      instructions += Constant(constant_value);
    } else {
      instructions += TranslateExpression(initializer);
    }
  }
  instructions += StoreLocal(variable);
  instructions += Drop();
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitFunctionDeclaration(FunctionDeclaration* node) {
  Fragment instructions = TranslateFunctionNode(node->function(), node);
  instructions += StoreLocal(LookupVariable(node->variable()));
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
  bool captured = false;
  for (int i = 0; i < variables.length(); ++i) {
    captured = captured || LookupVariable(variables[i])->is_captured();
    declarations += TranslateStatement(variables[i]);
  }

  ++loop_depth_;
  Fragment condition = node->condition() == NULL
      ? Constant(Bool::True())
      : TranslateExpression(node->condition());
  TargetEntryInstr* body_entry;
  TargetEntryInstr* loop_exit;
  condition += Branch(&body_entry, &loop_exit);

  Fragment body(body_entry);
  body += TranslateStatement(node->body());

  if (body.is_open()) {
    if (captured) body += CloneContext();
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
  LocalVariable* iterator = iterator_variables_[for_in_depth_];
  instructions += StoreLocal(iterator);
  instructions += Drop();

  ++for_in_depth_;
  ++loop_depth_;
  Fragment condition = LoadLocal(iterator);
  condition += PushArgument();
  condition += InstanceCall(Symbols::MoveNext(), Token::kILLEGAL, 1);
  TargetEntryInstr* body_entry;
  TargetEntryInstr* loop_exit;
  condition += Branch(&body_entry, &loop_exit);

  Fragment body(body_entry);
  int context_size = scopes_[node]->num_context_variables();
  if (context_size > 0) {
    body += PushContext(context_size);
    body += Drop();
  }
  body += LoadLocal(iterator);
  body += PushArgument();
  const dart::String& current_getter = dart::String::ZoneHandle(Z,
      dart::Field::GetterSymbol(Symbols::Current()));
  body += InstanceCall(current_getter, Token::kGET, 1);
  body += StoreLocal(LookupVariable(node->variable()));
  body += Drop();
  body += TranslateStatement(node->body());

  if (context_size > 0) {
    if (body.is_open()) {
      body += PopContext();
    } else {
      --context_depth_;
    }
  }

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

  fragment_ = Fragment(instructions.entry, loop_exit);
  --loop_depth_;
  --for_in_depth_;
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
  int target_context_depth = -1;
  JoinEntryInstr* destination =
      breakable_block_->BreakDestination(node->target(),
                                         &outer_finally,
                                         &target_context_depth);

  Fragment instructions;
  instructions +=
      TranslateFinallyFinalizers(outer_finally, target_context_depth);
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
      block.Destination(switch_case);
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

      // Evaluate the conditions for the default [SwitchCase] just for the
      // purpose of potentially triggering a compile-time error.
      for (intptr_t k = 0; k < switch_case->expressions().length(); k++) {
        constant_evaluator_.EvaluateExpression(switch_case->expressions()[k]);
      }

      if (block.HadJumper(switch_case)) {
        // There are several branches to the body, so we will make a goto to
        // the join block (and prepend a join instruction to the real body).
        JoinEntryInstr* join = block.Destination(switch_case);
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

        current_instructions += Constant(constant_evaluator_.EvaluateExpression(
            switch_case->expressions()[j]));
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
          JoinEntryInstr* join = block.Destination(switch_case);
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
  int target_context_depth = -1;
  JoinEntryInstr* entry =
      switch_block_->Destination(node->target(),
                                 &outer_finally,
                                 &target_context_depth);

  Fragment instructions;
  instructions +=
      TranslateFinallyFinalizers(outer_finally, target_context_depth);
  if (instructions.is_open()) {
    instructions += Goto(entry);
  }
  fragment_ = instructions;
}


void FlowGraphBuilder::VisitAssertStatement(AssertStatement* node) {
  if (!I->asserts()) {
    fragment_ = Fragment();
    return;
  }

  TargetEntryInstr* then;
  TargetEntryInstr* otherwise;

  Fragment instructions;
  instructions += TranslateExpression(node->condition());
  instructions += Branch(&then, &otherwise);

  const dart::Class& klass = dart::Class::ZoneHandle(Z,
      dart::Library::LookupCoreClass(Symbols::AssertionError()));
  ASSERT(!klass.IsNull());
  const dart::Function& constructor = dart::Function::ZoneHandle(Z,
      klass.LookupConstructorAllowPrivate(
        H.DartSymbol("_AssertionError._create")));
  ASSERT(!constructor.IsNull());

  const dart::String& url = H.DartString(
      parsed_function_->function().ToLibNamePrefixedQualifiedCString(),
      Heap::kOld);

  // Create instance of _AssertionError
  Fragment otherwise_fragment(otherwise);
  otherwise_fragment += AllocateObject(klass, 0);
  LocalVariable* instance = MakeTemporary();

  // Call _AssertionError._create constructor.
  otherwise_fragment += LoadLocal(instance);
  otherwise_fragment += PushArgument();  // this

  otherwise_fragment += node->message() != NULL
      ? TranslateExpression(node->message())
      : Constant(H.DartString("<no message>", Heap::kOld));
  otherwise_fragment += PushArgument();  // message

  otherwise_fragment += Constant(url);
  otherwise_fragment += PushArgument();  // url

  otherwise_fragment += IntConstant(0);
  otherwise_fragment += PushArgument();  // line

  otherwise_fragment += IntConstant(0);
  otherwise_fragment += PushArgument();  // column

  otherwise_fragment += StaticCall(constructor, 5);
  otherwise_fragment += Drop();

  // Throw _AssertionError exception.
  otherwise_fragment += PushArgument();
  otherwise_fragment += ThrowException();
  otherwise_fragment += Drop();

  fragment_ = Fragment(instructions.entry, then);
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

    int context_size = scopes_[catch_clause]->num_context_variables();
    if (context_size > 0) {
      catch_handler_body += PushContext(context_size);
      catch_handler_body += Drop();
    }

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
    AbstractType* type_guard = NULL;
    if (catch_clause->guard() != NULL &&
        !catch_clause->guard()->IsDynamicType()) {
      type_guard = &T.TranslateType(catch_clause->guard());
      handler_types.SetAt(i, *type_guard);
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

    if (context_size > 0) {
      if (catch_handler_body.is_open()) {
        catch_handler_body += PopContext();
      } else {
        --context_depth_;
      }
    }

    if (type_guard != NULL) {
      catch_body += LoadLocal(CurrentException());
      catch_body += PushArgument();  // exception
      catch_body += NullConstant();
      catch_body += PushArgument();  // type arguments
      catch_body += Constant(*type_guard);
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


Fragment FlowGraphBuilder::TranslateFunctionNode(FunctionNode* node,
                                                 TreeNode* parent) {
  // The VM has a per-isolate table of functions indexed by the enclosing
  // function and token position.  We don't have token positions, so we've
  // simply numbered the immediately-nested functions with respect to the
  // parent.
  Function& function = Function::ZoneHandle(Z);
  for (unsigned i = 0; i < function_scopes_.size(); ++i) {
    if (function_scopes_[i].function != node) continue;

    function = I->LookupClosureFunction(parsed_function_->function(),
                                        TokenPosition(i));
    if (function.IsNull()) {
      const dart::String* name;
      if (parent->IsFunctionExpression()) {
        name = &Symbols::AnonymousClosure();
      } else {
        ASSERT(parent->IsFunctionDeclaration());
        name = &H.DartSymbol(
            FunctionDeclaration::Cast(parent)->variable()->name());
      }
      function = Function::NewClosureFunction(*name,
                                              parsed_function_->function(),
                                              TokenPosition(i));
      const ContextScope& context_scope = ContextScope::Handle(Z,
          function_scopes_[i].scope->PreserveOuterScope(context_depth_));
      function.set_context_scope(context_scope);
      function.set_dil_function(reinterpret_cast<intptr_t>(node));
      function.set_result_type(Object::dynamic_type());
      function.set_is_debuggable(false);
      DilReader::SetupFunctionParameters(H,
                                         T,
                                         dart::Class::Handle(Z),
                                         function,
                                         node,
                                         false,  // is_method
                                         true);  // is_closure
      I->AddClosureFunction(function);
    }
    break;
  }

  const dart::Class& closure_class =
      dart::Class::ZoneHandle(Z, I->object_store()->closure_class());
  ASSERT(!closure_class.IsNull());
  Fragment instructions = AllocateObject(closure_class, function);
  LocalVariable* closure = MakeTemporary();

  // TODO(kmillikin): Generic closures need type arguments.

  // Store the function and the context in the closure.
  instructions += LoadLocal(closure);
  instructions += Constant(function);
  instructions += StoreInstanceField(Closure::function_offset());

  instructions += LoadLocal(closure);
  instructions += LoadLocal(parsed_function_->current_context_var());
  instructions += StoreInstanceField(Closure::context_offset());

  return instructions;
}


}  // namespace dil
}  // namespace dart
