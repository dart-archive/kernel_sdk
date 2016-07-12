// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_TO_IL_H_
#define VM_DIL_TO_IL_H_

#include "vm/growable_array.h"
#include "vm/hash_map.h"

#include "vm/dil.h"
#include "vm/flow_graph.h"
#include "vm/intermediate_language.h"

namespace dart {
namespace dil {

template<typename K, typename V>
class Map : public DirectChainedHashMap<ProperPointerKeyValueTrait<K, V> > {
 public:
  typedef typename ProperPointerKeyValueTrait<K, V>::Key Key;
  typedef typename ProperPointerKeyValueTrait<K, V>::Value Value;
  typedef typename ProperPointerKeyValueTrait<K, V>::Pair Pair;

  inline void Insert(const Key& key, const Value& value) {
    Pair pair(key, value);
    DirectChainedHashMap<ProperPointerKeyValueTrait<K, V> >::Insert(pair);
  }

  inline V Lookup(const Key& key) {
    Pair* pair =
        DirectChainedHashMap<ProperPointerKeyValueTrait<K, V> >::Lookup(key);
    if (pair == NULL) {
      return V();
    } else {
      return pair->value;
    }
  }
};

class BreakableBlock;
class CatchBlock;
class FlowGraphBuilder;
class SwitchBlock;
class TryCatchBlock;
class TryFinallyBlock;

class Fragment {
 public:
  Instruction* entry;
  Instruction* current;

  Fragment() : entry(NULL), current(NULL) {}

  explicit Fragment(Instruction* instruction)
    : entry(instruction), current(instruction) {}

  Fragment(Instruction* entry, Instruction* current)
    : entry(entry), current(current) {}

  bool is_open() { return entry == NULL || current != NULL; }
  bool is_closed() { return !is_open(); }

  Fragment& operator+=(const Fragment& other);
  Fragment& operator<<=(Instruction* next);

  Fragment closed();
};

Fragment operator+(const Fragment& first, const Fragment& second);
Fragment operator<<(const Fragment& fragment, Instruction* next);

typedef ZoneGrowableArray<PushArgumentInstr*>* ArgumentArray;


class ActiveClass {
 public:
  ActiveClass() : dil_class(NULL), klass(NULL), dil_function(NULL) {}

  // The current enclosing dil class (if available, otherwise NULL).
  Class* dil_class;

  // The current enclosing class (or the library top-level class).
  const dart::Class* klass;

  // The current function.
  FunctionNode* dil_function;
};


class ActiveClassScope {
 public:
  ActiveClassScope(ActiveClass* active_class,
                   Class* dil_class,
                   const dart::Class* klass)
    : active_class_(active_class) {
    saved_dil_class_ = active_class_->dil_class;
    saved_class_ = active_class_->klass;

    active_class_->dil_class = dil_class;
    active_class_->klass = klass;
  }
  ~ActiveClassScope() {
    active_class_->klass = saved_class_;
    active_class_->dil_class = saved_dil_class_;
  }

 private:
  ActiveClass* active_class_;
  Class* saved_dil_class_;
  const dart::Class* saved_class_;
};

class ActiveFunctionScope {
 public:
  ActiveFunctionScope(ActiveClass* active_class,
                      FunctionNode* dil_function)
    : active_class_(active_class) {
    saved_function_ = active_class_->dil_function;
    active_class_->dil_function = dil_function;
  }
  ~ActiveFunctionScope() {
    active_class_->dil_function = saved_function_;
  }

 private:
  ActiveClass* active_class_;
  FunctionNode* saved_function_;
};


class TranslationHelper {
 public:
  TranslationHelper(dart::Thread* thread, dart::Zone* zone, Isolate* isolate)
      : thread_(thread), zone_(zone), isolate_(isolate) {}
  virtual ~TranslationHelper() {}

  Thread* thread() { return thread_; }

  Zone* zone() { return zone_; }

  Isolate* isolate() { return isolate_; }

  const dart::String& DartString(const char* content,
                                 Heap::Space space = Heap::kNew);
  dart::String& DartString(String* content, Heap::Space space = Heap::kNew);
  const dart::String& DartSymbol(const char* content) const;
  const dart::String& DartSymbol(String* content) const;

  const dart::String& DartClassName(Class* dil_klass);

  const dart::String& DartConstructorName(Constructor* node);
  const dart::String& DartProcedureName(Procedure* procedure);

  const dart::String& DartSetterName(String* content);
  const dart::String& DartGetterName(String* content);
  const dart::String& DartInitializerName(String* content);
  const dart::String& DartFactoryName(Class* klass, String* method_name);

  // A subclass overrides these when reading in the DIL program in order to
  // support recursive type expressions (e.g. for "implements X" ...
  // annotations).
  virtual RawLibrary* LookupLibraryByDilLibrary(Library* library);
  virtual RawClass* LookupClassByDilClass(Class* klass);

  RawField* LookupFieldByDilField(Field* field);
  RawFunction* LookupStaticMethodByDilProcedure(Procedure* procedure);
  RawFunction* LookupConstructorByDilConstructor(Constructor* constructor);
  dart::RawFunction* LookupConstructorByDilConstructor(
      const dart::Class& owner, Constructor* constructor);

  dart::Type& GetCanonicalType(const dart::Class& klass);

  void ReportError(const char* format, ...);
  void ReportError(const Error& prev_error, const char* format, ...);

 private:
  dart::Thread* thread_;
  dart::Zone* zone_;
  dart::Isolate* isolate_;
};

// Regarding malformed types:
// The spec says in section "19.1 Static Types" roughly:
//
//   A type T is malformed iff:
//     * T does not denote a type in scope
//     * T refers to a type parameter in a static member
//     * T is a parametrized Type G<T1, ...> and G is malformed
//     * T denotes declarations from multiple imports
//
// Any use of a malformed type gives rise to a static warning.  A malformed
// type is then interpreted as dynamic by the static type checker and the
// runtime unless explicitly specified otherwise.
class DartTypeTranslator : public DartTypeVisitor {
 public:
  DartTypeTranslator(TranslationHelper* helper,
                     ActiveClass* active_class,
                     bool finalize = true)
      : translation_helper_(*helper),
        active_class_(active_class),
        zone_(helper->zone()),
        result_(AbstractType::Handle(helper->zone())),
        finalize_(finalize) {}

  // Can return a malformed type.
  AbstractType& TranslateType(DartType* node);

  // Can return a malformed type.
  AbstractType& TranslateTypeWithoutFinalization(DartType* node);


  virtual void VisitDefaultDartType(DartType* node) { UNREACHABLE(); }

  virtual void VisitInvalidType(InvalidType* node);

  virtual void VisitFunctionType(FunctionType* node);

  virtual void VisitTypeParameterType(TypeParameterType* node);

  virtual void VisitInterfaceType(InterfaceType* node);

  virtual void VisitDynamicType(DynamicType* node);

  virtual void VisitVoidType(VoidType* node);

  // Will return `TypeArguments::null()` in case any of the arguments are
  // malformed.
  const TypeArguments& TranslateInstantiatedTypeArguments(
      dart::Class* receiver_class, DartType** receiver_type_arguments,
      intptr_t length);

  // Will return `TypeArguments::null()` in case any of the arguments are
  // malformed.
  const TypeArguments& TranslateTypeArguments(
      DartType** dart_types, intptr_t length);

  const Type& ReceiverType(const dart::Class& klass);

 private:
  TranslationHelper& translation_helper_;
  ActiveClass* active_class_;
  Zone* zone_;
  AbstractType& result_;

  bool finalize_;
};


// There are several cases when we are compiling constant expressions:
//
//   * constant field initializers:
//      const FieldName = <expr>;
//
//   * constant expressions:
//      const [<expr>, ...]
//      const {<expr> : <expr>, ...}
//      const Constructor(<expr>, ...)
//
//   * constant default parameters:
//      f(a, [b = <expr>])
//      f(a, {b: <expr>})
//
//   * constant values to compare in a [SwitchCase]
//      case <expr>:
//
// In all cases `<expr>` must be recursively evaluated and canonicalized at
// compile-time.
class ConstantEvaluator : public ExpressionVisitor {
 public:
  ConstantEvaluator(FlowGraphBuilder* builder,
                    Zone* zone,
                    TranslationHelper* h,
                    DartTypeTranslator* type_translator)
      : builder_(builder),
        isolate_(Isolate::Current()),
        zone_(zone),
        translation_helper_(*h),
        type_translator_(*type_translator),
        result_(dart::Instance::Handle(zone)) {}
  virtual ~ConstantEvaluator() {}

  Instance& EvaluateExpression(Expression* node);
  Instance& EvaluateConstructorInvocation(ConstructorInvocation* node);
  Instance& EvaluateListLiteral(ListLiteral* node);
  Instance& EvaluateMapLiteral(MapLiteral* node);

  virtual void VisitDefaultExpression(Expression* node) { UNREACHABLE(); }

  virtual void VisitBigintLiteral(BigintLiteral* node);
  virtual void VisitBoolLiteral(BoolLiteral* node);
  virtual void VisitDoubleLiteral(DoubleLiteral* node);
  virtual void VisitIntLiteral(IntLiteral* node);
  virtual void VisitNullLiteral(NullLiteral* node);
  virtual void VisitStringLiteral(StringLiteral* node);
  virtual void VisitSymbolLiteral(SymbolLiteral* node);
  virtual void VisitTypeLiteral(TypeLiteral* node);

  virtual void VisitListLiteral(ListLiteral* node);
  virtual void VisitMapLiteral(MapLiteral* node);

  virtual void VisitConstructorInvocation(ConstructorInvocation* node);
  virtual void VisitMethodInvocation(MethodInvocation* node);
  virtual void VisitStaticGet(StaticGet* node);
  virtual void VisitVariableGet(VariableGet* node);
  virtual void VisitStaticInvocation(StaticInvocation* node);
  virtual void VisitStringConcatenation(StringConcatenation* node);
  virtual void VisitConditionalExpression(ConditionalExpression* node);
  virtual void VisitLogicalExpression(LogicalExpression* node);
  virtual void VisitNot(Not* node);

 private:
  RawInstance* Canonicalize(const Instance& instance);

  const Object& RunFunction(const Function& function,
                            Arguments* arguments,
                            const Instance* receiver = NULL,
                            const TypeArguments* type_args = NULL);

  const Object& RunFunction(const Function& function,
                            const Array& arguments,
                            const Array& names);

  FlowGraphBuilder* builder_;
  Isolate* isolate_;
  Zone* zone_;
  TranslationHelper& translation_helper_;
  DartTypeTranslator& type_translator_;

  Instance& result_;
};


struct FunctionScope {
  FunctionNode* function;
  LocalScope* scope;
};


class ScopeBuildingResult : public ZoneAllocated {
 public:
  Map<VariableDeclaration, LocalVariable*> locals;
  Map<TreeNode, LocalScope*> scopes;
  GrowableArray<FunctionScope> function_scopes;

  // Only non-NULL for instance functions.
  LocalVariable* this_variable = NULL;

  // Only non-NULL for factory constructor functions.
  LocalVariable* type_arguments_variable = NULL;

  // Non-NULL when the function contains a switch statement.
  LocalVariable* switch_variable = NULL;

  // Non-NULL when the function contains a return inside a finally block.
  LocalVariable* finally_return_variable = NULL;

  // Non-NULL when the function is a setter.
  LocalVariable* setter_value = NULL;

  // Variables used in exception handlers, one per exception handler nesting
  // level.
  GrowableArray<LocalVariable*> exception_variables;
  GrowableArray<LocalVariable*> stack_trace_variables;
  GrowableArray<LocalVariable*> catch_context_variables;

  // For-in iterators, one per for-in nesting level.
  GrowableArray<LocalVariable*> iterator_variables;
};


class ScopeBuilder : public RecursiveVisitor {
 public:
  ScopeBuilder(ParsedFunction* parsed_function, TreeNode* node)
      : result_(NULL),
        parsed_function_(parsed_function),
        node_(node),
        zone_(Thread::Current()->zone()),
        translation_helper_(Thread::Current(), zone_, Isolate::Current()),
        function_scope_(NULL),
        scope_(NULL),
        loop_depth_(0),
        function_depth_(0),
        handler_depth_(0),
        finally_depth_(0),
        for_in_depth_(0),
        name_index_(0) {
  }

  virtual ~ScopeBuilder() {}

  ScopeBuildingResult* BuildScopes();

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

  void AddParameters(FunctionNode* function, intptr_t pos = 0);
  void AddParameter(VariableDeclaration* declaration, intptr_t pos);
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

  ScopeBuildingResult* result_;
  ParsedFunction* parsed_function_;
  TreeNode* node_;

  Zone* zone_;
  TranslationHelper translation_helper_;

  LocalScope* function_scope_;
  LocalScope* scope_;
  intptr_t loop_depth_;
  intptr_t function_depth_;
  unsigned handler_depth_;
  intptr_t finally_depth_;
  unsigned for_in_depth_;

  intptr_t name_index_;
};


class FlowGraphBuilder : public TreeVisitor {
 public:
  FlowGraphBuilder(TreeNode* node,
                   ParsedFunction* parsed_function,
                   const ZoneGrowableArray<const ICData*>& ic_data_array,
                   intptr_t osr_id,
                   intptr_t first_block_id = 1);
  virtual ~FlowGraphBuilder();

  FlowGraph* BuildGraph();

  virtual void VisitDefaultTreeNode(TreeNode* node) { UNREACHABLE(); }

  virtual void VisitInvalidExpression(InvalidExpression* node);
  virtual void VisitNullLiteral(NullLiteral* node);
  virtual void VisitBoolLiteral(BoolLiteral* node);
  virtual void VisitIntLiteral(IntLiteral* node);
  virtual void VisitBigintLiteral(BigintLiteral* node);
  virtual void VisitDoubleLiteral(DoubleLiteral* node);
  virtual void VisitStringLiteral(StringLiteral* node);
  virtual void VisitSymbolLiteral(SymbolLiteral* node);
  virtual void VisitTypeLiteral(TypeLiteral* node);
  virtual void VisitVariableGet(VariableGet* node);
  virtual void VisitVariableSet(VariableSet* node);
  virtual void VisitStaticGet(StaticGet* node);
  virtual void VisitStaticSet(StaticSet* node);
  virtual void VisitPropertyGet(PropertyGet* node);
  virtual void VisitPropertySet(PropertySet* node);
  virtual void VisitSuperPropertyGet(SuperPropertyGet* node);
  virtual void VisitSuperPropertySet(SuperPropertySet* node);
  virtual void VisitStaticInvocation(StaticInvocation* node);
  virtual void VisitMethodInvocation(MethodInvocation* node);
  virtual void VisitSuperMethodInvocation(SuperMethodInvocation* node);
  virtual void VisitConstructorInvocation(ConstructorInvocation* node);
  virtual void VisitIsExpression(IsExpression* node);
  virtual void VisitAsExpression(AsExpression* node);
  virtual void VisitConditionalExpression(ConditionalExpression* node);
  virtual void VisitLogicalExpression(LogicalExpression* node);
  virtual void VisitNot(Not* node);
  virtual void VisitThisExpression(ThisExpression* node);
  virtual void VisitStringConcatenation(StringConcatenation* node);
  virtual void VisitListLiteral(ListLiteral* node);
  virtual void VisitMapLiteral(MapLiteral* node);
  virtual void VisitFunctionExpression(FunctionExpression* node);
  virtual void VisitLet(Let* node);
  virtual void VisitThrow(Throw* node);
  virtual void VisitRethrow(Rethrow* node);

  virtual void VisitInvalidStatement(InvalidStatement* node);
  virtual void VisitEmptyStatement(EmptyStatement* node);
  virtual void VisitBlock(Block* node);
  virtual void VisitReturnStatement(ReturnStatement* node);
  virtual void VisitExpressionStatement(ExpressionStatement* node);
  virtual void VisitVariableDeclaration(VariableDeclaration* node);
  virtual void VisitFunctionDeclaration(FunctionDeclaration* node);
  virtual void VisitIfStatement(IfStatement* node);
  virtual void VisitWhileStatement(WhileStatement* node);
  virtual void VisitDoStatement(DoStatement* node);
  virtual void VisitForStatement(ForStatement* node);
  virtual void VisitForInStatement(ForInStatement* node);
  virtual void VisitLabeledStatement(LabeledStatement* node);
  virtual void VisitBreakStatement(BreakStatement* node);
  virtual void VisitSwitchStatement(SwitchStatement* node);
  virtual void VisitContinueSwitchStatement(ContinueSwitchStatement* node);
  virtual void VisitAssertStatement(AssertStatement* node);
  virtual void VisitTryFinally(TryFinally* node);
  virtual void VisitTryCatch(TryCatch* node);

 private:
  FlowGraph* BuildGraphOfFunction(FunctionNode* node,
                                  Constructor* constructor = NULL);
  FlowGraph* BuildGraphOfFieldAccessor(Field* node,
                                       LocalVariable* setter_value);
  FlowGraph* BuildGraphOfStaticFieldInitializer(Field* node);
  FlowGraph* BuildGraphOfMethodExtractor(const Function& method);
  FlowGraph* BuildGraphOfImplicitClosureFunction(FunctionNode* dil_function,
                                                 const Function& function);
  FlowGraph* BuildGraphOfNoSuchMethodDispatcher(const Function& function);
  FlowGraph* BuildGraphOfInvokeFieldDispatcher(const Function& function);

  void SetupDefaultParameterValues(FunctionNode* function);

  TargetEntryInstr* BuildTargetEntry();
  JoinEntryInstr* BuildJoinEntry();

  Fragment TranslateArguments(Arguments* node, Array* argument_names);
  ArgumentArray GetArguments(int count);

  Fragment TranslateInitializers(Class* dil_klass,
                                 List<Initializer>* initialiers);

  Fragment TranslateStatement(Statement* statement);
  Fragment TranslateExpression(Expression* expression);

  Fragment TranslateFinallyFinalizers(TryFinallyBlock* outer_finally,
                                      intptr_t target_context_depth);

  Fragment TranslateFunctionNode(FunctionNode* node, TreeNode* parent);

  Fragment EnterScope(TreeNode* node, bool* new_context = NULL);
  Fragment ExitScope(TreeNode* node);

  Fragment LoadContextAt(int depth);
  Fragment AdjustContextTo(int depth);

  Fragment PushContext(int size);
  Fragment PopContext();

  Fragment LoadInstantiatorTypeArguments();
  Fragment InstantiateTypeArguments(const TypeArguments& type_arguments);
  Fragment TranslateInstantiatedTypeArguments(
      const TypeArguments& type_arguments);

  Fragment AllocateContext(int size);
  Fragment AllocateObject(const dart::Class& klass, intptr_t argument_count);
  Fragment AllocateObject(const dart::Class& klass,
                          const Function& closure_function);
  Fragment BooleanNegate();
  Fragment StrictCompare();
  Fragment Branch(TargetEntryInstr** then_entry,
                  TargetEntryInstr** otherwise_entry);
  Fragment BranchIfNull(TargetEntryInstr** then_entry,
                        TargetEntryInstr** otherwise_entry);
  Fragment CatchBlockEntry(const Array& handler_types, intptr_t handler_index);
  Fragment TryCatch(int try_handler_index);
  Fragment CheckStackOverflow();
  Fragment CloneContext();
  Fragment Constant(const Object& value);
  Fragment CreateArray();
  Fragment Goto(JoinEntryInstr* destination);
  Fragment IntConstant(int64_t value);
  Fragment InstanceCall(const dart::String& name,
                        Token::Kind kind,
                        intptr_t argument_count,
                        intptr_t num_args_checked = 1);
  Fragment InstanceCall(const dart::String& name,
                        Token::Kind kind,
                        intptr_t argument_count,
                        const Array& argument_names,
                        intptr_t num_args_checked = 1);
  Fragment ClosureCall(int argument_count, const Array& argument_names);
  Fragment ThrowException();
  Fragment RethrowException(int catch_try_index);
  Fragment LoadField(const dart::Field& field);
  Fragment LoadField(intptr_t offset);
  Fragment LoadLocal(LocalVariable* variable);
  Fragment InitStaticField(const dart::Field& field);
  Fragment LoadStaticField();
  Fragment NullConstant();
  Fragment PushArgument();
  Fragment Return();
  Fragment StaticCall(const Function& target, intptr_t argument_count);
  Fragment StaticCall(const Function& target,
                      intptr_t argument_count,
                      const Array& argument_names);
  Fragment StoreIndexed(intptr_t class_id);
  Fragment StoreInstanceField(const dart::Field& field);
  Fragment StoreInstanceField(intptr_t offset);
  Fragment StoreLocal(LocalVariable* variable);
  Fragment StoreStaticField(const dart::Field& field);
  Fragment StringInterpolate();
  Fragment ThrowTypeError();
  Fragment ThrowNoSuchMethodError();

  dart::RawFunction* LookupMethodByMember(
      Member* target, const dart::String& method_name);

  LocalVariable* MakeTemporary();
  LocalVariable* MakeNonTemporary(const dart::String& symbol);

  intptr_t CurrentTryIndex();
  intptr_t AllocateTryIndex() { return next_used_try_index_++; }

  void AddVariable(VariableDeclaration* declaration, LocalVariable* variable);
  void AddParameter(VariableDeclaration* declaration,
                    LocalVariable* variable,
                    intptr_t pos);
  dart::LocalVariable* LookupVariable(VariableDeclaration* var);

  void SetTempIndex(Definition* definition);

  void Push(Definition* definition);
  Value* Pop();
  Fragment Drop();

  Token::Kind MethodKind(const dart::String& name);

  Zone* zone_;
  TranslationHelper translation_helper_;

  // The node we are currently compiling (e.g. FunctionNode, Constructor,
  // Field)
  TreeNode* node_;

  ParsedFunction* parsed_function_;
  intptr_t osr_id_;
  const ZoneGrowableArray<const ICData*>& ic_data_array_;

  intptr_t next_block_id_;
  intptr_t AllocateBlockId() { return next_block_id_++; }

  intptr_t next_function_id_;
  intptr_t AllocateFunctionId() { return next_function_id_++; }

  intptr_t context_depth_;
  intptr_t loop_depth_;
  unsigned handler_depth_;
  unsigned for_in_depth_;
  Fragment fragment_;
  Value* stack_;
  intptr_t pending_argument_count_;

  GraphEntryInstr* graph_entry_;

  ScopeBuildingResult* scopes_;

  LocalVariable* CurrentException() {
    return scopes_->exception_variables[handler_depth_ - 1];
  }
  LocalVariable* CurrentStackTrace() {
    return scopes_->stack_trace_variables[handler_depth_ - 1];
  }
  LocalVariable* CurrentCatchContext() {
    return scopes_->catch_context_variables[handler_depth_ - 1];
  }

  // A chained list of breakable blocks. Chaining and lookup is done by the
  // [BreakableBlock] class.
  BreakableBlock* breakable_block_;

  // A chained list of switch blocks. Chaining and lookup is done by the
  // [SwitchBlock] class.
  SwitchBlock* switch_block_;

  // A chained list of try-finally blocks. Chaining and lookup is done by the
  // [TryFinallyBlock] class.
  TryFinallyBlock* try_finally_block_;

  // A chained list of try-catch blocks. Chaining and lookup is done by the
  // [TryCatchBlock] class.
  TryCatchBlock* try_catch_block_;
  intptr_t next_used_try_index_;

  // A chained list of catch blocks. Chaining and lookup is done by the
  // [CatchBlock] class.
  CatchBlock* catch_block_;

  ActiveClass active_class_;
  DartTypeTranslator type_translator_;
  ConstantEvaluator constant_evaluator_;

  friend class BreakableBlock;
  friend class CatchBlock;
  friend class ConstantEvaluator;
  friend class DartTypeTranslator;
  friend class ScopeBuilder;
  friend class SwitchBlock;
  friend class TryCatchBlock;
  friend class TryFinallyBlock;
};

}  // namespace dil
}  // namespace dart


#endif  // VM_DIL_TO_IL_H_
