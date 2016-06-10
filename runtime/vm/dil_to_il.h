// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_TO_IL_H_
#define VM_DIL_TO_IL_H_

#include <map>
#include <vector>

#include "vm/dil.h"
#include "vm/flow_graph.h"
#include "vm/intermediate_language.h"

namespace dart {
namespace dil {

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

class TranslationHelper {
 public:
  explicit TranslationHelper(dart::Zone* zone) : zone_(zone) {}

  Zone* zone() { return zone_; }

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
  const dart::String& DartFactoryName(String* klass_name, String* method_name);

  RawLibrary* LookupLibraryByDilLibrary(Library* library);
  RawClass* LookupClassByDilClass(Class* klass);
  RawField* LookupFieldByDilField(Field* field);
  RawFunction* LookupStaticMethodByDilProcedure(Procedure* procedure);
  RawFunction* LookupConstructorByDilConstructor(Constructor* constructor);
  dart::RawFunction* LookupConstructorByDilConstructor(
      const dart::Class& owner, Constructor* constructor);

  void ReportError(const char* format, ...);
  void ReportError(const Error& prev_error, const char* format, ...);

 private:
  dart::Zone* zone_;
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
  ConstantEvaluator(FlowGraphBuilder* builder, Zone* zone, TranslationHelper* h)
      : builder_(builder),
        isolate_(Isolate::Current()),
        zone_(zone),
        translation_helper_(*h),
        result_(dart::Instance::Handle(zone)) {}
  virtual ~ConstantEvaluator() {}

  Instance& EvaluateExpression(Expression* node);
  Instance& EvaluateConstructorInvocation(ConstructorInvocation* node);
  Instance& EvaluateListLiteral(ListLiteral* node);
  Instance& EvaluateMapLiteral(MapLiteral* node);

  virtual void VisitDefaultExpression(Expression* node) { UNREACHABLE(); }

  // virtual void VisitBigintLiteral(BigintLiteral* node);
  virtual void VisitBoolLiteral(BoolLiteral* node);
  virtual void VisitDoubleLiteral(DoubleLiteral* node);
  virtual void VisitIntLiteral(IntLiteral* node);
  virtual void VisitNullLiteral(NullLiteral* node);
  virtual void VisitStringLiteral(StringLiteral* node);

  virtual void VisitListLiteral(ListLiteral* node);
  virtual void VisitMapLiteral(MapLiteral* node);

  virtual void VisitConstructorInvocation(ConstructorInvocation* node);
  virtual void VisitMethodInvocation(MethodInvocation* node);
  virtual void VisitStaticGet(StaticGet* node);
  virtual void VisitVariableGet(VariableGet* node);

  // TODO(kustermann): Figure out what else we need here.

 private:
  RawInstance* Canonicalize(const Instance& instance);

  const Object& RunFunction(const Function& function,
                            const Array& arguments,
                            const Array& names);

  FlowGraphBuilder* builder_;
  Isolate* isolate_;
  Zone* zone_;
  TranslationHelper& translation_helper_;
  Instance& result_;
};


struct FunctionScope {
  FunctionNode* function;
  LocalScope* scope;
};


class FlowGraphBuilder : public TreeVisitor {
 public:
  FlowGraphBuilder(TreeNode* node,
                   ParsedFunction* parsed_function,
                   int first_block_id = 1);
  virtual ~FlowGraphBuilder();

  FlowGraph* BuildGraph();

  virtual void VisitDefaultTreeNode(TreeNode* node) { UNREACHABLE(); }

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
                                      int target_context_depth);

  Fragment TranslateFunctionNode(FunctionNode* node, TreeNode* parent);

  Fragment LoadContextAt(int depth);
  Fragment AdjustContextTo(int depth);

  Fragment PushContext(int size);
  Fragment PopContext();

  Fragment AllocateContext(int size);
  Fragment AllocateObject(const dart::Class& klass, int argument_count);
  Fragment AllocateObject(const dart::Class& klass,
                          const Function& closure_function);
  Fragment BooleanNegate();
  Fragment Boolify();
  Fragment Branch(TargetEntryInstr** then_entry,
                  TargetEntryInstr** otherwise_entry);
  Fragment BranchIfNull(TargetEntryInstr** then_entry,
                        TargetEntryInstr** otherwise_entry);
  Fragment CatchBlockEntry(const Array& handler_types, int handler_index);
  Fragment TryCatch(int try_handler_index);
  Fragment CheckStackOverflow();
  Fragment CloneContext();
  Fragment Constant(const Object& value);
  Fragment CreateArray();
  Fragment Goto(JoinEntryInstr* destination);
  Fragment IntConstant(int64_t value);
  Fragment InstanceCall(const dart::String& name,
                        Token::Kind kind,
                        int argument_count);
  Fragment InstanceCall(const dart::String& name,
                        Token::Kind kind,
                        int argument_count,
                        const Array& argument_names);
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
  Fragment StaticCall(const Function& target, int argument_count);
  Fragment StaticCall(const Function& target,
                      int argument_count,
                      const Array& argument_names);
  Fragment StoreIndexed(intptr_t class_id);
  Fragment StoreInstanceField(const dart::Field& field);
  Fragment StoreInstanceField(intptr_t offset);
  Fragment StoreLocal(LocalVariable* variable);
  Fragment StoreStaticField(const dart::Field& field);
  Fragment StringInterpolate();

  dart::RawFunction* LookupMethodByMember(
      Member* target, const dart::String& method_name);

  LocalVariable* MakeTemporary();
  LocalVariable* MakeNonTemporary(const dart::String& symbol);

  intptr_t CurrentTryIndex();
  intptr_t AllocateTryIndex() { return next_used_try_index_++; }

  void AddVariable(VariableDeclaration* declaration, LocalVariable* variable);
  void AddParameter(VariableDeclaration* declaration,
                    LocalVariable* variable,
                    int pos);
  dart::LocalVariable* LookupVariable(VariableDeclaration* var);

  void SetTempIndex(Definition* definition);

  void Push(Definition* definition);
  Value* Pop();
  Fragment Drop();

  Zone* zone_;
  TranslationHelper translation_helper_;
  ConstantEvaluator constant_evaluator_;

  // The node we are currently compiling (e.g. FunctionNode, Constructor,
  // Field)
  TreeNode* node_;

  ParsedFunction* parsed_function_;
  const ZoneGrowableArray<const ICData*> ic_data_array_;

  int next_block_id_;
  int AllocateBlockId() { return next_block_id_++; }

  int next_function_id_;
  int AllocateFunctionId() { return next_function_id_++; }

  std::map<VariableDeclaration*, LocalVariable*> locals_;
  std::map<TreeNode*, LocalScope*> scopes_;
  std::vector<FunctionScope> function_scopes_;

  int context_depth_;
  int loop_depth_;
  unsigned handler_depth_;
  unsigned for_in_depth_;
  Fragment fragment_;
  Value* stack_;
  int pending_argument_count_;

  GraphEntryInstr* graph_entry_;

  // Only non-NULL for instance functions.
  LocalVariable* this_variable_;

  // Non-NULL when the function contains a switch statement.
  LocalVariable* switch_variable_;

  // Non-NULL when the function contains a return inside a finally block.
  LocalVariable* finally_return_variable_;

  // Variables used in exception handlers, one per exception handler nesting
  // level.
  std::vector<LocalVariable*> exception_variables_;
  std::vector<LocalVariable*> stack_trace_variables_;
  std::vector<LocalVariable*> catch_context_variables_;

  // For-in iterators, one per for-in nesting level.
  std::vector<LocalVariable*> iterator_variables_;

  LocalVariable* CurrentException() {
    return exception_variables_[handler_depth_ - 1];
  }
  LocalVariable* CurrentStackTrace() {
    return stack_trace_variables_[handler_depth_ - 1];
  }
  LocalVariable* CurrentCatchContext() {
    return catch_context_variables_[handler_depth_ - 1];
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
  int next_used_try_index_;

  // A chained list of catch blocks. Chaining and lookup is done by the
  // [CatchBlock] class.
  CatchBlock* catch_block_;

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
