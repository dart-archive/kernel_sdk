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
class SwitchBlock;
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

  const dart::String& DartString(const char* content);
  dart::String& DartString(String* content, Heap::Space space = Heap::kNew);
  const dart::String& DartSymbol(const char* content);
  const dart::String& DartSymbol(String* content);

  const dart::String& DartClassName(Class* dil_klass);

  const dart::String& DartConstructorName(Constructor* node);
  const dart::String& DartProcedureName(Procedure* procedure);

  const dart::String& DartSetterName(String* content);
  const dart::String& DartGetterName(String* content);
  const dart::String& DartFactoryName(String* klass_name, String* method_name);

 private:
  dart::Zone* zone_;
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
  virtual void VisitLet(Let* node);

  virtual void VisitEmptyStatement(EmptyStatement* node);
  virtual void VisitBlock(Block* node);
  virtual void VisitReturnStatement(ReturnStatement* node);
  virtual void VisitExpressionStatement(ExpressionStatement* node);
  virtual void VisitVariableDeclaration(VariableDeclaration* node);
  virtual void VisitIfStatement(IfStatement* node);
  virtual void VisitWhileStatement(WhileStatement* node);
  virtual void VisitDoStatement(DoStatement* node);
  virtual void VisitForStatement(ForStatement* node);
  virtual void VisitForInStatement(ForInStatement* node);
  virtual void VisitLabeledStatement(LabeledStatement* node);
  virtual void VisitBreakStatement(BreakStatement* node);
  virtual void VisitSwitchStatement(SwitchStatement* node);
  virtual void VisitContinueSwitchStatement(ContinueSwitchStatement* node);
  virtual void VisitTryFinally(TryFinally* node);

  void AdjustTemporaries(int base);

 private:
  FlowGraph* BuildGraphOfFunction(FunctionNode* node,
                                  Constructor* constructor = NULL);
  FlowGraph* BuildGraphOfFieldAccessor(Field* node);
  FlowGraph* BuildGraphOfStaticFieldInitializer(Field* node);

  Fragment TranslateArguments(Arguments* node, Array* argument_names);
  ArgumentArray GetArguments(int count);

  Fragment TranslateInitializers(Class* dil_klass,
                                 List<Initializer>* initialiers);

  Fragment TranslateStatement(Statement* statement) {
    statement->AcceptStatementVisitor(this);
    return fragment_;
  }

  Fragment TranslateExpression(Expression* expression) {
    expression->AcceptExpressionVisitor(this);
    ASSERT(fragment_.is_open());
    return fragment_;
  }

  Fragment TranslateFinallyFinalizers(TryFinallyBlock* outer_finally);

  Fragment AllocateObject(const dart::Class& klass);
  Fragment BooleanNegate();
  Fragment Boolify();
  Fragment Branch(TargetEntryInstr** then_entry,
                  TargetEntryInstr** otherwise_entry);
  Fragment BranchIfNull(TargetEntryInstr** then_entry,
                        TargetEntryInstr** otherwise_entry);
  Fragment CheckStackOverflow();
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
  Fragment LoadField(const dart::Field& field);
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
  Fragment StoreLocal(LocalVariable* variable);
  Fragment StoreStaticField(const dart::Field& field);

  dart::RawLibrary* LookupLibraryByDilLibrary(Library* library);
  dart::RawClass* LookupClassByDilClass(Class* klass);
  dart::RawField* LookupFieldByDilField(Field* field);
  dart::RawFunction* LookupStaticMethodByDilProcedure(Procedure* procedure);
  dart::RawFunction* LookupConstructorByDilConstructor(
      const dart::Class& owner, Constructor* constructor);
  dart::RawFunction* LookupConstructorByDilConstructor(
      Constructor* constructor);
  dart::RawFunction* LookupMethodByMember(
      Member* target, const dart::String& method_name);

  LocalVariable* MakeTemporary();
  LocalVariable* MakeNonTemporary(const dart::String& symbol);

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

  // The node we are currently compiling (e.g. FunctionNode, Constructor,
  // Field)
  TreeNode* node_;

  ParsedFunction* parsed_function_;
  const ZoneGrowableArray<const ICData*> ic_data_array_;

  int next_block_id_;
  int AllocateBlockId() { return next_block_id_++; }

  std::map<VariableDeclaration*, LocalVariable*> locals_;
  std::vector<LocalVariable*> temporaries_;

  LocalScope* scope_;
  int loop_depth_;
  Fragment fragment_;
  Value* stack_;
  int pending_argument_count_;

  // Only non-NULL for instance functions.
  LocalVariable* this_variable_;

  // A chained list of breakable blocks. Chaining and lookup is done by the
  // [BreakableBlock] class.
  BreakableBlock* breakable_block_;

  // A chained list of switch blocks. Chaining and lookup is done by the
  // [SwitchBlock] class.
  SwitchBlock* switch_block_;

  // A chained list of try-finally blocks. Chaining and lookup is done by the
  // [TryFinallyBlock] class.
  TryFinallyBlock* try_finally_block_;

  friend class BreakableBlock;
  friend class SwitchBlock;
  friend class DartTypeTranslator;
  friend class TryFinallyBlock;
};

}  // namespace dil
}  // namespace dart


#endif  // VM_DIL_TO_IL_H_
