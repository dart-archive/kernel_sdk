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

class FlowGraphBuilder : public TreeVisitor {
 public:
  FlowGraphBuilder(Procedure* procedure,
                   const ParsedFunction& parsed_function,
                   int first_block_id = 1);

  FlowGraph* BuildGraph();

  void VisitDefaultTreeNode(TreeNode* node) { UNREACHABLE(); }

  void VisitNullLiteral(NullLiteral* node);
  void VisitBoolLiteral(BoolLiteral* node);
  void VisitIntLiteral(IntLiteral* node);
  void VisitBigintLiteral(BigintLiteral* node);
  void VisitDoubleLiteral(DoubleLiteral* node);
  void VisitStringLiteral(StringLiteral* node);
  void VisitTypeLiteral(TypeLiteral* node);
  void VisitVariableGet(VariableGet* node);
  void VisitVariableSet(VariableSet* node);
  void VisitStaticGet(StaticGet* node);
  void VisitStaticSet(StaticSet* node);
  void VisitPropertyGet(PropertyGet* node);
  void VisitPropertySet(PropertySet* node);
  void VisitStaticInvocation(StaticInvocation* node);
  void VisitMethodInvocation(MethodInvocation* node);
  void VisitConstructorInvocation(ConstructorInvocation* node);
  void VisitIsExpression(IsExpression* node);
  void VisitAsExpression(AsExpression* node);
  void VisitConditionalExpression(ConditionalExpression* node);
  void VisitLogicalExpression(LogicalExpression* node);

  void VisitBlock(Block* node);
  void VisitReturnStatement(ReturnStatement* node);
  void VisitExpressionStatement(ExpressionStatement* node);
  void VisitVariableDeclaration(VariableDeclaration* node);

  void AdjustTemporaries(int base);

 private:
  Fragment TranslateArguments(Arguments* node, ArgumentArray* arguments);

  Fragment VisitStatement(Statement* statement) {
    statement->AcceptStatementVisitor(this);
    return fragment_;
  }

  Fragment VisitExpression(Expression* expression) {
    expression->AcceptExpressionVisitor(this);
    return fragment_;
  }

  Fragment EmitConstant(const Object& value);

  Fragment EmitStaticCall(const Function& target, ArgumentArray arguments);
  Fragment EmitStaticCall(const Function& target);

  Fragment EmitInstanceCall(const dart::String& name,
                            Token::Kind kind,
                            ArgumentArray arguments);
  Fragment EmitInstanceCall(const dart::String& name,
                            Token::Kind kind,
                            PushArgumentInstr* argument);
  Fragment EmitInstanceCall(const dart::String& name,
                            Token::Kind kind,
                            PushArgumentInstr* argument0,
                            PushArgumentInstr* argument1);
  Fragment EmitInstanceCall(const dart::String& name,
                            Token::Kind kind,
                            PushArgumentInstr* argument0,
                            PushArgumentInstr* argument1,
                            PushArgumentInstr* argument2);
  Fragment EmitInstanceCall(const dart::String& name,
                            Token::Kind kind,
                            PushArgumentInstr* argument0,
                            PushArgumentInstr* argument1,
                            PushArgumentInstr* argument2,
                            PushArgumentInstr* argument3);

  dart::RawString* DartString(String* string,  // NOLINT
                              Heap::Space space = Heap::kNew);
  dart::RawClass* LookupClassByName(const dart::String& name);
  dart::RawClass* LookupClassByName(String* name);
  dart::RawField* LookupFieldByName(const dart::String& name);
  dart::RawField* LookupFieldByName(String* name);
  dart::RawFunction* LookupStaticMethodByName(const dart::String& name);
  dart::RawFunction* LookupStaticMethodByName(String* name);

  PushArgumentInstr* MakeArgument();
  Fragment AddArgumentToList(ZoneGrowableArray<PushArgumentInstr*>* arguments);

  Fragment MakeTemporary(LocalVariable** variable);
  Fragment DropTemporaries(intptr_t count);

  void AddVariable(VariableDeclaration* declaration,
                   LocalVariable* variable);

  void SetTempIndex(Definition* definition);

  void Push(Definition* definition);
  Value* Pop();
  void Drop();

  Zone* zone_;

  Procedure* procedure_;

  const ParsedFunction& parsed_function_;
  const dart::Library& library_;
  const ZoneGrowableArray<const ICData*> ic_data_array_;

  int next_block_id_;
  int AllocateBlockId() { return next_block_id_++; }

  std::map<VariableDeclaration*, LocalVariable*> locals_;
  std::vector<LocalVariable*> temporaries_;

  Fragment fragment_;
  Value* stack_;
  int pending_argument_count_;

  friend class DartTypeTranslator;
};

}  // namespace dil
}  // namespace dart


#endif  // VM_DIL_TO_IL_H_
