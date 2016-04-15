// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_TO_IL_H_
#define VM_DIL_TO_IL_H_

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

  Fragment(Instruction* instruction)
    : entry(instruction), current(instruction) {}

  Fragment(Instruction* entry, Instruction* current)
    : entry(entry), current(current) {}
};

Fragment operator+(const Fragment& first, const Fragment& second);
Fragment operator<<(const Fragment& fragment, Instruction* next);
Fragment operator>>(Instruction* entry, const Fragment& fragment);
Fragment operator!(const Fragment& fragment);

class FlowGraphBuilder : private TreeVisitor {
 public:
  FlowGraphBuilder(Procedure* procedure,
                   const ParsedFunction& parsed_function,
                   Program* program,
                   int first_block_id = 1)
    : zone_(Thread::Current()->zone()),
      procedure_(procedure),
      parsed_function_(parsed_function),
      program_(program),
      next_block_id_(first_block_id),
      stack_(NULL) {
  }

  FlowGraph* BuildGraph();

 private:
  void VisitDefaultTreeNode(TreeNode* node) { UNREACHABLE(); }

  void VisitNullLiteral(NullLiteral* node);
  void VisitStringLiteral(StringLiteral* node);
  void VisitStaticInvocation(StaticInvocation* node);

  void VisitArguments(Arguments* node);

  void VisitBlock(Block* node);
  void VisitReturnStatement(ReturnStatement* node);
  void VisitExpressionStatement(ExpressionStatement* node);

  Fragment VisitStatement(Statement* statement) {
    statement->AcceptStatementVisitor(this);
    return fragment_;
  }

  Fragment VisitExpression(Expression* expression) {
    expression->AcceptExpressionVisitor(this);
    return fragment_;
  }

  Zone* zone_;
  Zone* zone() { return zone_; }

  Procedure* procedure_;
  Procedure* procedure() { return procedure_; }

  const ParsedFunction& parsed_function_;

  Program* program_;

  int next_block_id_;
  int AllocateBlockId() { return next_block_id_++; }

  Fragment fragment_;
  Value* stack_;
  void Push(Definition* definition);
  Value* Pop();
  void Drop();
};

}  // namespace dil
}  // namespace dart


#endif  // VM_DIL_TO_IL_H_
