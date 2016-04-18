// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/dil.h"

namespace dart {

namespace dil {

Node::~Node() {}

TreeNode::~TreeNode() {}
void TreeNode::AcceptVisitor(Visitor* visitor) {
  AcceptTreeVisitor(visitor);
}

Library::~Library() { }
void Library::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitLibrary(this);
}

Class::~Class() {}
void Class::AcceptTreeVisitor(TreeVisitor* visitor) {
  AcceptClassVisitor(visitor);
}

NormalClass::~NormalClass() {}
void NormalClass::AcceptClassVisitor(ClassVisitor* visitor) {
  visitor->VisitNormalClass(this);
}

MixinClass::~MixinClass() {}
void MixinClass::AcceptClassVisitor(ClassVisitor* visitor) {
  visitor->VisitMixinClass(this);
}

Member::~Member() {}
void Member::AcceptTreeVisitor(TreeVisitor* visitor) {
  AcceptMemberVisitor(visitor);
}

Field::~Field() {}
void Field::AcceptMemberVisitor(MemberVisitor* visitor) {
  visitor->VisitField(this);
}

Constructor::~Constructor() {}
void Constructor::AcceptMemberVisitor(MemberVisitor* visitor) {
  visitor->VisitConstructor(this);
}

Procedure::~Procedure() {}
void Procedure::AcceptMemberVisitor(MemberVisitor* visitor) {
  visitor->VisitProcedure(this);
}

Initializer::~Initializer() {}
void Initializer::AcceptTreeVisitor(TreeVisitor* visitor) {
  AcceptInitializerVisitor(visitor);
}

InvalidInitializer::~InvalidInitializer() {}
void InvalidInitializer::AcceptInitializerVisitor(InitializerVisitor* visitor) {
  visitor->VisitInvalidInitializer(this);
}

FieldInitializer::~FieldInitializer() {}
void FieldInitializer::AcceptInitializerVisitor(InitializerVisitor* visitor) {
  visitor->VisitFieldInitializer(this);
}

SuperInitializer::~SuperInitializer() {}
void SuperInitializer::AcceptInitializerVisitor(InitializerVisitor* visitor) {
  visitor->VisitSuperInitializer(this);
}

RedirectingInitializer::~RedirectingInitializer() {}
void RedirectingInitializer::AcceptInitializerVisitor(InitializerVisitor* visitor) {
  visitor->VisitRedirectingInitializer(this);
}

FunctionNode::~FunctionNode() {}
void FunctionNode::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitFunctionNode(this);
}

Expression::~Expression() {}
void Expression::AcceptTreeVisitor(TreeVisitor* visitor) {
  AcceptExpressionVisitor(visitor);
}

InvalidExpression::~InvalidExpression() {}
void InvalidExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitInvalidExpression(this);
}

VariableGet::~VariableGet() {}
void VariableGet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitVariableGet(this);
}

VariableSet::~VariableSet() {}
void VariableSet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitVariableSet(this);
}

PropertyGet::~PropertyGet() {}
void PropertyGet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitPropertyGet(this);
}

PropertySet::~PropertySet() {}
void PropertySet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitPropertySet(this);
}

SuperPropertyGet::~SuperPropertyGet() {}
void SuperPropertyGet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitSuperPropertyGet(this);
}

SuperPropertySet::~SuperPropertySet() {}
void SuperPropertySet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitSuperPropertySet(this);
}

StaticGet::~StaticGet() {}
void StaticGet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitStaticGet(this);
}

StaticSet::~StaticSet() {}
void StaticSet::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitStaticSet(this);
}

Arguments::~Arguments() {}
void Arguments::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitArguments(this);
}

NamedExpression::~NamedExpression() {}
void NamedExpression::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitNamedExpression(this);
}

MethodInvocation::~MethodInvocation() {}
void MethodInvocation::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitMethodInvocation(this);
}

SuperMethodInvocation::~SuperMethodInvocation() {}
void SuperMethodInvocation::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitSuperMethodInvocation(this);
}

StaticInvocation::~StaticInvocation() {}
void StaticInvocation::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitStaticInvocation(this);
}

ConstructorInvocation::~ConstructorInvocation() {}
void ConstructorInvocation::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitConstructorInvocation(this);
}

Not::~Not() {}
void Not::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitNot(this);
}

LogicalExpression::~LogicalExpression() {}
void LogicalExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitLogicalExpression(this);
}

ConditionalExpression::~ConditionalExpression() {}
void ConditionalExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitConditionalExpression(this);
}

StringConcatenation::~StringConcatenation() {}
void StringConcatenation::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitStringConcatenation(this);
}

IsExpression::~IsExpression() {}
void IsExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitIsExpression(this);
}

AsExpression::~AsExpression() {}
void AsExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitAsExpression(this);
}

BasicLiteral::~BasicLiteral() {}

StringLiteral::~StringLiteral() {}
void StringLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitStringLiteral(this);
}

BigintLiteral::~BigintLiteral() {}
void BigintLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitBigintLiteral(this);
}

IntLiteral::~IntLiteral() {}
void IntLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitIntLiteral(this);
}

DoubleLiteral::~DoubleLiteral() {}
void DoubleLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitDoubleLiteral(this);
}

BoolLiteral::~BoolLiteral() {}
void BoolLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitBoolLiteral(this);
}

NullLiteral::~NullLiteral() {}
void NullLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitNullLiteral(this);
}

SymbolLiteral::~SymbolLiteral() {}
void SymbolLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitSymbolLiteral(this);
}

TypeLiteral::~TypeLiteral() {}
void TypeLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitTypeLiteral(this);
}

ThisExpression::~ThisExpression() {}
void ThisExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitThisExpression(this);
}

Rethrow::~Rethrow() {}
void Rethrow::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitRethrow(this);
}

Throw::~Throw() {}
void Throw::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitThrow(this);
}

ListLiteral::~ListLiteral() {}
void ListLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitListLiteral(this);
}

MapLiteral::~MapLiteral() {}
void MapLiteral::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitMapLiteral(this);
}

MapEntry::~MapEntry() {}
void MapEntry::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitMapEntry(this);
}

AwaitExpression::~AwaitExpression() {}
void AwaitExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitAwaitExpression(this);
}

FunctionExpression::~FunctionExpression() {}
void FunctionExpression::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitFunctionExpression(this);
}

Let::~Let() {}
void Let::AcceptExpressionVisitor(ExpressionVisitor* visitor) {
  visitor->VisitLet(this);
}

Statement::~Statement() {}
void Statement::AcceptTreeVisitor(TreeVisitor* visitor) {
  AcceptStatementVisitor(visitor);
}

InvalidStatement::~InvalidStatement() {}
void InvalidStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitInvalidStatement(this);
}

ExpressionStatement::~ExpressionStatement() {}
void ExpressionStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitExpressionStatement(this);
}

Block::~Block() {}
void Block::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitBlock(this);
}

EmptyStatement::~EmptyStatement() {}
void EmptyStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitEmptyStatement(this);
}

AssertStatement::~AssertStatement() {}
void AssertStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitAssertStatement(this);
}

LabeledStatement::~LabeledStatement() {}
void LabeledStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitLabeledStatement(this);
}

BreakStatement::~BreakStatement() {}
void BreakStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitBreakStatement(this);
}

WhileStatement::~WhileStatement() {}
void WhileStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitWhileStatement(this);
}

DoStatement::~DoStatement() {}
void DoStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitDoStatement(this);
}

ForStatement::~ForStatement() {}
void ForStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitForStatement(this);
}

ForInStatement::~ForInStatement() {}
void ForInStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitForInStatement(this);
}

SwitchStatement::~SwitchStatement() {}
void SwitchStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitSwitchStatement(this);
}

SwitchCase::~SwitchCase() {}
void SwitchCase::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitSwitchCase(this);
}

ContinueSwitchStatement::~ContinueSwitchStatement() {}
void ContinueSwitchStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitContinueSwitchStatement(this);
}

IfStatement::~IfStatement() {}
void IfStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitIfStatement(this);
}

ReturnStatement::~ReturnStatement() {}
void ReturnStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitReturnStatement(this);
}

TryCatch::~TryCatch() {}
void TryCatch::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitTryCatch(this);
}

Catch::~Catch() {}
void Catch::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitCatch(this);
}

TryFinally::~TryFinally() {}
void TryFinally::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitTryFinally(this);
}

YieldStatement::~YieldStatement() {}
void YieldStatement::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitYieldStatement(this);
}

VariableDeclaration::~VariableDeclaration() {}
void VariableDeclaration::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitVariableDeclaration(this);
}

FunctionDeclaration::~FunctionDeclaration() {}
void FunctionDeclaration::AcceptStatementVisitor(StatementVisitor* visitor) {
  visitor->VisitFunctionDeclaration(this);
}

Name::~Name() {}
void Name::AcceptVisitor(Visitor* visitor) {
  visitor->VisitName(this);
}

DartType::~DartType() {}
void DartType::AcceptVisitor(Visitor* visitor) {
  AcceptDartTypeVisitor(visitor);
}

InvalidType::~InvalidType() {}
void InvalidType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitInvalidType(this);
}

DynamicType::~DynamicType() {}
void DynamicType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitDynamicType(this);
}

VoidType::~VoidType() {}
void VoidType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitVoidType(this);
}

InterfaceType::~InterfaceType() {}
void InterfaceType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitInterfaceType(this);
}

FunctionType::~FunctionType() {}
void FunctionType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitFunctionType(this);
}

TypeParameterType::~TypeParameterType() { }
void TypeParameterType::AcceptDartTypeVisitor(DartTypeVisitor* visitor) {
  visitor->VisitTypeParameterType(this);
}

TypeParameter::~TypeParameter() {}
void TypeParameter::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitTypeParameter(this);
}

Program::~Program() {}
void Program::AcceptTreeVisitor(TreeVisitor* visitor) {
  visitor->VisitProgram(this);
}

}  // namespace dil

}  // namespace dart
