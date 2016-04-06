// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_H_
#define VM_DIL_H_

#include <stdint.h>

#include "platform/assert.h"
#include "vm/globals.h"

namespace dart {

namespace dil {

class Reader;

// Boxes a value of type `T*` and `delete`s it on destruction.
template<typename T>
class Child {
 public:
  Child() : pointer_(NULL) {}
  explicit Child(T* value) : pointer_(value) {}

  ~Child() { delete pointer_; }

  // Support `Child<T> box = T* obj`.
  T*& operator=(T* value) {
    ASSERT(pointer_ == NULL);
    return pointer_ = value;
  }

  // Implicitly convert `Child<T>` to `T*`.
  operator T*&() { return pointer_; }

 private:
  T* pointer_;
};

// Boxes a value of type `T*` (only used to mark a member as a weak reference).
template<typename T>
class Ref {
 public:
  Ref() : pointer_(NULL) {}
  explicit Ref(T* value) : pointer_(value) {}

  // Support `Ref<T> box = T* obj`.
  T*& operator=(T* value) {
    ASSERT(pointer_ == NULL);
    return pointer_ = value;
  }

  // Implicitly convert `Ref<T>` to `T*`.
  operator T*&() { return pointer_; }

 private:
  T* pointer_;
};

template<typename T>
class List {
 public:
  List() : array_(NULL), length_(-1) { }
  ~List();

  template<typename IT>
  void ReadFrom(Reader* reader);

  template<typename IT>
  void ReadFromStatic(Reader* reader);

  // Extends the array to at least be able to hold [length] elements.
  //
  // Free places will be filled with `NULL` values.
  void EnsureInitialized(int length);

  // Returns element at [index].
  //
  // If the array is not big enough, it will be grown via `EnsureInitialized`.
  // If the element doesn't exist, it will be created via `new IT()`.
  template<typename IT>
  IT* GetOrCreate(int index);

  // Returns element at [index].
  T*& operator[](int index) {
    ASSERT(index < length_);
    return array_[index];
  }

  int length() { return length_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(List);

  Child<T>* array_;
  int length_;
};

template<typename A, typename B>
class Tuple {
 public:
  static Tuple<A, B>* ReadFrom(Reader* reader);

  Tuple(A* a, B* b) : first_(a), second_(b) {}
  ~Tuple() {
    delete first_;
    delete second_;
  }

 private:
  Child<A> first_;
  Child<B> second_;
};

class String {
 public:
  static String* ReadFrom(Reader* reader);

  String(uint8_t* utf8, int length) {
    buffer_ = new uint8_t[length];
    memcpy(buffer_, utf8, length);
  }
  ~String() {
    delete buffer_;
  }

 private:
  uint8_t* buffer_;
  int size_;
};

class Arguments;
class Catch;
class Class;
class Constructor;
class DartType;
class Expression;
class Field;
class FunctionNode;
class Initializer;
class InterfaceType;
class MapEntry;
class Name;
class NamedExpression;
class Procedure;
class Statement;
class SwitchCase;
class TypeParameter;
class VariableDeclaration;

class ExpressionVisitor;
class StatementVisitor;
class MemberVisitor;
class ClassVisitor;
class InitializerVisitor;
class DartTypeVisitor;
class ClassReferenceVisitor;
class MemberReferenceVisitor;
class TreeVisitor;
class Visitor;

class Node {
 public:
  virtual ~Node();

  virtual void AcceptVisitor(Visitor* visitor) = 0;
};

// TODO(kustermann): Maybe we need a [parent] pointer here.
class TreeNode : public Node {
 public:
  virtual ~TreeNode();

  virtual void AcceptVisitor(Visitor* visitor);
  virtual void AcceptTreeVisitor(TreeVisitor* visitor) = 0;
};

class Library : public TreeNode {
 public:
  Library* ReadFrom(Reader* reader);

  Library() : name_(NULL) {}
  virtual ~Library();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  List<Class>& classes() { return classes_; }
  List<Field>& fields() { return fields_; }
  List<Procedure>& procedures() { return procedures_; }

 private:
  Child<String> name_;
  List<Class> classes_;
  List<Field> fields_;
  List<Procedure> procedures_;
};

class Class : public TreeNode {
 public:
  Class* ReadFrom(Reader* reader);

  Class() : name_(NULL), is_abstract_(false) {}
  virtual ~Class();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptClassVisitor(ClassVisitor* visitor) = 0;

  String* name() { return name_; }
  bool is_abstract() { return is_abstract_; }

  virtual List<TypeParameter>& type_parameters() = 0;
  virtual List<InterfaceType>& implemented_classes() = 0;
  virtual List<Field>& fields() = 0;
  virtual List<Constructor>& constructors() = 0;
  virtual List<Procedure>& procedures() = 0;

 private:
  Child<String> name_;
  bool is_abstract_;
};

class NormalClass : public Class {
 public:
  NormalClass* ReadFrom(Reader* reader);

  virtual ~NormalClass();

  virtual void AcceptClassVisitor(ClassVisitor* visitor);

  InterfaceType* super_class() { return super_class_; }
  virtual List<TypeParameter>& type_parameters() { return type_parameters_; }
  virtual List<InterfaceType>& implemented_classes() { return implemented_classes_; }
  virtual List<Field>& fields() { return fields_; }
  virtual List<Constructor>& constructors() { return constructors_; }
  virtual List<Procedure>& procedures() { return procedures_; }

 private:
  List<TypeParameter> type_parameters_;
  Child<InterfaceType> super_class_;
  List<InterfaceType> implemented_classes_;
  List<Field> fields_;
  List<Constructor> constructors_;
  List<Procedure> procedures_;
};

class MixinClass : public Class {
 public:
  MixinClass* ReadFrom(Reader* reader);

  virtual ~MixinClass();

  virtual void AcceptClassVisitor(ClassVisitor* visitor);

  InterfaceType* first() { return first_; }
  InterfaceType* second() { return second_; }
  virtual List<TypeParameter>& type_parameters() { return type_parameters_; }
  virtual List<InterfaceType>& implemented_classes() { return implemented_classes_; }
  virtual List<Field>& fields() { return fields_; }
  virtual List<Constructor>& constructors() { return constructors_; }
  virtual List<Procedure>& procedures() { return procedures_; }

 private:
  List<TypeParameter> type_parameters_;
  Child<InterfaceType> first_;
  Child<InterfaceType> second_;
  List<InterfaceType> implemented_classes_;
  List<Constructor> constructors_;

  // Dummy instances which are empty lists.
  List<Field> fields_;
  List<Procedure> procedures_;
};

class Member : public TreeNode {
 public:
  static Member* ReadFrom(Reader* reader);
  static Class* ReadClassFrom(Reader* reader);

  virtual ~Member();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptMemberVisitor(MemberVisitor* visitor) = 0;

  Name* name() { return name_; }

 protected:
  Child<Name> name_;
};

class Field : public Member {
 public:
  static Field* ReadFrom(Reader* reader);

  Field() {}
  virtual ~Field();

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  word flags() { return flags_; }
  DartType* type() { return type_; }
  Expression* initializer() { return initializer_; }

 private:
  word flags_;
  Child<DartType> type_;
  Child<Expression> initializer_;
};

class Constructor : public Member {
 public:
  static Constructor* ReadFrom(Reader* reader);

  virtual ~Constructor();

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  FunctionNode* function() { return function_; }
  List<Initializer>& initializers() { return initializers_; }

 private:
  Child<FunctionNode> function_;
  List<Initializer> initializers_;
};

class Procedure : public Member {
 public:
  enum Flags {
    kIsStatic,
    kIsAbstract
  };

  // Keep in sync with package:dynamo/lib/ast.dart:ProcedureKind
  enum ProcedureKind {
    kMethod,
    kGetter,
    kSetter,
    kIndexGetter,
    kIndexSetter,
    kOperator,
    kFactory,

    kIncompleteProcedure = 255
  };

  Procedure* ReadFrom(Reader* reader);

  Procedure() : kind_(kIncompleteProcedure), flags_(0), function_(NULL) {}
  virtual ~Procedure();

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  ProcedureKind kind() { return kind_; }
  word flags() { return flags_; }
  FunctionNode* function() { return function_; }

 private:
  ProcedureKind kind_;
  word flags_;
  Child<FunctionNode> function_;
};

class Initializer : public TreeNode {
 public:
  virtual ~Initializer();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor) = 0;
};

class InvalidInitializer : public Initializer {
 public:
  virtual ~InvalidInitializer();
  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);
};

class FieldInitializer : public Initializer {
 public:
  virtual ~FieldInitializer();

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Field* field() { return field_; }
  Expression* value() { return value_; }

 private:
  Child<Field> field_;
  Child<Expression> value_;
};

class SuperInitializer : public Initializer {
 public:
  virtual ~SuperInitializer();

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  Ref<Constructor> target_;
  Child<Arguments> arguments_;
};

class RedirectingInitializer : public Initializer {
 public:
  virtual ~RedirectingInitializer();

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  Ref<Constructor> target_;
  Child<Arguments> arguments_;
};

class FunctionNode : public TreeNode {
 public:
  static FunctionNode* ReadFrom(Reader* reader);

  virtual ~FunctionNode();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<TypeParameter>& type_parameters() { return type_parameters_; }
  int required_parameter_count() { return required_parameter_count_; }
  List<VariableDeclaration>& positional_parameters() { return positional_parameters_; }
  List<VariableDeclaration>& named_parameters() { return named_parameters_; }
  DartType* return_type() { return return_type_; }
  Statement* body() { return body_; }

 private:
  FunctionNode() : required_parameter_count_(-1), return_type_(NULL), body_(NULL) {}

  List<TypeParameter> type_parameters_;
  int required_parameter_count_;
  List<VariableDeclaration> positional_parameters_;
  List<VariableDeclaration> named_parameters_;
  Child<DartType> return_type_;
  Child<Statement> body_;
};

class Expression : public TreeNode {
 public:
  static Expression* ReadFrom(Reader* reader);

  virtual ~Expression();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor) = 0;
};

class InvalidExpression : public Expression {
 public:
  static InvalidExpression* ReadFrom(Reader* reader);

  virtual ~InvalidExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class VariableGet : public Expression {
 public:
  static VariableGet* ReadFrom(Reader* reader);

  virtual ~VariableGet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }

 private:
  Ref<VariableDeclaration> variable_;
};

class VariableSet : public Expression {
 public:
  static VariableSet* ReadFrom(Reader* reader);

  virtual ~VariableSet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* expression() { return expression_; }

 private:
  Ref<VariableDeclaration> variable_;
  Child<Expression> expression_;
};

class PropertyGet : public Expression {
 public:
  static PropertyGet* ReadFrom(Reader* reader);

  virtual ~PropertyGet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }

 private:
  Child<Expression> receiver_;
  Child<Name> name_;
};

class PropertySet : public Expression {
 public:
  static PropertySet* ReadFrom(Reader* reader);

  virtual ~PropertySet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }
  Expression* value() { return value_; }

 private:
  Child<Expression> receiver_;
  Child<Name> name_;
  Child<Expression> value_;
};

class SuperPropertyGet : public Expression {
 public:
  static SuperPropertyGet* ReadFrom(Reader* reader);

  virtual ~SuperPropertyGet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }

 private:
  Ref<Member> target_;
};

class SuperPropertySet : public Expression {
 public:
  static SuperPropertySet* ReadFrom(Reader* reader);

  virtual ~SuperPropertySet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }
  Expression* expression() { return expression_; }

 private:
  Ref<Member> target_;
  Child<Expression> expression_;
};

class StaticGet : public Expression {
 public:
  static StaticGet* ReadFrom(Reader* reader);

  virtual ~StaticGet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }

 private:
  Ref<Member> target_;
};

class StaticSet : public Expression {
 public:
  static StaticSet* ReadFrom(Reader* reader);

  virtual ~StaticSet();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }
  Expression* expression() { return expression_; }

 private:
  Ref<Member> target_;
  Child<Expression> expression_;
};

class Arguments : public TreeNode {
 public:
  static Arguments* ReadFrom(Reader* reader);

  virtual ~Arguments();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<DartType>& types() { return types_; }
  List<Expression>& positional() { return positional_; }
  List<NamedExpression>& named() { return named_; }

 private:
  List<DartType> types_;
  List<Expression> positional_;
  List<NamedExpression> named_;
};

class NamedExpression : public TreeNode {
 public:
  static NamedExpression* ReadFrom(Reader* reader);

  NamedExpression(String* name, Expression* expr) : name_(name), expression_(expr) {}
  virtual ~NamedExpression();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  Expression* expression() { return expression_; }

 private:
  Child<String> name_;
  Child<Expression> expression_;
};

class MethodInvocation : public Expression {
 public:
  static MethodInvocation* ReadFrom(Reader* reader);

  virtual ~MethodInvocation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }
  Arguments* arguments() { return arguments_; }

 private:
  Child<Expression> receiver_;
  Child<Name> name_;
  Child<Arguments> arguments_;
};

class SuperMethodInvocation : public Expression {
 public:
  static SuperMethodInvocation* ReadFrom(Reader* reader);

  virtual ~SuperMethodInvocation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Procedure* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  Ref<Procedure> target_;
  Child<Arguments> arguments_;
};

class StaticInvocation : public Expression {
 public:
  static StaticInvocation* ReadFrom(Reader* reader);

  explicit StaticInvocation(Procedure* procedure, Arguments* args)
      : procedure_(procedure), arguments_(args) {}
  ~StaticInvocation() {
    // NOTE: [procedure_] is a weak handle.
    delete arguments_;
  }

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Procedure* procedure() { return procedure_; }
  Arguments* arguments() { return arguments_; }

 private:
  Ref<Procedure> procedure_;
  Child<Arguments> arguments_;
};

class FunctionInvocation : public Expression {
 public:
  static FunctionInvocation* ReadFrom(Reader* reader);

  virtual ~FunctionInvocation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* function() { return function_; }
  Arguments* arguments() { return arguments_; }

 private:
  Ref<Expression> function_;
  Child<Arguments> arguments_;
};

class ConstructorInvocation : public Expression {
 public:
  static ConstructorInvocation* ReadFrom(Reader* reader);

  virtual ~ConstructorInvocation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }
  bool is_const() { return is_const_; }

 private:
  Ref<Constructor> target_;
  Child<Arguments> arguments_;
  bool is_const_;
};

class Not : public Expression {
 public:
  static Not* ReadFrom(Reader* reader);

  virtual ~Not();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  Child<Expression> expression_;
};

class LogicalExpression : public Expression {
 public:
  static LogicalExpression* ReadFrom(Reader* reader);

  virtual ~LogicalExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* left() { return left_; }
  String* op() { return operator_; }
  Expression* right() { return right_; }

 private:
  Child<Expression> left_;
  Child<String> operator_;
  Child<Expression> right_;
};

class ConditionalExpression : public Expression {
 public:
  static ConditionalExpression* ReadFrom(Reader* reader);

  virtual ~ConditionalExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* condition() { return condition_; }
  Expression* then() { return then_; }
  Expression* otherwise() { return otherwise_; }

 private:
  Child<Expression> condition_;
  Child<Expression> then_;
  Child<Expression> otherwise_;
};

class StringConcatenation : public Expression {
 public:
  static StringConcatenation* ReadFrom(Reader* reader);

  virtual ~StringConcatenation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  List<Expression>& expressions() { return expressions_; }

 private:
  List<Expression> expressions_;
};

class IsExpression : public Expression {
 public:
  static IsExpression* ReadFrom(Reader* reader);

  virtual ~IsExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }
  DartType* type() { return type_; }

 private:
  Child<Expression> operand_;
  Child<DartType> type_;
};

class AsExpression : public Expression {
 public:
  static AsExpression* ReadFrom(Reader* reader);

  virtual ~AsExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }
  DartType* type() { return type_; }

 private:
  Child<Expression> operand_;
  Child<DartType> type_;
};

class BasicLiteral : public Expression {
 public:
  virtual ~BasicLiteral();
};

class StringLiteral : public BasicLiteral {
 public:
  static StringLiteral* ReadFrom(Reader* reader);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  StringLiteral(String* string) : value_(string) {}
  virtual ~StringLiteral();

  String* value() { return value_; }

 private:
  Child<String> value_;
};

class IntLiteral : public BasicLiteral {
 public:
  static IntLiteral* ReadFrom(Reader* reader);

  virtual ~IntLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  int32_t value() { return value_; }

 private:
  int32_t value_; // FIXME: What about arbitrary size integers?
};

class DoubleLiteral : public BasicLiteral {
 public:
  static DoubleLiteral* ReadFrom(Reader* reader);

  virtual ~DoubleLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  double value() { return value_; }

 private:
  double value_;
};

class BoolLiteral : public BasicLiteral {
 public:
  static BoolLiteral* ReadFrom(Reader* reader);

  virtual ~BoolLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool value() { return value_; }

 private:
  bool value_;
};

class NullLiteral : public BasicLiteral {
 public:
  static NullLiteral* ReadFrom(Reader* reader);

  virtual ~NullLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class SymbolLiteral : public Expression {
 public:
  static SymbolLiteral* ReadFrom(Reader* reader);

  virtual ~SymbolLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  String* value() { return value_; }

 private:
  Child<String> value_;
};

class TypeLiteral : public Expression {
 public:
  static TypeLiteral* ReadFrom(Reader* reader);

  virtual ~TypeLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
  DartType* type() { return type_; }

 private:
  Child<DartType> type_;
};

class ThisExpression : public Expression {
 public:
  static ThisExpression* ReadFrom(Reader* reader);

  virtual ~ThisExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class Rethrow : public Expression {
 public:
  static Rethrow* ReadFrom(Reader* reader);

  virtual ~Rethrow();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class Throw : public Expression {
 public:
  static Throw* ReadFrom(Reader* reader);

  virtual ~Throw();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  Child<Expression> expression_;
};

class ListLiteral : public Expression {
 public:
  static ListLiteral* ReadFrom(Reader* reader);

  virtual ~ListLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool is_const() { return is_const_; }
  DartType* type() { return type_; }
  List<Expression>& expressions() { return expressions_; }

 private:
  bool is_const_;
  Child<DartType> type_;
  List<Expression> expressions_;
};

class MapLiteral : public Expression {
 public:
  static MapLiteral* ReadFrom(Reader* reader);

  virtual ~MapLiteral();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool is_const() { return is_const_; }
  DartType* keyType() { return key_type_; }
  DartType* valueType() { return value_type_; }
  List<MapEntry>& entries() { return entries_; }

 private:
  bool is_const_;
  Child<DartType> key_type_;
  Child<DartType> value_type_;
  List<MapEntry> entries_;
};

class MapEntry : public TreeNode {
 public:
  virtual ~MapEntry();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  Expression* key() { return key_; }
  Expression* value() { return value_; }

 private:
  Child<Expression> key_;
  Child<Expression> value_;
};

class AwaitExpression : public Expression {
 public:
  static AwaitExpression* ReadFrom(Reader* reader);

  virtual ~AwaitExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }

 private:
  Child<Expression> operand_;
};

class FunctionExpression : public Expression {
 public:
  static FunctionExpression* ReadFrom(Reader* reader);

  virtual ~FunctionExpression();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  FunctionNode* function() { return function_; }

 private:
  Child<FunctionNode> function_;
};

class Let : public Expression {
 public:
  static Let* ReadFrom(Reader* reader);

  virtual ~Let();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* body() { return body_; }

 private:
  Child<VariableDeclaration> variable_;
  Child<Expression> body_;
};

class Statement : public TreeNode {
 public:
  static Statement* ReadFrom(Reader* reader);

  virtual ~Statement();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptStatementVisitor(StatementVisitor* visitor) = 0;
};

class InvalidStatement : public Statement {
 public:
  static InvalidStatement* ReadFrom(Reader* reader);

  virtual ~InvalidStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);
};

class ExpressionStatement : public Statement {
 public:
  static ExpressionStatement* ReadFrom(Reader* reader);

  explicit ExpressionStatement(Expression* exp) : expression_(exp) {}
  virtual ~ExpressionStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  Child<Expression> expression_;
};

class Block : public Statement {
 public:
  static Block* ReadFrom(Reader* reader);

  virtual ~Block();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  List<Statement>& statements() { return statements_; }

 private:
  List<Statement> statements_;
};

class EmptyStatement : public Statement {
 public:
  static EmptyStatement* ReadFrom(Reader* reader);

  virtual ~EmptyStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);
};

class AssertStatement : public Statement {
 public:
  static AssertStatement* ReadFrom(Reader* reader);

  virtual ~AssertStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Expression* message() { return message_; }

 private:
  Child<Expression> condition_;
  Child<Expression> message_;
};

class LabeledStatement : public Statement {
 public:
  static LabeledStatement* ReadFrom(Reader* reader);

  virtual ~LabeledStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }

 private:
  Child<Statement> body_;
};

class BreakStatement : public Statement {
 public:
  static BreakStatement* ReadFrom(Reader* reader);

  virtual ~BreakStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  LabeledStatement* target() { return target_; }

 private:
  Ref<LabeledStatement> target_;
};

class WhileStatement : public Statement {
 public:
  static WhileStatement* ReadFrom(Reader* reader);

  virtual ~WhileStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* body() { return body_; }

 private:
  Child<Expression> condition_;
  Child<Statement> body_;
};

class DoStatement : public Statement {
 public:
  static DoStatement* ReadFrom(Reader* reader);

  virtual ~DoStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* body() { return body_; }

 private:
  Child<Expression> condition_;
  Child<Statement> body_;
};

class ForStatement : public Statement {
 public:
  static ForStatement* ReadFrom(Reader* reader);

  virtual ~ForStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  List<VariableDeclaration>& variables() { return variables_; }
  Expression* condition() { return condition_; }
  List<Expression>& updates() { return updates_; }
  Statement* body() { return body_; }

 private:
  List<VariableDeclaration> variables_;
  Child<Expression> condition_;
  List<Expression> updates_;
  Child<Statement> body_;
};

class ForInStatement : public Statement {
 public:
  static ForInStatement* ReadFrom(Reader* reader);

  virtual ~ForInStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* iterable() { return iterable_; }
  Statement* body() { return body_; }
  bool is_async() { return is_async_; }

 private:
  Child<VariableDeclaration> variable_;
  Child<Expression> iterable_;
  Child<Statement> body_;
  bool is_async_;
};

class SwitchStatement : public Statement {
 public:
  static SwitchStatement* ReadFrom(Reader* reader);

  virtual ~SwitchStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  List<SwitchCase>& cases() { return cases_; }

 private:
  Child<Expression> condition_;
  List<SwitchCase> cases_;
};

// TODO: SwitchCase : TreeNode
class SwitchCase : public TreeNode {
 public:
  virtual ~SwitchCase();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<Expression>& expressions() { return expressions_; }
  Statement* statement() { return statement_; }
  bool is_default() { return is_default_; }

 private:
  List<Expression> expressions_;
  Child<Statement> statement_;
  bool is_default_;
};

class ContinueSwitchStatement : public Statement {
 public:
  static ContinueSwitchStatement* ReadFrom(Reader* reader);

  virtual ~ContinueSwitchStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  SwitchCase* target() { return target_; }

 private:
  Ref<SwitchCase> target_;
};

class IfStatement : public Statement {
 public:
  static IfStatement* ReadFrom(Reader* reader);

  virtual ~IfStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* then() { return then_; }
  Statement* otherwise() { return otherwise_; }

 private:
  Child<Expression> condition_;
  Child<Statement> then_;
  Child<Statement> otherwise_;
};

class ReturnStatement : public Statement {
 public:
  static ReturnStatement* ReadFrom(Reader* reader);

  virtual ~ReturnStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  Child<Expression> expression_;
};

class TryCatch : public Statement {
 public:
  static TryCatch* ReadFrom(Reader* reader);

  virtual ~TryCatch();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }
  List<Catch>& catches() { return catches_; }

 private:
  Child<Statement> body_;
  List<Catch> catches_;
};

// TODO: Catch : TreeNode
class Catch : public TreeNode {
 public:
  virtual ~Catch();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  DartType* guard() { return guard_; }
  VariableDeclaration* exception() { return exception_; }
  VariableDeclaration* stack_trace() { return stack_trace_; }
  Statement* body() { return body_; }

 private:
  Child<DartType> guard_;
  Child<VariableDeclaration> exception_;
  Child<VariableDeclaration> stack_trace_;
  Child<Statement> body_;
};

class TryFinally : public Statement {
 public:
  static TryFinally* ReadFrom(Reader* reader);

  virtual ~TryFinally();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }
  Statement* finalizer() { return finalizer_; }

 private:
  Child<Statement> body_;
  Child<Statement> finalizer_;
};

class YieldStatement : public Statement {
 public:
  static YieldStatement* ReadFrom(Reader* reader);

  virtual ~YieldStatement();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* expression() { return expression_; }
  bool is_yield_start() { return is_yield_star_; }

 private:
  Child<Expression> expression_;
  bool is_yield_star_;
};

class VariableDeclaration : public Statement {
 public:
  static VariableDeclaration* ReadFrom(Reader* reader);

  virtual ~VariableDeclaration();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  String* name() { return name_; }
  word flags() { return flags_; }
  DartType* type() { return type_; }
  Expression* initializer() { return initializer_; }

 private:
  Child<String> name_;
  word flags_;
  Child<DartType> type_;
  Child<Expression> initializer_;
};

class FunctionDeclaration : public Statement {
 public:
  static FunctionDeclaration* ReadFrom(Reader* reader);

  virtual ~FunctionDeclaration();

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  FunctionNode* function() { return function_; }

 private:
  Child<VariableDeclaration> variable_;
  Child<FunctionNode> function_;
};

class Name : public Node {
 public:
  static Name* ReadFrom(Reader* reader);

  // TODO: Use a real [Library] and not an index.
  virtual ~Name();

  virtual void AcceptVisitor(Visitor* visitor);

  String* string() { return string_; }
  int library_index() { return library_index_; }

 private:
  Name(String* string, int library_index) : string_(string), library_index_(library_index) { }

  Child<String> string_;
  int library_index_;
};

// TODO: _PrivateName/_PublicName : Name

class DartType : public Node {
 public:
  static DartType* ReadFrom(Reader* reader);

  virtual ~DartType();

  virtual void AcceptVisitor(Visitor* visitor);
  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor) = 0;
};

class InvalidType : public DartType {
 public:
  static InvalidType* ReadFrom(Reader* reader);

  virtual ~InvalidType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class DynamicType : public DartType {
 public:
  static DynamicType* ReadFrom(Reader* reader);

  virtual ~DynamicType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class VoidType : public DartType {
 public:
  static VoidType* ReadFrom(Reader* reader);

  virtual ~VoidType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class InterfaceType : public DartType {
 public:
  static InterfaceType* ReadFrom(Reader* reader);

  InterfaceType(Class* klass) : klass_(klass) {}
  virtual ~InterfaceType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  Class* klass() { return klass_; }
  List<DartType>& type_arguments() { return type_arguments_; }

 private:
  Ref<Class> klass_;
  List<DartType> type_arguments_;
};

class FunctionType : public DartType {
 public:
  static FunctionType* ReadFrom(Reader* reader);

  virtual ~FunctionType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  List<TypeParameter>& type_parameters() { return type_parameters_; }
  int required_parameter_count() { return required_parameter_count_; }
  List<DartType>& positional_parameters() { return positional_parameters_; }
  List<Tuple<String, DartType> >& named_parameters() { return named_parameters_; }
  DartType* return_type() { return return_type_; }

 private:
  List<TypeParameter> type_parameters_;
  int required_parameter_count_;
  List<DartType> positional_parameters_;
  List<Tuple<String, DartType> > named_parameters_;
  Child<DartType> return_type_;
};

class TypeParameterType : public DartType {
 public:
  static TypeParameterType* ReadFrom(Reader* reader);

  virtual ~TypeParameterType();

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  TypeParameter* parameter() { return parameter_; }

 private:
  Child<TypeParameter> parameter_;
};

class TypeParameter : public TreeNode {
 public:
  static TypeParameter* ReadFrom(Reader* reader);

  virtual ~TypeParameter();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  DartType* bound() { return bound_; }

 private:
  Child<String> name_;
  Child<DartType> bound_;
};

class Program : public TreeNode {
 public:
  static Program* ReadFrom(Reader* reader);

  virtual ~Program();

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<Library>& libraries() { return libraries_; }
  Procedure* main_method() { return main_method_; }

 private:
  List<Library> libraries_;
  Ref<Procedure> main_method_;
};

class ExpressionVisitor {
 public:
  virtual ~ExpressionVisitor() {}

  virtual void VisitDefaultExpression(Expression* node) = 0;
  virtual void VisitDefaultBasicLiteral(BasicLiteral* node) { VisitDefaultExpression(node); }
  virtual void VisitInvalidExpression(InvalidExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitVariableGet(VariableGet* node) { VisitDefaultExpression(node); }
  virtual void VisitVariableSet(VariableSet* node) { VisitDefaultExpression(node); }
  virtual void VisitPropertyGet(PropertyGet* node) { VisitDefaultExpression(node); }
  virtual void VisitPropertySet(PropertySet* node) { VisitDefaultExpression(node); }
  virtual void VisitSuperPropertyGet(SuperPropertyGet* node) { VisitDefaultExpression(node); }
  virtual void VisitSuperPropertySet(SuperPropertySet* node) { VisitDefaultExpression(node); }
  virtual void VisitStaticGet(StaticGet* node) { VisitDefaultExpression(node); }
  virtual void VisitStaticSet(StaticSet* node) { VisitDefaultExpression(node); }
  virtual void VisitMethodInvocation(MethodInvocation* node) { VisitDefaultExpression(node); }
  virtual void VisitSuperMethodInvocation(SuperMethodInvocation* node) { VisitDefaultExpression(node); }
  virtual void VisitStaticInvocation(StaticInvocation* node) { VisitDefaultExpression(node); }
  virtual void VisitFunctionInvocation(FunctionInvocation* node) { VisitDefaultExpression(node); }
  virtual void VisitConstructorInvocation(ConstructorInvocation* node) { VisitDefaultExpression(node); }
  virtual void VisitNot(Not* node) { VisitDefaultExpression(node); }
  virtual void VisitLogicalExpression(LogicalExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitConditionalExpression(ConditionalExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitStringConcatenation(StringConcatenation* node) { VisitDefaultExpression(node); }
  virtual void VisitIsExpression(IsExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitAsExpression(AsExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitSymbolLiteral(SymbolLiteral* node) { VisitDefaultExpression(node); }
  virtual void VisitTypeLiteral(TypeLiteral* node) { VisitDefaultExpression(node); }
  virtual void VisitThisExpression(ThisExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitRethrow(Rethrow* node) { VisitDefaultExpression(node); }
  virtual void VisitThrow(Throw* node) { VisitDefaultExpression(node); }
  virtual void VisitListLiteral(ListLiteral* node) { VisitDefaultExpression(node); }
  virtual void VisitMapLiteral(MapLiteral* node) { VisitDefaultExpression(node); }
  virtual void VisitAwaitExpression(AwaitExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitFunctionExpression(FunctionExpression* node) { VisitDefaultExpression(node); }
  virtual void VisitStringLiteral(StringLiteral* node) { VisitDefaultBasicLiteral(node); }
  virtual void VisitIntLiteral(IntLiteral* node) { VisitDefaultBasicLiteral(node); }
  virtual void VisitDoubleLiteral(DoubleLiteral* node) { VisitDefaultBasicLiteral(node); }
  virtual void VisitBoolLiteral(BoolLiteral* node) { VisitDefaultBasicLiteral(node); }
  virtual void VisitNullLiteral(NullLiteral* node) { VisitDefaultBasicLiteral(node); }
  virtual void VisitLet(Let* node) { VisitDefaultExpression(node); }
};

class StatementVisitor {
 public:
  virtual ~StatementVisitor() {}

  virtual void VisitDefaultStatement(Statement* node) = 0;
  virtual void VisitInvalidStatement(InvalidStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitExpressionStatement(ExpressionStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitBlock(Block* node) { VisitDefaultStatement(node); }
  virtual void VisitEmptyStatement(EmptyStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitAssertStatement(AssertStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitLabeledStatement(LabeledStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitBreakStatement(BreakStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitWhileStatement(WhileStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitDoStatement(DoStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitForStatement(ForStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitForInStatement(ForInStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitSwitchStatement(SwitchStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitContinueSwitchStatement(ContinueSwitchStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitIfStatement(IfStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitReturnStatement(ReturnStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitTryCatch(TryCatch* node) { VisitDefaultStatement(node); }
  virtual void VisitTryFinally(TryFinally* node) { VisitDefaultStatement(node); }
  virtual void VisitYieldStatement(YieldStatement* node) { VisitDefaultStatement(node); }
  virtual void VisitVariableDeclaration(VariableDeclaration* node) { VisitDefaultStatement(node); }
  virtual void VisitFunctionDeclaration(FunctionDeclaration* node) { VisitDefaultStatement(node); }
};

class MemberVisitor {
 public:
  virtual ~MemberVisitor() {}

  virtual void VisitDefaultMember(Member* node) = 0;
  virtual void VisitConstructor(Constructor* node) { VisitDefaultMember(node); }
  virtual void VisitProcedure(Procedure* node) { VisitDefaultMember(node); }
  virtual void VisitField(Field* node) { VisitDefaultMember(node); }
};

class ClassVisitor {
 public:
  virtual ~ClassVisitor() {}

  virtual void VisitDefaultClass(Class* node) = 0;
  virtual void VisitNormalClass(NormalClass* node) { VisitDefaultClass(node); }
  virtual void VisitMixinClass(MixinClass* node) { VisitDefaultClass(node); }
};

class InitializerVisitor {
 public:
  virtual ~InitializerVisitor() {}

  virtual void VisitDefaultInitializer(Initializer* node) = 0;
  virtual void VisitInvalidInitializer(InvalidInitializer* node) { VisitDefaultInitializer(node); }
  virtual void VisitFieldInitializer(FieldInitializer* node) { VisitDefaultInitializer(node); }
  virtual void VisitSuperInitializer(SuperInitializer* node) { VisitDefaultInitializer(node); }
  virtual void VisitRedirectingInitializer(RedirectingInitializer* node) { VisitDefaultInitializer(node); }
};

class DartTypeVisitor {
 public:
  virtual ~DartTypeVisitor() {}

  virtual void VisitDefaultDartType(DartType* node) = 0;
  virtual void VisitInvalidType(InvalidType* node) { VisitDefaultDartType(node); }
  virtual void VisitDynamicType(DynamicType* node) { VisitDefaultDartType(node); }
  virtual void VisitVoidType(VoidType* node) { VisitDefaultDartType(node); }
  virtual void VisitInterfaceType(InterfaceType* node) { VisitDefaultDartType(node); }
  virtual void VisitFunctionType(FunctionType* node) { VisitDefaultDartType(node); }
  virtual void VisitTypeParameterType(TypeParameterType* node) { VisitDefaultDartType(node); }
};

class ClassReferenceVisitor {
 public:
  virtual ~ClassReferenceVisitor() {}

  virtual void VisitDefaultClassReference(Class* node) = 0;
  virtual void VisitNormalClassReference(NormalClass* node) { VisitDefaultClassReference(node); }
  virtual void VisitMixinClassReference(MixinClass* node) { VisitDefaultClassReference(node); }
};

class MemberReferenceVisitor {
 public:
  virtual ~MemberReferenceVisitor() {}

  virtual void VisitDefaultMemberReference(Member* node) = 0;
  virtual void VisitFieldReference(Field* node) { VisitDefaultMemberReference(node); }
  virtual void VisitConstructorReference(Constructor* node) { VisitDefaultMemberReference(node); }
  virtual void VisitProcedureReference(Procedure* node) { VisitDefaultMemberReference(node); }
};

class TreeVisitor : public ExpressionVisitor,
                    public StatementVisitor,
                    public MemberVisitor,
                    public ClassVisitor,
                    public InitializerVisitor {
 public:
  virtual ~TreeVisitor() {}

  virtual void VisitDefaultTreeNode(TreeNode* node) = 0;
  virtual void VisitDefaultStatement(Statement* node) { VisitDefaultTreeNode(node); }
  virtual void VisitDefaultExpression(Expression* node) { VisitDefaultTreeNode(node); }
  virtual void VisitDefaultMember(Member* node) { VisitDefaultTreeNode(node); }
  virtual void VisitDefaultClass(Class* node) { VisitDefaultTreeNode(node); }
  virtual void VisitDefaultInitializer(Initializer* node) { VisitDefaultTreeNode(node); }

  virtual void VisitLibrary(Library* node) { VisitDefaultTreeNode(node); }
  virtual void VisitTypeParameter(TypeParameter* node) { VisitDefaultTreeNode(node); }
  virtual void VisitFunctionNode(FunctionNode* node) { VisitDefaultTreeNode(node); }
  virtual void VisitArguments(Arguments* node) { VisitDefaultTreeNode(node); }
  virtual void VisitNamedExpression(NamedExpression* node) { VisitDefaultTreeNode(node); }
  virtual void VisitSwitchCase(SwitchCase* node) { VisitDefaultTreeNode(node); }
  virtual void VisitCatch(Catch* node) { VisitDefaultTreeNode(node); }
  virtual void VisitMapEntry(MapEntry* node) { VisitDefaultTreeNode(node); }
  virtual void VisitProgram(Program* node) { VisitDefaultTreeNode(node); }
};

class Visitor : public TreeVisitor,
                public DartTypeVisitor,
                public ClassReferenceVisitor,
                public MemberReferenceVisitor {
 public:
  virtual ~Visitor() {}

  virtual void VisitDefaultNode(Node* node) = 0;
  virtual void VisitDefaultTreeNode(TreeNode* node) { VisitDefaultNode(node); }
  virtual void VisitDefaultDartType(DartType* node) { VisitDefaultNode(node); }
  virtual void VisitName(Name* node) { VisitDefaultNode(node); }
};


template<typename T>
List<T>::~List() {
  for (int i = 0; i < length_; i++) {
    delete array_[i];
  }
  delete array_;
}

template<typename T>
void List<T>::EnsureInitialized(int length) {
  if (length < length_) return;

  Child<T>* old_array = array_;
  int old_length = length_;

  length_ = length;
  array_ = new Child<T>[length_];

  // Move old elements at the start (if necessary).
  int offset = 0;
  if (old_array != NULL) {
    for (; offset < old_length; offset++) {
      array_[offset] = old_array[offset];
    }
  }

  // Set the rest to NULL.
  for (; offset < length_; offset++) {
    array_[offset] = NULL;
  }
}

template<typename T>
template<typename IT>
IT* List<T>::GetOrCreate(int index) {
  EnsureInitialized(index + 1);

  T* member = array_[index];
  if (member == NULL) {
    member = array_[index] = new IT();
  }
  return static_cast<IT*>(member);
}

}  // namespace dil

dil::Program* GetPrecompiledDil(const char* filename);

}  // namespace dart

#endif  // VM_DIL_H_
