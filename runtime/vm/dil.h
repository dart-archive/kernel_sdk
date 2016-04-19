// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_H_
#define VM_DIL_H_

#include <stdint.h>

#include "platform/assert.h"
#include "vm/globals.h"

#define DIL_NODES_DO(M) \
  M(Name) \
  M(DartType) \
  M(InvalidType) \
  M(DynamicType) \
  M(VoidType) \
  M(InterfaceType) \
  M(FunctionType) \
  M(TypeParameterType)

#define DIL_TREE_NODES_DO(M) \
  M(Library) \
  M(Class) \
  M(NormalClass) \
  M(MixinClass) \
  M(Member) \
  M(Field) \
  M(Constructor) \
  M(Procedure) \
  M(Initializer) \
  M(InvalidInitializer) \
  M(FieldInitializer) \
  M(SuperInitializer) \
  M(RedirectingInitializer) \
  M(FunctionNode) \
  M(Expression) \
  M(InvalidExpression) \
  M(VariableGet) \
  M(VariableSet) \
  M(PropertyGet) \
  M(PropertySet) \
  M(SuperPropertyGet) \
  M(SuperPropertySet) \
  M(StaticGet) \
  M(StaticSet) \
  M(Arguments) \
  M(NamedExpression) \
  M(MethodInvocation) \
  M(SuperMethodInvocation) \
  M(StaticInvocation) \
  M(ConstructorInvocation) \
  M(Not) \
  M(LogicalExpression) \
  M(ConditionalExpression) \
  M(StringConcatenation) \
  M(IsExpression) \
  M(AsExpression) \
  M(BasicLiteral) \
  M(StringLiteral) \
  M(BigintLiteral) \
  M(IntLiteral) \
  M(DoubleLiteral) \
  M(BoolLiteral) \
  M(NullLiteral) \
  M(SymbolLiteral) \
  M(TypeLiteral) \
  M(ThisExpression) \
  M(Rethrow) \
  M(Throw) \
  M(ListLiteral) \
  M(MapLiteral) \
  M(MapEntry) \
  M(AwaitExpression) \
  M(FunctionExpression) \
  M(Let) \
  M(Statement) \
  M(InvalidStatement) \
  M(ExpressionStatement) \
  M(Block) \
  M(EmptyStatement) \
  M(AssertStatement) \
  M(LabeledStatement) \
  M(BreakStatement) \
  M(WhileStatement) \
  M(DoStatement) \
  M(ForStatement) \
  M(ForInStatement) \
  M(SwitchStatement) \
  M(SwitchCase) \
  M(ContinueSwitchStatement) \
  M(IfStatement) \
  M(ReturnStatement) \
  M(TryCatch) \
  M(Catch) \
  M(TryFinally) \
  M(YieldStatement) \
  M(VariableDeclaration) \
  M(FunctionDeclaration) \
  M(TypeParameter) \
  M(Program)

#define DIL_ALL_NODES_DO(M) \
  M(Node) \
  DIL_NODES_DO(M) \
  M(TreeNode) \
  DIL_TREE_NODES_DO(M)

#define DIL_VISITORS_DO(M) \
  M(ExpressionVisitor) \
  M(StatementVisitor) \
  M(MemberVisitor) \
  M(ClassVisitor) \
  M(InitializerVisitor) \
  M(DartTypeVisitor) \
  M(ClassReferenceVisitor) \
  M(MemberReferenceVisitor) \
  M(TreeVisitor) \
  M(Visitor)

namespace dart {

namespace dil {

class Reader;
class TreeNode;
class Writer;

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

  T* operator->() { return pointer_; }

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

  T* operator->() { return pointer_; }

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
  void ReadFrom(Reader* reader, TreeNode* parent);

  template<typename IT>
  void ReadFromStatic(Reader* reader);

  void WriteTo(Writer* writer);

  template<typename IT>
  void WriteToStatic(Writer* writer);

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

  template<typename IT, typename PT>
  IT* GetOrCreate(int index, PT* parent);

  // Returns element at [index].
  T*& operator[](int index) {
    ASSERT(index < length_);
    return array_[index];
  }

  int length() { return length_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(List);

  T** array_;
  int length_;
};

template<typename A, typename B>
class Tuple {
 public:
  static Tuple<A, B>* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  Tuple(A* a, B* b) : first_(a), second_(b) {}

 private:
  DISALLOW_COPY_AND_ASSIGN(Tuple);
  Tuple() {}

  Ref<A> first_;
  Child<B> second_;
};

class String {
 public:
  static String* ReadFrom(Reader* reader);
  static String* ReadFromImpl(Reader* reader);
  void WriteTo(Writer* writer);
  void WriteToImpl(Writer* writer);

  String(uint8_t* utf8, int length) {
    buffer_ = new uint8_t[length];
    size_ = length;
    memcpy(buffer_, utf8, length);
  }
  ~String() {
    delete[] buffer_;
  }

  uint8_t* buffer() { return buffer_; }
  int size() { return size_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(String);

  uint8_t* buffer_;
  int size_;
};

class StringTable {
 public:
  void ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  List<String>& strings() { return strings_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(StringTable);
  StringTable() {}

  friend class Program;

  List<String> strings_;
};

// Forward declare all classes.
#define DO(name) class name;
DIL_ALL_NODES_DO(DO)
DIL_VISITORS_DO(DO)
#undef DO


#define DEFINE_CASTING_OPERATIONS(klass)        \
  virtual bool Is##klass() { return true; }     \
                                                \
  static klass* Cast(Node* node) {              \
    ASSERT(node == NULL || node->Is##klass());  \
    return static_cast<klass*>(node);           \
  }

#define DEFINE_IS_OPERATION(klass)            \
  virtual bool Is##klass() { return false; }  \

#define DEFINE_ALL_IS_OPERATIONS()            \
  DIL_NODES_DO(DEFINE_IS_OPERATION)           \
  DEFINE_IS_OPERATION(TreeNode)               \
  DIL_TREE_NODES_DO(DEFINE_IS_OPERATION)

class Node {
 public:
  virtual ~Node();

  DEFINE_ALL_IS_OPERATIONS();
  DEFINE_CASTING_OPERATIONS(Node);

  virtual void AcceptVisitor(Visitor* visitor) = 0;
};

class TreeNode : public Node {
 public:
  TreeNode() {}
  virtual ~TreeNode();

  DEFINE_CASTING_OPERATIONS(TreeNode);

  virtual void AcceptVisitor(Visitor* visitor);
  virtual void AcceptTreeVisitor(TreeVisitor* visitor) = 0;
};

class Library : public TreeNode {
 public:
  Library* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Library();

  DEFINE_CASTING_OPERATIONS(Library);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  List<Class>& classes() { return classes_; }
  List<Field>& fields() { return fields_; }
  List<Procedure>& procedures() { return procedures_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Library);
  Library() : name_(NULL) {}

  template<typename T>
  friend class List;

  Ref<String> name_;
  List<Class> classes_;
  List<Field> fields_;
  List<Procedure> procedures_;
};

class Class : public TreeNode {
 public:
  Class* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Class();

  DEFINE_CASTING_OPERATIONS(Class);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptClassVisitor(ClassVisitor* visitor) = 0;

  Library* parent() { return parent_; }
  String* name() { return name_; }
  bool is_abstract() { return is_abstract_; }

  virtual List<TypeParameter>& type_parameters() = 0;
  virtual List<InterfaceType>& implemented_classes() = 0;
  virtual List<Field>& fields() = 0;
  virtual List<Constructor>& constructors() = 0;
  virtual List<Procedure>& procedures() = 0;

 protected:
  Class() : is_abstract_(false) {}

 private:
  template<typename T>
  friend class List;

  DISALLOW_COPY_AND_ASSIGN(Class);

  Ref<Library> parent_;
  Ref<String> name_;
  bool is_abstract_;
};

class NormalClass : public Class {
 public:
  NormalClass* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~NormalClass();

  DEFINE_CASTING_OPERATIONS(NormalClass);

  virtual void AcceptClassVisitor(ClassVisitor* visitor);

  InterfaceType* super_class() { return super_class_; }
  virtual List<TypeParameter>& type_parameters() { return type_parameters_; }
  virtual List<InterfaceType>& implemented_classes() { return implemented_classes_; }
  virtual List<Field>& fields() { return fields_; }
  virtual List<Constructor>& constructors() { return constructors_; }
  virtual List<Procedure>& procedures() { return procedures_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(NormalClass);
  NormalClass() {}

  template<typename T>
  friend class List;

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
  void WriteTo(Writer* writer);

  virtual ~MixinClass();

  DEFINE_CASTING_OPERATIONS(MixinClass);

  virtual void AcceptClassVisitor(ClassVisitor* visitor);

  InterfaceType* first() { return first_; }
  InterfaceType* second() { return second_; }
  virtual List<TypeParameter>& type_parameters() { return type_parameters_; }
  virtual List<InterfaceType>& implemented_classes() { return implemented_classes_; }
  virtual List<Field>& fields() { return fields_; }
  virtual List<Constructor>& constructors() { return constructors_; }
  virtual List<Procedure>& procedures() { return procedures_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(MixinClass);
  MixinClass() {}

  template<typename T>
  friend class List;

  bool is_abstract_;
  Ref<String> name_;
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
  virtual ~Member();

  DEFINE_CASTING_OPERATIONS(Member);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptMemberVisitor(MemberVisitor* visitor) = 0;

  TreeNode* parent() { return parent_; }
  Name* name() { return name_; }

 protected:
  template<typename T>
  friend class List;

  Ref<TreeNode> parent_;
  Child<Name> name_;
};

class Field : public Member {
 public:
  Field* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Field();

  DEFINE_CASTING_OPERATIONS(Field);

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  word flags() { return flags_; }
  DartType* type() { return type_; }
  Expression* initializer() { return initializer_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Field);
  Field() {}

  template<typename T>
  friend class List;

  word flags_;
  Child<DartType> type_;
  Child<Expression> initializer_;
};

class Constructor : public Member {
 public:
  Constructor* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Constructor();

  DEFINE_CASTING_OPERATIONS(Constructor);

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  Name* name() { return name_; }
  FunctionNode* function() { return function_; }
  List<Initializer>& initializers() { return initializers_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Constructor);

  template<typename T>
  friend class List;

  Constructor() {}

  uint8_t flags_;
  Child<Name> name_;
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
  void WriteTo(Writer* writer);

  virtual ~Procedure();

  DEFINE_CASTING_OPERATIONS(Procedure);

  virtual void AcceptMemberVisitor(MemberVisitor* visitor);

  ProcedureKind kind() { return kind_; }
  word flags() { return flags_; }
  FunctionNode* function() { return function_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Procedure);
  Procedure() : kind_(kIncompleteProcedure), flags_(0), function_(NULL) {}

  template<typename T>
  friend class List;

  ProcedureKind kind_;
  word flags_;
  Child<FunctionNode> function_;
};

class Initializer : public TreeNode {
 public:
  static Initializer* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer) = 0;

  virtual ~Initializer();

  DEFINE_CASTING_OPERATIONS(Initializer);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor) = 0;
};

class InvalidInitializer : public Initializer {
 public:
  static InvalidInitializer* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~InvalidInitializer();

  DEFINE_CASTING_OPERATIONS(InvalidInitializer);
  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);
};

class FieldInitializer : public Initializer {
 public:
  static FieldInitializer* ReadFromImpl(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~FieldInitializer();

  DEFINE_CASTING_OPERATIONS(FieldInitializer);

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Field* field() { return field_; }
  Expression* value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(FieldInitializer);
  FieldInitializer() {}

  Ref<Field> field_;
  Child<Expression> value_;
};

class SuperInitializer : public Initializer {
 public:
  static SuperInitializer* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SuperInitializer();

  DEFINE_CASTING_OPERATIONS(SuperInitializer);

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SuperInitializer);
  SuperInitializer() {}

  Ref<Constructor> target_;
  Child<Arguments> arguments_;
};

class RedirectingInitializer : public Initializer {
 public:
  static RedirectingInitializer* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~RedirectingInitializer();

  DEFINE_CASTING_OPERATIONS(RedirectingInitializer);

  virtual void AcceptInitializerVisitor(InitializerVisitor* visitor);

  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(RedirectingInitializer);
  RedirectingInitializer() {}

  Ref<Constructor> target_;
  Child<Arguments> arguments_;
};

class FunctionNode : public TreeNode {
 public:
  enum AsyncMarker {
    kSync = 0,
    kSyncStar = 1,
    kAsync = 2,
    kAsyncStar = 3,
  };

  static FunctionNode* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~FunctionNode();

  DEFINE_CASTING_OPERATIONS(FunctionNode);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  AsyncMarker async_marker() { return async_marker_; }
  List<TypeParameter>& type_parameters() { return type_parameters_; }
  int required_parameter_count() { return required_parameter_count_; }
  List<VariableDeclaration>& positional_parameters() { return positional_parameters_; }
  List<VariableDeclaration>& named_parameters() { return named_parameters_; }
  DartType* return_type() { return return_type_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(FunctionNode);
  FunctionNode() {}

  AsyncMarker async_marker_;
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
  virtual void WriteTo(Writer* writer) = 0;

  virtual ~Expression();

  DEFINE_CASTING_OPERATIONS(Expression);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor) = 0;
};

class InvalidExpression : public Expression {
 public:
  static InvalidExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~InvalidExpression();

  DEFINE_CASTING_OPERATIONS(InvalidExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class VariableGet : public Expression {
 public:
  static VariableGet* ReadFrom(Reader* reader);
  static VariableGet* ReadFrom(Reader* reader, uint8_t payload);
  virtual void WriteTo(Writer* writer);

  virtual ~VariableGet();

  DEFINE_CASTING_OPERATIONS(VariableGet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(VariableGet);
  VariableGet() {}

  Ref<VariableDeclaration> variable_;
};

class VariableSet : public Expression {
 public:
  static VariableSet* ReadFrom(Reader* reader);
  static VariableSet* ReadFrom(Reader* reader, uint8_t payload);
  virtual void WriteTo(Writer* writer);

  virtual ~VariableSet();

  DEFINE_CASTING_OPERATIONS(VariableSet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(VariableSet);
  VariableSet() {}

  Ref<VariableDeclaration> variable_;
  Child<Expression> expression_;
};

class PropertyGet : public Expression {
 public:
  static PropertyGet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~PropertyGet();

  DEFINE_CASTING_OPERATIONS(PropertyGet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(PropertyGet);
  PropertyGet() {}

  Child<Expression> receiver_;
  Child<Name> name_;
};

class PropertySet : public Expression {
 public:
  static PropertySet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~PropertySet();

  DEFINE_CASTING_OPERATIONS(PropertySet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }
  Expression* value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(PropertySet);
  PropertySet() {}

  Child<Expression> receiver_;
  Child<Name> name_;
  Child<Expression> value_;
};

class SuperPropertyGet : public Expression {
 public:
  static SuperPropertyGet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SuperPropertyGet();

  DEFINE_CASTING_OPERATIONS(SuperPropertyGet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SuperPropertyGet);
  SuperPropertyGet() {}

  Ref<Member> target_;
};

class SuperPropertySet : public Expression {
 public:
  static SuperPropertySet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SuperPropertySet();

  DEFINE_CASTING_OPERATIONS(SuperPropertySet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }
  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SuperPropertySet);
  SuperPropertySet() {}

  Ref<Member> target_;
  Child<Expression> expression_;
};

class StaticGet : public Expression {
 public:
  static StaticGet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~StaticGet();

  DEFINE_CASTING_OPERATIONS(StaticGet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(StaticGet);
  StaticGet() {}

  Ref<Member> target_;
};

class StaticSet : public Expression {
 public:
  static StaticSet* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~StaticSet();

  DEFINE_CASTING_OPERATIONS(StaticSet);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Member* target() { return target_; }
  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(StaticSet);
  StaticSet() {}

  Ref<Member> target_;
  Child<Expression> expression_;
};

class Arguments : public TreeNode {
 public:
  static Arguments* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Arguments();

  DEFINE_CASTING_OPERATIONS(Arguments);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<DartType>& types() { return types_; }
  List<Expression>& positional() { return positional_; }
  List<NamedExpression>& named() { return named_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Arguments);
  Arguments() {}

  List<DartType> types_;
  List<Expression> positional_;
  List<NamedExpression> named_;
};

class NamedExpression : public TreeNode {
 public:
  static NamedExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  NamedExpression(String* name, Expression* expr) : name_(name), expression_(expr) {}
  virtual ~NamedExpression();

  DEFINE_CASTING_OPERATIONS(NamedExpression);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(NamedExpression);
  NamedExpression() {}

  Ref<String> name_;
  Child<Expression> expression_;
};

class MethodInvocation : public Expression {
 public:
  static MethodInvocation* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~MethodInvocation();

  DEFINE_CASTING_OPERATIONS(MethodInvocation);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* receiver() { return receiver_; }
  Name* name() { return name_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(MethodInvocation);
  MethodInvocation() {}

  Child<Expression> receiver_;
  Child<Name> name_;
  Child<Arguments> arguments_;
};

class SuperMethodInvocation : public Expression {
 public:
  static SuperMethodInvocation* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SuperMethodInvocation();

  DEFINE_CASTING_OPERATIONS(SuperMethodInvocation);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Procedure* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SuperMethodInvocation);
  SuperMethodInvocation() {}

  Ref<Procedure> target_;
  Child<Arguments> arguments_;
};

class StaticInvocation : public Expression {
 public:
  static StaticInvocation* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  explicit StaticInvocation(Procedure* procedure, Arguments* args)
      : procedure_(procedure), arguments_(args) {}
  ~StaticInvocation();

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Procedure* procedure() { return procedure_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(StaticInvocation);
  StaticInvocation() {}

  Ref<Procedure> procedure_;
  Child<Arguments> arguments_;
};

class ConstructorInvocation : public Expression {
 public:
  static ConstructorInvocation* ReadFrom(Reader* reader, bool is_const);
  virtual void WriteTo(Writer* writer);

  virtual ~ConstructorInvocation();

  DEFINE_CASTING_OPERATIONS(ConstructorInvocation);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool is_const() { return is_const_; }
  Constructor* target() { return target_; }
  Arguments* arguments() { return arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ConstructorInvocation);
  ConstructorInvocation() {}

  bool is_const_;
  Ref<Constructor> target_;
  Child<Arguments> arguments_;
};

class Not : public Expression {
 public:
  static Not* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Not();

  DEFINE_CASTING_OPERATIONS(Not);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Not);
  Not() {}

  Child<Expression> expression_;
};

class LogicalExpression : public Expression {
 public:
  static LogicalExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~LogicalExpression();

  DEFINE_CASTING_OPERATIONS(LogicalExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* left() { return left_; }
  uint8_t op() { return operator_; }
  Expression* right() { return right_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(LogicalExpression);
  LogicalExpression() {}

  Child<Expression> left_;
  uint8_t operator_;
  Child<Expression> right_;
};

class ConditionalExpression : public Expression {
 public:
  static ConditionalExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ConditionalExpression();

  DEFINE_CASTING_OPERATIONS(ConditionalExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* condition() { return condition_; }
  Expression* then() { return then_; }
  Expression* otherwise() { return otherwise_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ConditionalExpression);
  ConditionalExpression() {}

  Child<Expression> condition_;
  Child<Expression> then_;
  Child<Expression> otherwise_;
};

class StringConcatenation : public Expression {
 public:
  static StringConcatenation* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~StringConcatenation();

  DEFINE_CASTING_OPERATIONS(StringConcatenation);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  List<Expression>& expressions() { return expressions_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(StringConcatenation);
  StringConcatenation() {}

  List<Expression> expressions_;
};

class IsExpression : public Expression {
 public:
  static IsExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~IsExpression();

  DEFINE_CASTING_OPERATIONS(IsExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }
  DartType* type() { return type_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(IsExpression);
  IsExpression() {}

  Child<Expression> operand_;
  Child<DartType> type_;
};

class AsExpression : public Expression {
 public:
  static AsExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~AsExpression();

  DEFINE_CASTING_OPERATIONS(AsExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }
  DartType* type() { return type_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(AsExpression);
  AsExpression() {}

  Child<Expression> operand_;
  Child<DartType> type_;
};

class BasicLiteral : public Expression {
 public:
  virtual ~BasicLiteral();

  DEFINE_CASTING_OPERATIONS(BasicLiteral);
};

class StringLiteral : public BasicLiteral {
 public:
  static StringLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  StringLiteral(String* string) : value_(string) {}
  virtual ~StringLiteral();

  DEFINE_CASTING_OPERATIONS(StringLiteral);

  String* value() { return value_; }

 protected:
  StringLiteral() {}

  Ref<String> value_;

 private:
  DISALLOW_COPY_AND_ASSIGN(StringLiteral);
};

class BigintLiteral : public StringLiteral {
 public:
  static BigintLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  BigintLiteral(String* string) : StringLiteral(string) {}
  virtual ~BigintLiteral();

  DEFINE_CASTING_OPERATIONS(BigintLiteral);

 private:
  DISALLOW_COPY_AND_ASSIGN(BigintLiteral);
  BigintLiteral() {}
};

class IntLiteral : public BasicLiteral {
 public:
  static IntLiteral* ReadFrom(Reader* reader, bool is_negative);
  static IntLiteral* ReadFrom(Reader* reader, uint8_t payload);
  virtual void WriteTo(Writer* writer);

  virtual ~IntLiteral();

  DEFINE_CASTING_OPERATIONS(IntLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  int64_t value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(IntLiteral);
  IntLiteral() {}

  int64_t value_;
};

class DoubleLiteral : public BasicLiteral {
 public:
  static DoubleLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~DoubleLiteral();

  DEFINE_CASTING_OPERATIONS(DoubleLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  String* value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(DoubleLiteral);
  DoubleLiteral() {}

  Ref<String> value_;
};

class BoolLiteral : public BasicLiteral {
 public:
  static BoolLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~BoolLiteral();

  DEFINE_CASTING_OPERATIONS(BoolLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(BoolLiteral);
  BoolLiteral() {}

  bool value_;
};

class NullLiteral : public BasicLiteral {
 public:
  static NullLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~NullLiteral();

  DEFINE_CASTING_OPERATIONS(NullLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class SymbolLiteral : public Expression {
 public:
  static SymbolLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SymbolLiteral();

  DEFINE_CASTING_OPERATIONS(SymbolLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  String* value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SymbolLiteral);
  SymbolLiteral() {}

  Ref<String> value_;
};

class TypeLiteral : public Expression {
 public:
  static TypeLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~TypeLiteral();

  DEFINE_CASTING_OPERATIONS(TypeLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
  DartType* type() { return type_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(TypeLiteral);
  TypeLiteral() {}

  Child<DartType> type_;
};

class ThisExpression : public Expression {
 public:
  static ThisExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ThisExpression();

  DEFINE_CASTING_OPERATIONS(ThisExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class Rethrow : public Expression {
 public:
  static Rethrow* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Rethrow();

  DEFINE_CASTING_OPERATIONS(Rethrow);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);
};

class Throw : public Expression {
 public:
  static Throw* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Throw();

  DEFINE_CASTING_OPERATIONS(Throw);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Throw);
  Throw() {}

  Child<Expression> expression_;
};

class ListLiteral : public Expression {
 public:
  static ListLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ListLiteral();

  DEFINE_CASTING_OPERATIONS(ListLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool is_const() { return is_const_; }
  DartType* type() { return type_; }
  List<Expression>& expressions() { return expressions_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ListLiteral);
  ListLiteral() {}

  bool is_const_;
  Child<DartType> type_;
  List<Expression> expressions_;
};

class MapLiteral : public Expression {
 public:
  static MapLiteral* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~MapLiteral();

  DEFINE_CASTING_OPERATIONS(MapLiteral);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  bool is_const() { return is_const_; }
  DartType* keyType() { return key_type_; }
  DartType* valueType() { return value_type_; }
  List<MapEntry>& entries() { return entries_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(MapLiteral);
  MapLiteral() {}

  bool is_const_;
  Child<DartType> key_type_;
  Child<DartType> value_type_;
  List<MapEntry> entries_;
};

class MapEntry : public TreeNode {
 public:
  static MapEntry* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~MapEntry();

  DEFINE_CASTING_OPERATIONS(MapEntry);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  Expression* key() { return key_; }
  Expression* value() { return value_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(MapEntry);
  MapEntry() {}

  template<typename T>
  friend class List;

  Child<Expression> key_;
  Child<Expression> value_;
};

class AwaitExpression : public Expression {
 public:
  static AwaitExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~AwaitExpression();

  DEFINE_CASTING_OPERATIONS(AwaitExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  Expression* operand() { return operand_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(AwaitExpression);
  AwaitExpression() {}

  Child<Expression> operand_;
};

class FunctionExpression : public Expression {
 public:
  static FunctionExpression* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~FunctionExpression();

  DEFINE_CASTING_OPERATIONS(FunctionExpression);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  FunctionNode* function() { return function_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(FunctionExpression);
  FunctionExpression() {}

  Child<FunctionNode> function_;
};

class Let : public Expression {
 public:
  static Let* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Let();

  DEFINE_CASTING_OPERATIONS(Let);

  virtual void AcceptExpressionVisitor(ExpressionVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Let);
  Let() {}

  Child<VariableDeclaration> variable_;
  Child<Expression> body_;
};

class Statement : public TreeNode {
 public:
  static Statement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer) = 0;

  virtual ~Statement();

  DEFINE_CASTING_OPERATIONS(Statement);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);
  virtual void AcceptStatementVisitor(StatementVisitor* visitor) = 0;
};

class InvalidStatement : public Statement {
 public:
  static InvalidStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~InvalidStatement();

  DEFINE_CASTING_OPERATIONS(InvalidStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);
};

class ExpressionStatement : public Statement {
 public:
  static ExpressionStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  explicit ExpressionStatement(Expression* exp) : expression_(exp) {}
  virtual ~ExpressionStatement();

  DEFINE_CASTING_OPERATIONS(ExpressionStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ExpressionStatement);
  ExpressionStatement() {}

  Child<Expression> expression_;
};

class Block : public Statement {
 public:
  static Block* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~Block();

  DEFINE_CASTING_OPERATIONS(Block);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  List<Statement>& statements() { return statements_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Block);
  Block() {}

  List<Statement> statements_;
};

class EmptyStatement : public Statement {
 public:
  static EmptyStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~EmptyStatement();

  DEFINE_CASTING_OPERATIONS(EmptyStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);
};

class AssertStatement : public Statement {
 public:
  static AssertStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~AssertStatement();

  DEFINE_CASTING_OPERATIONS(AssertStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Expression* message() { return message_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(AssertStatement);
  AssertStatement() {}

  Child<Expression> condition_;
  Child<Expression> message_;
};

class LabeledStatement : public Statement {
 public:
  static LabeledStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~LabeledStatement();

  DEFINE_CASTING_OPERATIONS(LabeledStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(LabeledStatement);
  LabeledStatement() {}

  Child<Statement> body_;
};

class BreakStatement : public Statement {
 public:
  static BreakStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~BreakStatement();

  DEFINE_CASTING_OPERATIONS(BreakStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  LabeledStatement* target() { return target_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(BreakStatement);
  BreakStatement() {}

  Ref<LabeledStatement> target_;
};

class WhileStatement : public Statement {
 public:
  static WhileStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~WhileStatement();

  DEFINE_CASTING_OPERATIONS(WhileStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(WhileStatement);
  WhileStatement() {}

  Child<Expression> condition_;
  Child<Statement> body_;
};

class DoStatement : public Statement {
 public:
  static DoStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~DoStatement();

  DEFINE_CASTING_OPERATIONS(DoStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(DoStatement);
  DoStatement() {}

  Child<Expression> condition_;
  Child<Statement> body_;
};

class ForStatement : public Statement {
 public:
  static ForStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ForStatement();

  DEFINE_CASTING_OPERATIONS(ForStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  List<VariableDeclaration>& variables() { return variables_; }
  Expression* condition() { return condition_; }
  List<Expression>& updates() { return updates_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ForStatement);
  ForStatement() {}

  List<VariableDeclaration> variables_;
  Child<Expression> condition_;
  List<Expression> updates_;
  Child<Statement> body_;
};

class ForInStatement : public Statement {
 public:
  static ForInStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ForInStatement();

  DEFINE_CASTING_OPERATIONS(ForInStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  Expression* iterable() { return iterable_; }
  Statement* body() { return body_; }
  bool is_async() { return is_async_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ForInStatement);
  ForInStatement() {}

  Child<VariableDeclaration> variable_;
  Child<Expression> iterable_;
  Child<Statement> body_;
  bool is_async_;
};

class SwitchStatement : public Statement {
 public:
  static SwitchStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~SwitchStatement();

  DEFINE_CASTING_OPERATIONS(SwitchStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  List<SwitchCase>& cases() { return cases_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SwitchStatement);
  SwitchStatement() {}

  Child<Expression> condition_;
  List<SwitchCase> cases_;
};

class SwitchCase : public TreeNode {
 public:
  SwitchCase* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~SwitchCase();

  DEFINE_CASTING_OPERATIONS(SwitchCase);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  List<Expression>& expressions() { return expressions_; }
  bool is_default() { return is_default_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(SwitchCase);
  SwitchCase() {}

  template<typename T>
  friend class List;

  List<Expression> expressions_;
  bool is_default_;
  Child<Statement> body_;
};

class ContinueSwitchStatement : public Statement {
 public:
  static ContinueSwitchStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ContinueSwitchStatement();

  DEFINE_CASTING_OPERATIONS(ContinueSwitchStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  SwitchCase* target() { return target_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ContinueSwitchStatement);
  ContinueSwitchStatement() {}

  Ref<SwitchCase> target_;
};

class IfStatement : public Statement {
 public:
  static IfStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~IfStatement();

  DEFINE_CASTING_OPERATIONS(IfStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* condition() { return condition_; }
  Statement* then() { return then_; }
  Statement* otherwise() { return otherwise_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(IfStatement);
  IfStatement() {}

  Child<Expression> condition_;
  Child<Statement> then_;
  Child<Statement> otherwise_;
};

class ReturnStatement : public Statement {
 public:
  static ReturnStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~ReturnStatement();

  DEFINE_CASTING_OPERATIONS(ReturnStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(ReturnStatement);
  ReturnStatement() {}

  Child<Expression> expression_;
};

class TryCatch : public Statement {
 public:
  static TryCatch* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~TryCatch();

  DEFINE_CASTING_OPERATIONS(TryCatch);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }
  List<Catch>& catches() { return catches_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(TryCatch);
  TryCatch() {}

  Child<Statement> body_;
  List<Catch> catches_;
};

class Catch : public TreeNode {
 public:
  static Catch* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Catch();

  DEFINE_CASTING_OPERATIONS(Catch);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  DartType* guard() { return guard_; }
  VariableDeclaration* exception() { return exception_; }
  VariableDeclaration* stack_trace() { return stack_trace_; }
  Statement* body() { return body_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Catch);
  Catch() {}

  template<typename T>
  friend class List;

  Child<DartType> guard_;
  Child<VariableDeclaration> exception_;
  Child<VariableDeclaration> stack_trace_;
  Child<Statement> body_;
};

class TryFinally : public Statement {
 public:
  static TryFinally* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~TryFinally();

  DEFINE_CASTING_OPERATIONS(TryFinally);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  Statement* body() { return body_; }
  Statement* finalizer() { return finalizer_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(TryFinally);
  TryFinally() {}

  Child<Statement> body_;
  Child<Statement> finalizer_;
};

class YieldStatement : public Statement {
 public:
  static YieldStatement* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~YieldStatement();

  DEFINE_CASTING_OPERATIONS(YieldStatement);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  bool is_yield_start() { return is_yield_star_; }
  Expression* expression() { return expression_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(YieldStatement);
  YieldStatement() {}

  bool is_yield_star_;
  Child<Expression> expression_;
};

class VariableDeclaration : public Statement {
 public:
  static VariableDeclaration* ReadFrom(Reader* reader);
  static VariableDeclaration* ReadFromImpl(Reader* reader);
  virtual void WriteTo(Writer* writer);
  void WriteToImpl(Writer* writer);

  virtual ~VariableDeclaration();

  DEFINE_CASTING_OPERATIONS(VariableDeclaration);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  word flags() { return flags_; }
  String* name() { return name_; }
  DartType* type() { return type_; }
  Expression* initializer() { return initializer_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(VariableDeclaration);
  VariableDeclaration() {}

  template<typename T>
  friend class List;

  word flags_;
  Ref<String> name_;
  Child<DartType> type_;
  Child<Expression> initializer_;
};

class FunctionDeclaration : public Statement {
 public:
  static FunctionDeclaration* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer);

  virtual ~FunctionDeclaration();

  DEFINE_CASTING_OPERATIONS(FunctionDeclaration);

  virtual void AcceptStatementVisitor(StatementVisitor* visitor);

  VariableDeclaration* variable() { return variable_; }
  FunctionNode* function() { return function_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(FunctionDeclaration);
  FunctionDeclaration() {}

  Child<VariableDeclaration> variable_;
  Child<FunctionNode> function_;
};

class Name : public Node {
 public:
  static Name* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Name();

  DEFINE_CASTING_OPERATIONS(Name);

  virtual void AcceptVisitor(Visitor* visitor);

  String* string() { return string_; }
  Library* library() { return library_; }

 private:
  Name(String* string, Library* library) : string_(string), library_(library) { }

  Ref<String> string_;
  Ref<Library> library_;
};

// TODO: _PrivateName/_PublicName : Name

class DartType : public Node {
 public:
  static DartType* ReadFrom(Reader* reader);
  virtual void WriteTo(Writer* writer) = 0;

  virtual ~DartType();

  DEFINE_CASTING_OPERATIONS(DartType);

  virtual void AcceptVisitor(Visitor* visitor);
  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor) = 0;
};

class InvalidType : public DartType {
 public:
  static InvalidType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~InvalidType();

  DEFINE_CASTING_OPERATIONS(InvalidType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class DynamicType : public DartType {
 public:
  static DynamicType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~DynamicType();

  DEFINE_CASTING_OPERATIONS(DynamicType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class VoidType : public DartType {
 public:
  static VoidType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~VoidType();

  DEFINE_CASTING_OPERATIONS(VoidType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);
};

class InterfaceType : public DartType {
 public:
  static InterfaceType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  InterfaceType(Class* klass) : klass_(klass) {}
  virtual ~InterfaceType();

  DEFINE_CASTING_OPERATIONS(InterfaceType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  Class* klass() { return klass_; }
  List<DartType>& type_arguments() { return type_arguments_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(InterfaceType);
  InterfaceType() {}

  Ref<Class> klass_;
  List<DartType> type_arguments_;
};

class FunctionType : public DartType {
 public:
  static FunctionType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~FunctionType();

  DEFINE_CASTING_OPERATIONS(FunctionType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  List<TypeParameter>& type_parameters() { return type_parameters_; }
  int required_parameter_count() { return required_parameter_count_; }
  List<DartType>& positional_parameters() { return positional_parameters_; }
  List<Tuple<String, DartType> >& named_parameters() { return named_parameters_; }
  DartType* return_type() { return return_type_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(FunctionType);
  FunctionType() {}

  List<TypeParameter> type_parameters_;
  int required_parameter_count_;
  List<DartType> positional_parameters_;
  List<Tuple<String, DartType> > named_parameters_;
  Child<DartType> return_type_;
};

class TypeParameterType : public DartType {
 public:
  static TypeParameterType* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~TypeParameterType();

  DEFINE_CASTING_OPERATIONS(TypeParameterType);

  virtual void AcceptDartTypeVisitor(DartTypeVisitor* visitor);

  TypeParameter* parameter() { return parameter_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(TypeParameterType);
  TypeParameterType() {}

  Ref<TypeParameter> parameter_;
};

class TypeParameter : public TreeNode {
 public:
  static TypeParameter* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~TypeParameter();

  DEFINE_CASTING_OPERATIONS(TypeParameter);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  String* name() { return name_; }
  DartType* bound() { return bound_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(TypeParameter);
  TypeParameter() {}

  template<typename T>
  friend class List;

  Ref<String> name_;
  Child<DartType> bound_;
};

class Program : public TreeNode {
 public:
  static Program* ReadFrom(Reader* reader);
  void WriteTo(Writer* writer);

  virtual ~Program();

  DEFINE_CASTING_OPERATIONS(Program);

  virtual void AcceptTreeVisitor(TreeVisitor* visitor);

  StringTable& string_table() { return string_table_; }
  List<Library>& libraries() { return libraries_; }
  Procedure* main_method() { return main_method_; }

 private:
  DISALLOW_COPY_AND_ASSIGN(Program);
  Program() {}

  List<Library> libraries_;
  Ref<Procedure> main_method_;
  StringTable string_table_;
};

class Reference {
 public:
  static Member* ReadMemberFrom(Reader* reader);
  static void WriteMemberTo(Writer* writer, Member* member);

  static Class* ReadClassFrom(Reader* reader);
  static void WriteClassTo(Writer* writer, Class* klass);

  static String* ReadStringFrom(Reader* reader);
  static void WriteStringTo(Writer* writer, String* string);
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
  virtual void VisitBigintLiteral(BigintLiteral* node) { VisitDefaultBasicLiteral(node); }
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
  delete[] array_;
}

template<typename T>
void List<T>::EnsureInitialized(int length) {
  if (length < length_) return;

  T** old_array = array_;
  int old_length = length_;

  // TODO: Mabe we should use double-growth instead to avoid running into the
  // quadratic case.
  length_ = length;
  array_ = new T*[length_];

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

  delete[] old_array;
}

template<typename T>
template<typename IT>
IT* List<T>::GetOrCreate(int index) {
  EnsureInitialized(index + 1);

  T* member = array_[index];
  if (member == NULL) {
    member = array_[index] = new IT();
  }
  return IT::Cast(member);
}


template<typename T>
template<typename IT, typename PT>
IT* List<T>::GetOrCreate(int index, PT* parent) {
  EnsureInitialized(index + 1);

  T* member = array_[index];
  if (member == NULL) {
    member = array_[index] = new IT();
    member->parent_ = parent;
  } else {
    ASSERT(member->parent_ == parent);
  }
  return IT::Cast(member);
}

}  // namespace dil

dil::Program* ReadPrecompiledDil(const char* filename);
bool WritePrecompiledDil(const char* filename, dil::Program* program);

}  // namespace dart

#endif  // VM_DIL_H_
