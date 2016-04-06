// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <vector>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "vm/dil.h"

#include "platform/signal_blocker.h"

#if 0
#define TRACE_OFFSET() reader->DumpOffset(__PRETTY_FUNCTION__);
#else
#define TRACE_OFFSET()
#endif

namespace dart {

class File {
 public:
  explicit File(const char* filename)
      : filename_(filename), buffer_(NULL), size_(-1) {}

  ~File() {
    delete[] buffer_;
  }

  bool ReadAll() {
    size_ = FileSize();
    if (size_ > 0) {
      // TODO(kustermann): We could consider simply mmap()ing the file, but
      // that might not work so easily on windows.
      int fd = TEMP_FAILURE_RETRY(open(filename_, O_RDONLY));
      if (fd == -1) return false;
      buffer_ = new uint8_t[size_];
      long offset = 0;
      while (offset < size_) {
        ssize_t bytes = TEMP_FAILURE_RETRY(read(fd, buffer_, size_ - offset));
        if (bytes < 0) FATAL("Error during reading dil file.");
        offset += bytes;
      }
      TEMP_FAILURE_RETRY(close(fd));
      return true;
    }
    return false;
  }

  uint8_t* buffer() { return buffer_; }
  long size() { return size_; }

 private:
  int FileSize() {
    struct stat file_stat;
    if (stat(filename_, &file_stat) == 0) {
      return file_stat.st_size;
    }
    return -1;
  }

  const char* filename_;
  uint8_t* buffer_;
  long size_;
};

namespace dil {

static const uint32_t kMagicProgramFile = 0x90ABCDEFu;

// Keep in sync with package:dynamo/lib/binary/tag.dart
enum Tag {
  kNothing = 0,
  kSomething = 1,

  kNormalClass = 2,
  kMixinClass = 3,

  kField = 4,
  kConstructor = 5,
  kProcedure = 6,

  kInvalidInitializer = 7,
  kFieldInitializer = 8,
  kSuperInitializer = 9,
  kRedirectingInitializer = 10,

  kVariableGet = 20,
  kVariableSet = 21,
  kPropertyGet = 22,
  kPropertySet = 23,
  kSuperPropertyGet = 24,
  kSuperPropertySet = 25,
  kStaticGet = 26,
  kStaticSet = 27,
  kMethodInvocation = 28,
  kSuperMethodInvocation = 29,
  kStaticInvocation = 30,
  kFunctionInvocation = 31,
  kConstructorInvocation = 32,
  kNot = 33,
  kLogicalExpression = 34,
  kConditionalExpression = 35,
  kStringConcatenation = 36,
  kIsExpression = 37,
  kAsExpression = 38,
  kStringLiteral = 39,
  kIntLiteral = 40,
  kDoubleLiteral = 41,
  kBoolLiteral = 42,
  kNullLiteral = 43,
  kSymbolLiteral = 44,
  kTypeLiteral = 45,
  kThisExpression = 46,
  kRethrow = 47,
  kThrow = 48,
  kListLiteral = 49,
  kMapLiteral = 50,
  kAwaitExpression = 51,
  kFunctionExpression = 52,
  kLet = 53,
  kBigIntLiteral = 54,

  kInvalidStatement = 60,
  kExpressionStatement = 61,
  kBlock = 62,
  kEmptyStatement = 63,
  kAssertStatement = 64,
  kLabeledStatement = 65,
  kBreakStatement = 66,
  kWhileStatement = 67,
  kDoStatement = 68,
  kForStatement = 69,
  kForInStatement = 70,
  kSwitchStatement = 71,
  kContinueSwitchStatement = 72,
  kIfStatement = 73,
  kReturnStatement = 74,
  kTryCatch = 75,
  kTryFinally = 76,
  kYieldStatement = 77,
  kVariableDeclaration = 78,
  kFunctionDeclaration = 79,

  kInvalidType = 90,
  kDynamicType = 91,
  kVoidType = 92,
  kInterfaceType = 93,
  kFunctionType = 94,
  kTypeParameterType = 95,

  kNormalClassReference = 100,
  kMixinClassReference = 101,

  kLibraryFieldReference = 102,
  kClassFieldReference = 103,
  kClassConstructorReference = 104,
  kLibraryProcedureReference = 105,
  kClassProcedureReference = 106
};

template<typename T>
class BlockStack {
 public:
  BlockStack() : current_count_(0) {}

  void EnterScope() {
    variable_count_.push_back(current_count_);
    current_count_ = 0;
  }

  void LeaveScope() {
    variables_.resize(variables_.size() - current_count_);
    current_count_ = variable_count_[variable_count_.size() - 1];
    variable_count_.pop_back();
  }

  T* Lookup(int index) {
    ASSERT(static_cast<unsigned>(index) < variables_.size());
    return variables_[index];
  }

  void Push(T* v) {
    variables_.push_back(v);
    current_count_++;
  }

  void Pop(T* v) {
    variables_.pop_back();
    current_count_--;
  }

  void Push(List<T>* decl) {
    for (int i = 0; i < decl->length(); i++) {
      variables_.push_back(decl[i]);
      current_count_++;
    }
  }

  void Pop(List<T>* decl) {
    variables_.resize(variables_.size() - decl->length());
    current_count_ -= decl->length();
  }

 private:
  int current_count_;
  std::vector<T*> variables_;
  std::vector<int> variable_count_;
};

class Builder {
 public:
  Builder() : program_(NULL) {}
  ~Builder() {}

  Program* program() { return program_; }
  void set_program(Program* program) { program_ = program; }

  BlockStack<VariableDeclaration>& variables() { return scope_; }
  BlockStack<TypeParameter>& type_parameters() { return type_parameters_; }
  BlockStack<LabeledStatement>& lables() { return labels_; }
  BlockStack<SwitchCase>& switch_cases() { return switch_cases_; }

 private:
  Program* program_;
  BlockStack<VariableDeclaration> scope_;
  BlockStack<TypeParameter> type_parameters_;
  BlockStack<LabeledStatement> labels_;
  BlockStack<SwitchCase> switch_cases_;
};

class VariableScope {
 public:
  VariableScope(Builder* builder) : builder_(builder) {
    builder_->variables().EnterScope();
  }
  ~VariableScope() {
    builder_->variables().LeaveScope();
  }
 private:
  Builder* builder_;
};

class TypeParameterScope {
 public:
  TypeParameterScope(Builder* builder) : builder_(builder) {
    builder_->type_parameters().EnterScope();
  }
  ~TypeParameterScope() {
    builder_->type_parameters().LeaveScope();
  }
 private:
  Builder* builder_;
};

class LabelScope {
 public:
  LabelScope(Builder* builder) : builder_(builder) {
    builder_->lables().EnterScope();
  }
  ~LabelScope() {
    builder_->lables().LeaveScope();
  }
 private:
  Builder* builder_;
};

class SwitchCaseScope {
 public:
  SwitchCaseScope(Builder* builder) : builder_(builder) {
    builder_->switch_cases().EnterScope();
  }
  ~SwitchCaseScope() {
    builder_->switch_cases().LeaveScope();
  }
 private:
  Builder* builder_;
};


class Reader {
 public:
  Reader(uint8_t* buffer, long size)
      : buffer_(buffer), size_(size), offset_(0) {}

  uint32_t ReadUInt32() {
    ASSERT(offset_ + 4 <= size_);

    uint32_t value =
        (buffer_[offset_ + 0] << 24) |
        (buffer_[offset_ + 1] << 16) |
        (buffer_[offset_ + 2] << 8) |
        (buffer_[offset_ + 3] << 0);
    offset_ += 4;
    return value;
  }

  intptr_t ReadListLength() {
    return ReadUInt32();
  }

  uint8_t ReadByte() {
    return buffer_[offset_++];
  }

  bool ReadBool() {
    return (ReadByte() & 1) == 1;
  }

  word ReadFlags() {
    return ReadByte();
  }

  Tag ReadTag() {
    return static_cast<Tag>(ReadByte());
  }

  uint8_t* Consume(int count) {
    ASSERT(offset_ + count <= size_);
    uint8_t* old = buffer_ + offset_;
    offset_ += count;
    return old;
  }

  void EnsureEnd() {
    if (offset_ != size_) {
      FATAL2("Reading Dil file: Expected to be at EOF (offset: %ld, size: %ld)", offset_, size_);
    }
  }

  void DumpOffset(const char* str) {
    printf("@%ld %s\n", offset_, str);
  }

  template<typename T>
  T* ReadOptional() {
    Tag tag = ReadTag();
    if (tag == kNothing) {
      return NULL;
    }
    ASSERT(tag == kSomething);
    return T::ReadFrom(this);
  }

  Builder* builder() { return &builder_; }

 private:
  uint8_t* buffer_;
  long size_;
  long offset_;
  Builder builder_;
};

template<typename T>
template<typename IT>
void List<T>::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  int length = reader->ReadListLength();
  EnsureInitialized(length);

  for (int i = 0; i < length_; i++) {
    GetOrCreate<IT>(i)->ReadFrom(reader);
  }
}

template<typename T>
template<typename IT>
void List<T>::ReadFromStatic(Reader* reader) {
  TRACE_OFFSET();
  int length = reader->ReadListLength();
  EnsureInitialized(length);

  for (int i = 0; i < length_; i++) {
    ASSERT(array_[i] == NULL);
    array_[i] = IT::ReadFrom(reader);
  }
}

template<typename A, typename B>
Tuple<A, B>* Tuple<A, B>::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  A* first = A::ReadFrom(reader);
  B* second = B::ReadFrom(reader);
  return new Tuple<A, B>(first, second);
}

template<typename B, typename S>
class DowncastReader {
 public:
  static S* ReadFrom(Reader* reader) {
    TRACE_OFFSET();
    return static_cast<S*>(B::ReadFrom(reader));
  }
};

String* String::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  uint32_t bytes = reader->ReadUInt32();
  String* string = new String(reader->Consume(bytes), bytes);
  return string;
}

Library* Library::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  name_ = String::ReadFrom(reader);

  int num_classes = reader->ReadUInt32();
  classes().EnsureInitialized(num_classes);
  for (int i = 0; i < num_classes; i++) {
    Tag tag = reader->ReadTag();
    if (tag == kNormalClass) {
      classes().GetOrCreate<NormalClass>(i)->ReadFrom(reader);
    } else {
      ASSERT(tag == kMixinClass);
      classes().GetOrCreate<MixinClass>(i)->ReadFrom(reader);
    }
  }

  fields().ReadFrom<Field>(reader);
  procedures().ReadFrom<Procedure>(reader);
  return this;
}

Class* Class::ReadFrom(Reader* reader) {
  TRACE_OFFSET();

  word flags = reader->ReadFlags();
  is_abstract_ = (flags & 1) != 0;
  name_ = String::ReadFrom(reader);

  return this;
}

NormalClass* NormalClass::ReadFrom(Reader* reader) {
  TRACE_OFFSET();

  // Read base class fields.
  static_cast<Class*>(this)->ReadFrom(reader);

  TypeParameterScope scope(reader->builder());

  type_parameters_.ReadFromStatic<TypeParameter>(reader);
  DartType* type = reader->ReadOptional<DartType>();

  // TODO: Assert for downcasts.
  super_class_ = static_cast<InterfaceType*>(type);
  implemented_classes_.ReadFromStatic<DowncastReader<DartType, InterfaceType> >(reader);
  fields_.ReadFrom<Field>(reader);
  constructors_.ReadFrom<Constructor>(reader);
  procedures_.ReadFrom<Procedure>(reader);

  return this;
}

MixinClass* MixinClass::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  TypeParameterScope scope(reader->builder());
  is_abstract_ = reader->ReadBool();
  name_ = String::ReadFrom(reader);
  type_parameters_.ReadFromStatic<TypeParameter>(reader);
  first_ = static_cast<InterfaceType*>(DartType::ReadFrom(reader));
  second_ = static_cast<InterfaceType*>(DartType::ReadFrom(reader));
  implemented_classes_.ReadFromStatic<InterfaceType>(reader);
  constructors_.ReadFrom<Constructor>(reader);
  return this;
}

Member* Member::ReadFrom(Reader* reader) {
  TRACE_OFFSET();

  Program* program = reader->builder()->program();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kLibraryFieldReference: {
      int library_idx = reader->ReadUInt32();
      int field_idx = reader->ReadUInt32();
      Library* library = program->libraries().GetOrCreate<Library>(library_idx);
      return library->fields().GetOrCreate<Field>(field_idx);
    }
    case kLibraryProcedureReference: {
      int library_idx = reader->ReadUInt32();
      int procedure_idx = reader->ReadUInt32();
      Library* library = program->libraries().GetOrCreate<Library>(library_idx);
      return library->procedures().GetOrCreate<Procedure>(procedure_idx);
    }
    case kClassFieldReference:
    case kClassConstructorReference:
    case kClassProcedureReference: {
      Class* klass = Member::ReadClassFrom(reader);
      if (tag == kClassFieldReference) {
        int field_idx = reader->ReadUInt32();
        return klass->fields().GetOrCreate<Field>(field_idx);
      } else if (tag == kClassConstructorReference) {
        int constructor_idx = reader->ReadUInt32();
        return klass->constructors().GetOrCreate<Constructor>(constructor_idx);
      } else {
        ASSERT(tag == kClassProcedureReference);
        int procedure_idx = reader->ReadUInt32();
        return klass->procedures().GetOrCreate<Procedure>(procedure_idx);
      }
    }
    default:
      UNREACHABLE();
      break;
  }

  UNREACHABLE();
  return NULL;
}

Class* Member::ReadClassFrom(Reader* reader) {
  TRACE_OFFSET();
  Program* program = reader->builder()->program();

  Tag klass_member_tag = reader->ReadTag();
  int library_idx = reader->ReadUInt32();
  int class_idx = reader->ReadUInt32();

  Library* library = program->libraries().GetOrCreate<Library>(library_idx);
  Class* klass;
  if (klass_member_tag == kNormalClassReference) {
    klass = library->classes().GetOrCreate<NormalClass>(class_idx);
  } else {
    ASSERT(klass_member_tag == kMixinClassReference);
    klass = library->classes().GetOrCreate<MixinClass>(class_idx);
  }
  return klass;
}

Field* Field::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kField);

  // TODO: Do we need a variable scop here?
  flags_ = reader->ReadFlags();
  name_ = Name::ReadFrom(reader);
  type_ = DartType::ReadFrom(reader);
  initializer_ = reader->ReadOptional<Expression>();
  return this;
}

Constructor* Constructor::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kConstructor);

  VariableScope vars(reader->builder());
  is_const_ = (reader->ReadFlags() & 1) == 1;
  name_ = Name::ReadFrom(reader);
  function_ = FunctionNode::ReadFrom(reader);
  initializers_.ReadFromStatic<Initializer>(reader);
  return this;
}

Procedure* Procedure::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kProcedure);

  VariableScope vars(reader->builder());
  kind_ = static_cast<ProcedureKind>(reader->ReadByte());
  flags_ = reader->ReadFlags();
  name_ = Name::ReadFrom(reader);
  function_ = reader->ReadOptional<FunctionNode>();
  return this;
}

Initializer* Initializer::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kInvalidInitializer:
      return InvalidInitializer::ReadFrom(reader);
    case kFieldInitializer:
      return FieldInitializer::ReadFromImpl(reader);
    case kSuperInitializer:
      return SuperInitializer::ReadFrom(reader);
    case kRedirectingInitializer:
      return RedirectingInitializer::ReadFrom(reader);
    default:
      UNREACHABLE();
  }
  return NULL;
}

InvalidInitializer* InvalidInitializer::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new InvalidInitializer();
}

FieldInitializer* FieldInitializer::ReadFromImpl(Reader* reader) {
  TRACE_OFFSET();
  FieldInitializer* initializer = new FieldInitializer();
  initializer->field_ = static_cast<Field*>(Member::ReadFrom(reader));
  initializer->value_ = Expression::ReadFrom(reader);
  return initializer;
}

SuperInitializer* SuperInitializer::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SuperInitializer* init = new SuperInitializer();
  init->target_ = static_cast<Constructor*>(Member::ReadFrom(reader));
  init->arguments_ = Arguments::ReadFrom(reader);
  return init;
}

RedirectingInitializer* RedirectingInitializer::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  RedirectingInitializer* init = new RedirectingInitializer();
  init->target_ = static_cast<Constructor*>(Member::ReadFrom(reader));
  init->arguments_ = Arguments::ReadFrom(reader);
  return init;
}

Expression* Expression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kVariableGet:
      return VariableGet::ReadFrom(reader);
    case kVariableSet:
      return VariableSet::ReadFrom(reader);
    case kPropertyGet:
      return PropertyGet::ReadFrom(reader);
    case kPropertySet:
      return PropertySet::ReadFrom(reader);
    case kSuperPropertyGet:
      return SuperPropertyGet::ReadFrom(reader);
    case kSuperPropertySet:
      return SuperPropertySet::ReadFrom(reader);
    case kStaticGet:
      return StaticGet::ReadFrom(reader);
    case kStaticSet:
      return StaticSet::ReadFrom(reader);
    case kMethodInvocation:
      return MethodInvocation::ReadFrom(reader);
    case kSuperMethodInvocation:
      return SuperMethodInvocation::ReadFrom(reader);
    case kStaticInvocation:
      return StaticInvocation::ReadFrom(reader);
    case kFunctionInvocation:
      return FunctionInvocation::ReadFrom(reader);
    case kConstructorInvocation:
      return ConstructorInvocation::ReadFrom(reader);
    case kNot:
      return Not::ReadFrom(reader);
    case kLogicalExpression:
      return LogicalExpression::ReadFrom(reader);
    case kConditionalExpression:
      return ConditionalExpression::ReadFrom(reader);
    case kStringConcatenation:
      return StringConcatenation::ReadFrom(reader);
    case kIsExpression:
      return IsExpression::ReadFrom(reader);
    case kAsExpression:
      return AsExpression::ReadFrom(reader);
    case kSymbolLiteral:
      return SymbolLiteral::ReadFrom(reader);
    case kTypeLiteral:
      return TypeLiteral::ReadFrom(reader);
    case kThisExpression:
      return ThisExpression::ReadFrom(reader);
    case kRethrow:
      return Rethrow::ReadFrom(reader);
    case kThrow:
      return Throw::ReadFrom(reader);
    case kListLiteral:
      return ListLiteral::ReadFrom(reader);
    case kMapLiteral:
      return MapLiteral::ReadFrom(reader);
    case kAwaitExpression:
      return AwaitExpression::ReadFrom(reader);
    case kFunctionExpression:
      return FunctionExpression::ReadFrom(reader);
    case kLet:
      return Let::ReadFrom(reader);
    case kBigIntLiteral:
      // TODO: At some point we probably want to represent bigints as proper
      // integers.
      return StringLiteral::ReadFrom(reader);
    case kStringLiteral:
      return StringLiteral::ReadFrom(reader);
    case kIntLiteral:
      return IntLiteral::ReadFrom(reader);
    case kDoubleLiteral:
      return DoubleLiteral::ReadFrom(reader);
    case kBoolLiteral:
      return BoolLiteral::ReadFrom(reader);
    case kNullLiteral:
      return NullLiteral::ReadFrom(reader);
    default:
      UNREACHABLE();
  }
  return NULL;
}

InvalidExpression* InvalidExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new InvalidExpression();
}

VariableGet* VariableGet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableGet* get = new VariableGet();
  get->variable_ = reader->builder()->variables().Lookup(reader->ReadUInt32());
  return get;
}

VariableSet* VariableSet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableSet* set = new VariableSet();
  set->variable_ = reader->builder()->variables().Lookup(reader->ReadUInt32());
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

PropertyGet* PropertyGet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  PropertyGet* get = new PropertyGet();
  get->receiver_ = Expression::ReadFrom(reader);
  get->name_ = Name::ReadFrom(reader);
  return get;
}

PropertySet* PropertySet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  PropertySet* set = new PropertySet();
  set->receiver_ = Expression::ReadFrom(reader);
  set->name_ = Name::ReadFrom(reader);
  set->value_= Expression::ReadFrom(reader);
  return set;
}

SuperPropertyGet* SuperPropertyGet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SuperPropertyGet* get = new SuperPropertyGet();
  get->target_ = Member::ReadFrom(reader);
  return get;
}

SuperPropertySet* SuperPropertySet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SuperPropertySet* set = new SuperPropertySet();
  set->target_ = Member::ReadFrom(reader);
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

StaticGet* StaticGet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  StaticGet* get = new StaticGet();
  get->target_ = Member::ReadFrom(reader);
  return get;
}

StaticSet* StaticSet::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  StaticSet* set = new StaticSet();
  set->target_ = Member::ReadFrom(reader);
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

Arguments* Arguments::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Arguments* arguments = new Arguments();
  arguments->types().ReadFromStatic<DartType>(reader);
  arguments->positional().ReadFromStatic<Expression>(reader);
  arguments->named().ReadFromStatic<NamedExpression>(reader);
  return arguments;
}

NamedExpression* NamedExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  String* name = String::ReadFrom(reader);
  Expression* expression = Expression::ReadFrom(reader);
  return new NamedExpression(name, expression);
}

MethodInvocation* MethodInvocation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  MethodInvocation* invocation = new MethodInvocation();
  invocation->receiver_ = Expression::ReadFrom(reader);
  invocation->name_ = Name::ReadFrom(reader);
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

SuperMethodInvocation* SuperMethodInvocation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SuperMethodInvocation* invocation = new SuperMethodInvocation();
  invocation->target_ = static_cast<Procedure*>(Member::ReadFrom(reader));
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

StaticInvocation* StaticInvocation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();

  Member* member = Member::ReadFrom(reader);
  Arguments* args = Arguments::ReadFrom(reader);

  // TODO(kustermann): Introduce dynamic assertions for downcasts.
  return new StaticInvocation(static_cast<Procedure*>(member), args);
}

FunctionInvocation* FunctionInvocation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  FunctionInvocation* invocation = new FunctionInvocation();
  invocation->function_ = Expression::ReadFrom(reader);
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

ConstructorInvocation* ConstructorInvocation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  ConstructorInvocation* invocation = new ConstructorInvocation();
  invocation->is_const_ = reader->ReadBool();
  invocation->target_ = static_cast<Constructor*>(Member::ReadFrom(reader));
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

Not* Not::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Not* n = new Not();
  n->expression_ = Expression::ReadFrom(reader);
  return n;
}

LogicalExpression* LogicalExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  LogicalExpression* expr = new LogicalExpression();
  expr->left_ = Expression::ReadFrom(reader);
  expr->operator_ = reader->ReadByte();
  expr->right_ = Expression::ReadFrom(reader);
  return expr;
}

ConditionalExpression* ConditionalExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  ConditionalExpression* expr = new ConditionalExpression();
  expr->condition_ = Expression::ReadFrom(reader);
  expr->then_ = Expression::ReadFrom(reader);
  expr->otherwise_ = Expression::ReadFrom(reader);
  return expr;
}

StringConcatenation* StringConcatenation::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  StringConcatenation* concat = new StringConcatenation();
  concat->expressions_.ReadFromStatic<Expression>(reader);
  return concat;
}

IsExpression* IsExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  IsExpression* expr = new IsExpression();
  expr->operand_ = Expression::ReadFrom(reader);
  expr->type_ = DartType::ReadFrom(reader);
  return expr;
}

AsExpression* AsExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  AsExpression* expr = new AsExpression();
  expr->operand_ = Expression::ReadFrom(reader);
  expr->type_ = DartType::ReadFrom(reader);
  return expr;
}

StringLiteral* StringLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new StringLiteral(String::ReadFrom(reader));
}

IntLiteral* IntLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  IntLiteral* literal = new IntLiteral();
  bool negative = reader->ReadBool();
  literal->value_ = negative ? -reader->ReadUInt32() : reader->ReadUInt32();
  return literal;
}

DoubleLiteral* DoubleLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  DoubleLiteral* literal = new DoubleLiteral();
  literal->value_ = String::ReadFrom(reader);
  return literal;
}

BoolLiteral* BoolLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  BoolLiteral* lit = new BoolLiteral();
  lit->value_ = reader->ReadBool();
  return lit;
}

NullLiteral* NullLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new NullLiteral();
}

SymbolLiteral* SymbolLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SymbolLiteral* lit = new SymbolLiteral();
  lit->value_ = String::ReadFrom(reader);
  return lit;
}

TypeLiteral* TypeLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  TypeLiteral* literal = new TypeLiteral();
  literal->type_ = DartType::ReadFrom(reader);
  return literal;
}

ThisExpression* ThisExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new ThisExpression();
}

Rethrow* Rethrow::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new Rethrow();
}

Throw* Throw::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Throw* t = new Throw();
  t->expression_ = Expression::ReadFrom(reader);
  return t;
}

ListLiteral* ListLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  ListLiteral* literal = new ListLiteral();
  literal->is_const_ = reader->ReadBool();
  literal->type_ = DartType::ReadFrom(reader);
  literal->expressions_.ReadFromStatic<Expression>(reader);
  return literal;
}

MapLiteral* MapLiteral::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  MapLiteral* literal = new MapLiteral();
  literal->is_const_ = reader->ReadBool();
  literal->key_type_ = DartType::ReadFrom(reader);
  literal->value_type_ = DartType::ReadFrom(reader);
  literal->entries_.ReadFromStatic<MapEntry>(reader);
  return literal;
}

MapEntry* MapEntry::ReadFrom(Reader* reader) {
  MapEntry* entry = new MapEntry();
  entry->key_ = Expression::ReadFrom(reader);
  entry->value_ = Expression::ReadFrom(reader);
  return entry;
}

AwaitExpression* AwaitExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  AwaitExpression* await = new AwaitExpression();
  await->operand_ = Expression::ReadFrom(reader);
  return await;
}

FunctionExpression* FunctionExpression::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  FunctionExpression* expr = new FunctionExpression();
  expr->function_ = FunctionNode::ReadFrom(reader);
  return expr;
}

Let* Let::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  Let* let = new Let();
  let->variable_ = VariableDeclaration::ReadFrom(reader);
  let->body_ = Expression::ReadFrom(reader);
  return let;
}

Statement* Statement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kInvalidStatement:
      return InvalidStatement::ReadFrom(reader);
    case kExpressionStatement:
      return ExpressionStatement::ReadFrom(reader);
    case kBlock:
      return Block::ReadFrom(reader);
    case kEmptyStatement:
      return EmptyStatement::ReadFrom(reader);
    case kAssertStatement:
      return AssertStatement::ReadFrom(reader);
    case kLabeledStatement:
      return LabeledStatement::ReadFrom(reader);
    case kBreakStatement:
      return BreakStatement::ReadFrom(reader);
    case kWhileStatement:
      return WhileStatement::ReadFrom(reader);
    case kDoStatement:
      return DoStatement::ReadFrom(reader);
    case kForStatement:
      return ForStatement::ReadFrom(reader);
    case kForInStatement:
      return ForInStatement::ReadFrom(reader);
    case kSwitchStatement:
      return SwitchStatement::ReadFrom(reader);
    case kContinueSwitchStatement:
      return ContinueSwitchStatement::ReadFrom(reader);
    case kIfStatement:
      return IfStatement::ReadFrom(reader);
    case kReturnStatement:
      return ReturnStatement::ReadFrom(reader);
    case kTryCatch:
      return TryCatch::ReadFrom(reader);
    case kTryFinally:
      return TryFinally::ReadFrom(reader);
    case kYieldStatement:
      return YieldStatement::ReadFrom(reader);
    case kVariableDeclaration:
      return VariableDeclaration::ReadFromImpl(reader);
    case kFunctionDeclaration:
      return FunctionDeclaration::ReadFrom(reader);
    default:
      UNREACHABLE();
  }
  return NULL;
}

InvalidStatement* InvalidStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new InvalidStatement();
}

ExpressionStatement* ExpressionStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new ExpressionStatement(Expression::ReadFrom(reader));
}

Block* Block::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  Block* block = new Block();
  block->statements().ReadFromStatic<Statement>(reader);
  return block;
}

EmptyStatement* EmptyStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new EmptyStatement();
}

AssertStatement* AssertStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  AssertStatement* stmt = new AssertStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  stmt->message_ = reader->ReadOptional<Expression>();
  return stmt;
}

LabeledStatement* LabeledStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  LabeledStatement* stmt = new LabeledStatement();
  reader->builder()->lables().Push(stmt);
  stmt->body_ = Statement::ReadFrom(reader);
  reader->builder()->lables().Pop(stmt);
  return stmt;
}

BreakStatement* BreakStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  BreakStatement* stmt = new BreakStatement();
  stmt->target_ = reader->builder()->lables().Lookup(reader->ReadUInt32());
  return stmt;
}

WhileStatement* WhileStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  WhileStatement* stmt = new WhileStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  stmt->body_ = Statement::ReadFrom(reader);
  return stmt;
}

DoStatement* DoStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  DoStatement* dostmt = new DoStatement();
  dostmt->body_ = Statement::ReadFrom(reader);
  dostmt->condition_ = Expression::ReadFrom(reader);
  return dostmt;
}

ForStatement* ForStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  ForStatement* forstmt = new ForStatement();
  forstmt->variables_.ReadFromStatic<VariableDeclaration>(reader);
  forstmt->condition_ = reader->ReadOptional<Expression>();
  forstmt->updates_.ReadFromStatic<Expression>(reader);
  forstmt->body_ = Statement::ReadFrom(reader);
  return forstmt;
}

ForInStatement* ForInStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  ForInStatement* forinstmt = new ForInStatement();
  forinstmt->is_async_ = reader->ReadBool();
  forinstmt->variable_ = VariableDeclaration::ReadFrom(reader);
  forinstmt->iterable_ = Expression::ReadFrom(reader);
  forinstmt->body_ = Statement::ReadFrom(reader);
  return forinstmt;
}

SwitchStatement* SwitchStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  SwitchCaseScope scope(reader->builder());
  SwitchStatement* stmt = new SwitchStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  // We need to explicitly create empty [SwitchCase]s first in order to add them
  // to the [SwitchCaseScope]. This is necessary since a [Statement] in a switch
  // case can refer to one defined later on.
  int count = reader->ReadUInt32();
  for (int i = 0; i < count; i++) {
    SwitchCase* sc = stmt->cases_.GetOrCreate<SwitchCase>(i);
    reader->builder()->switch_cases().Push(sc);
  }
  for (int i = 0; i < count; i++) {
    SwitchCase* sc = stmt->cases_[i];
    sc->ReadFrom(reader);
  }
  return stmt;
}

SwitchCase* SwitchCase::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  expressions_.ReadFromStatic<Expression>(reader);
  is_default_ = reader->ReadBool();
  body_ = Statement::ReadFrom(reader);
  return this;
}

ContinueSwitchStatement* ContinueSwitchStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  ContinueSwitchStatement* stmt =  new ContinueSwitchStatement();
  stmt->target_ = reader->builder()->switch_cases().Lookup(reader->ReadUInt32());
  return stmt;
}

IfStatement* IfStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  IfStatement* ifstmt = new IfStatement();
  ifstmt->condition_ = Expression::ReadFrom(reader);
  ifstmt->then_= Statement::ReadFrom(reader);
  ifstmt->otherwise_ = Statement::ReadFrom(reader);
  return ifstmt;
}

ReturnStatement* ReturnStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  ReturnStatement* ret = new ReturnStatement();
  ret->expression_ = reader->ReadOptional<Expression>();
  return ret;
}

TryCatch* TryCatch::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  TryCatch* tc = new TryCatch();
  tc->body_ = Statement::ReadFrom(reader);
  tc->catches_.ReadFromStatic<Catch>(reader);
  return tc;
}

Catch* Catch::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  VariableScope vars(reader->builder());
  Catch* c = new Catch();
  c->guard_ = reader->ReadOptional<DartType>();
  c->exception_ = reader->ReadOptional<VariableDeclaration>();
  c->stack_trace_ = reader->ReadOptional<VariableDeclaration>();
  c->body_ = Statement::ReadFrom(reader);
  return c;
}

TryFinally* TryFinally::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  TryFinally* tf = new TryFinally();
  tf->body_ = Statement::ReadFrom(reader);
  tf->finalizer_ = Statement::ReadFrom(reader);
  return tf;
}

YieldStatement* YieldStatement::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  YieldStatement* stmt = new YieldStatement();
  stmt->is_yield_star_ = reader->ReadBool();
  stmt->expression_ = Expression::ReadFrom(reader);
  return stmt;
}

VariableDeclaration* VariableDeclaration::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kVariableDeclaration);
  return VariableDeclaration::ReadFromImpl(reader);
}

VariableDeclaration* VariableDeclaration::ReadFromImpl(Reader* reader) {
  TRACE_OFFSET();
  VariableDeclaration* decl = new VariableDeclaration();
  reader->builder()->variables().Push(decl);
  decl->flags_ = reader->ReadFlags();
  decl->name_ = String::ReadFrom(reader);
  decl->type_ = reader->ReadOptional<DartType>();
  decl->initializer_ = reader->ReadOptional<Expression>();
  return decl;
}

FunctionDeclaration* FunctionDeclaration::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  FunctionDeclaration* decl = new FunctionDeclaration();
  decl->variable_ = VariableDeclaration::ReadFrom(reader);
  decl->function_ = FunctionNode::ReadFrom(reader);
  return decl;
}

Name* Name::ReadFrom(Reader* reader) {
  String* string = String::ReadFrom(reader);
  int lib_index = reader->ReadUInt32();
  return new Name(string, lib_index);
}

DartType* DartType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kInvalidType:
      return InvalidType::ReadFrom(reader);
    case kDynamicType:
      return DynamicType::ReadFrom(reader);
    case kVoidType:
      return VoidType::ReadFrom(reader);
    case kInterfaceType:
      return InterfaceType::ReadFrom(reader);
    case kFunctionType:
      return FunctionType::ReadFrom(reader);
    case kTypeParameterType:
      return TypeParameterType::ReadFrom(reader);
    default:
      UNREACHABLE();
  }
  UNREACHABLE();
  return NULL;
}

InvalidType* InvalidType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new InvalidType();
}

DynamicType* DynamicType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new DynamicType();
}

VoidType* VoidType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  return new VoidType();
}

InterfaceType* InterfaceType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  Class* klass = Member::ReadClassFrom(reader);
  InterfaceType* type = new InterfaceType(klass);
  type->type_arguments().ReadFromStatic<DartType>(reader);
  return type;
}

FunctionType* FunctionType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  FunctionType* type = new FunctionType();
  type->type_parameters().ReadFromStatic<TypeParameter>(reader);
  type->required_parameter_count_ = reader->ReadUInt32();
  type->positional_parameters().ReadFromStatic<DartType>(reader);
  type->named_parameters().ReadFromStatic<Tuple<String, DartType> >(reader);
  type->return_type_ = DartType::ReadFrom(reader);
  return type;
}

TypeParameterType* TypeParameterType::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  TypeParameterType* type = new TypeParameterType();
  type->parameter_ = reader->builder()->type_parameters().Lookup(reader->ReadUInt32());
  return type;
}

Program* Program::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  uint32_t magic = reader->ReadUInt32();
  if (magic != kMagicProgramFile) FATAL("Invalid magic identifier");

  Program* program = new Program();
  int libraries = reader->ReadUInt32();
  program->libraries().EnsureInitialized(libraries);

  reader->builder()->set_program(program);

  for (int i = 0; i < libraries; i++) {
    program->libraries().GetOrCreate<Library>(i)->ReadFrom(reader);
  }

  return program;
}

FunctionNode* FunctionNode::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  FunctionNode* function = new FunctionNode();
  function->type_parameters().ReadFromStatic<TypeParameter>(reader);
  function->required_parameter_count_ = reader->ReadUInt32();
  function->positional_parameters().ReadFromStatic<VariableDeclaration>(reader);
  function->named_parameters().ReadFromStatic<VariableDeclaration>(reader);
  function->return_type_ = reader->ReadOptional<DartType>();

  VariableScope vars(reader->builder());
  function->body_ = reader->ReadOptional<Statement>();
  return function;
}

TypeParameter* TypeParameter::ReadFrom(Reader* reader) {
  TRACE_OFFSET();
  TypeParameter* param = new TypeParameter();
  reader->builder()->type_parameters().Push(param);
  param->name_ = String::ReadFrom(reader);
  param->bound_ = DartType::ReadFrom(reader);
  return param;
}

}  // namespace dil

dil::Program* GetPrecompiledDil(const char* filename) {
  if (filename != NULL) {
    File file(filename);
    if (file.ReadAll()) {
      dil::Reader reader(file.buffer(), file.size());
      return dil::Program::ReadFrom(&reader);
    }
  }
  return NULL;
}

}  // namespace dart
