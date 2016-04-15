// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <vector>
#include <map>
#include <memory>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "vm/dil.h"

#include "platform/signal_blocker.h"

#if 0
#define TRACE_READ_OFFSET() reader->DumpOffset(__PRETTY_FUNCTION__);
#define TRACE_WRITE_OFFSET() writer->DumpOffset(__PRETTY_FUNCTION__);
#else
#define TRACE_READ_OFFSET()
#define TRACE_WRITE_OFFSET()
#endif

namespace dart {

class InputFile {
 public:
  explicit InputFile(const char* filename)
      : filename_(filename), buffer_(NULL), size_(-1) {}

  ~InputFile() {
    delete[] buffer_;
  }

  bool ReadAll() {
    size_ = FileSize();
    if (size_ > 0) {
      // TODO(kustermann): We could consider simply mmap()ing the file, but
      // that might not work so easily on windows.
      int fd = TEMP_FAILURE_RETRY(open(filename_, O_RDONLY));
      if (fd < 0) return false;
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

class OutputFile {
 public:
  explicit OutputFile(const char* filename)
      : filename_(filename), fd_(-1), offset_(0) {}

  ~OutputFile() {
    if (fd_ >= 0) {
      Flush();
      TEMP_FAILURE_RETRY(close(fd_));
    }
  }

  bool Open() {
    fd_ = TEMP_FAILURE_RETRY(open(filename_, O_CREAT | O_WRONLY | O_TRUNC, 0666));
    return fd_ >= 0;
  }

  void WriteByte(uint8_t byte) {
    if (offset_ >= kBufferSize) {
      Flush();
    }
    buffer_[offset_++] = byte;
  }

  void WriteBytes(uint8_t* buffer, int count) {
    while (count > 0) {
      if (offset_ >= kBufferSize) {
        Flush();
      }
      int space = kBufferSize - offset_;
      int to_write = space >= count ? count : space;

      memcpy(buffer_ + offset_, buffer, to_write);
      buffer += to_write;
      count -= to_write;
      offset_ += to_write;
    }
  }

  void Flush() {
    uint8_t* buffer = buffer_;
    int remaining = offset_;
    while (remaining > 0) {
      int result = TEMP_FAILURE_RETRY(write(fd_, buffer, remaining));
      if (result <= 0) FATAL("Something went wrong when writing");
      remaining -= result;
      buffer += result;
    }
    offset_ = 0;
  }

 private:
  static const int kBufferSize = 32 * 1024;

  const char* filename_;
  int fd_;
  uint8_t buffer_[kBufferSize];
  intptr_t offset_;
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

  kInvalidExpression = 19,
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

  void Push(List<T>* decl) {
    for (int i = 0; i < decl->length(); i++) {
      variables_.push_back(decl[i]);
      current_count_++;
    }
  }

  void Pop(T* decl) {
    variables_.resize(variables_.size() - 1);
    current_count_--;
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

template<typename T>
class BlockMap {
 public:
  BlockMap() : current_count_(0), stack_height_(0) {}

  void EnterScope() {
    variable_count_.push_back(current_count_);
    current_count_ = 0;
  }

  void LeaveScope() {
    stack_height_ -= current_count_;
    current_count_ = variable_count_[variable_count_.size() - 1];
    variable_count_.pop_back();
  }

  int Lookup(T* object) {
    ASSERT(variables_.find(object) != variables_.end());
    if (variables_.find(object) == variables_.end()) FATAL("lookup failure");
    return variables_[object];
  }

  void Push(T* v) {
    int index = stack_height_++;
    variables_[v] = index;
    current_count_++;
  }

  void Set(T* v, int index) {
    variables_[v] = index;
  }

  void Push(List<T>* decl) {
    for (int i = 0; i < decl->length(); i++) {
      Push(decl[i]);
    }
  }

  void Pop(T* v) {
    current_count_--;
    stack_height_--;
  }

 private:
  int current_count_;
  int stack_height_;
  std::map<T*, int> variables_;
  std::vector<int> variable_count_;
};

template<typename T>
class VariableScope {
 public:
  VariableScope(T* builder) : builder_(builder) {
    builder_->variables().EnterScope();
  }
  ~VariableScope() {
    builder_->variables().LeaveScope();
  }
 private:
  T* builder_;
};

template<typename T>
class TypeParameterScope {
 public:
  TypeParameterScope(T* builder) : builder_(builder) {
    builder_->type_parameters().EnterScope();
  }
  ~TypeParameterScope() {
    builder_->type_parameters().LeaveScope();
  }
 private:
  T* builder_;
};

template<typename T>
class SwitchCaseScope {
 public:
  SwitchCaseScope(T* builder) : builder_(builder) {
    builder_->switch_cases().EnterScope();
  }
  ~SwitchCaseScope() {
    builder_->switch_cases().LeaveScope();
  }
 private:
  T* builder_;
};

class ReaderHelper {
 public:
  ReaderHelper() : program_(NULL) {}
  ~ReaderHelper() {}

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

  uint32_t ReadUInt31() {
    ASSERT(offset_ + 1 <= size_);

    uint8_t byte0 = buffer_[offset_];
    if ((byte0 & 0x80) == 0) {
      offset_++;
      return byte0;
    }

    ASSERT(offset_ + 4 <= size_);

    uint32_t value =
        ((byte0 & ~0x80) << 24) |
        (buffer_[offset_ + 1] << 16) |
        (buffer_[offset_ + 2] << 8) |
        (buffer_[offset_ + 3] << 0);
    offset_ += 4;
    return value;
  }

  intptr_t ReadListLength() {
    return ReadUInt31();
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
    fprintf(stderr, "@%ld %s\n", offset_, str);
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

  ReaderHelper* helper() { return &builder_; }

 private:
  uint8_t* buffer_;
  long size_;
  long offset_;
  ReaderHelper builder_;
};

class WriterHelper {
 public:
  void SetProgram(Program* program) {
    program_ = program;
    for (int i = 0; i < program->libraries().length(); i++) {
      Library* lib = program->libraries()[i];
      libraries_.Set(lib, i);

      for (int j = 0; j < lib->classes().length(); j++) {
        Class* klass = lib->classes()[j];
        classes_.Set(klass, j);

        for (int k = 0; k < klass->fields().length(); k++) {
          Field* field = klass->fields()[k];
          fields_.Set(field, k);
        }
        for (int k = 0; k < klass->constructors().length(); k++) {
          Constructor* constructor = klass->constructors()[k];
          constructors_.Set(constructor, k);
        }
        for (int k = 0; k < klass->procedures().length(); k++) {
          Procedure* procedure = klass->procedures()[k];
          procedures_.Set(procedure, k);
        }
      }

      for (int k = 0; k < lib->fields().length(); k++) {
        Field* field = lib->fields()[k];
        fields_.Set(field, k);
      }

      for (int k = 0; k < lib->procedures().length(); k++) {
        Procedure* procedure = lib->procedures()[k];
        procedures_.Set(procedure, k);
      }
    }
  }

  Program* program() { return program_; }

  BlockMap<Library>& libraries() { return libraries_; }
  BlockMap<Class>& classes() { return classes_; }
  BlockMap<Field>& fields() { return fields_; }
  BlockMap<Procedure>& procedures() { return procedures_; }
  BlockMap<Constructor>& constructors() { return constructors_; }

  BlockMap<VariableDeclaration>& variables() { return scope_; }
  BlockMap<TypeParameter>& type_parameters() { return type_parameters_; }
  BlockMap<LabeledStatement>& lables() { return labels_; }
  BlockMap<SwitchCase>& switch_cases() { return switch_cases_; }

 private:
  Program* program_;

  BlockMap<Library> libraries_;
  BlockMap<Class> classes_;
  BlockMap<Field> fields_;
  BlockMap<Procedure> procedures_;
  BlockMap<Constructor> constructors_;

  BlockMap<VariableDeclaration> scope_;
  BlockMap<TypeParameter> type_parameters_;
  BlockMap<LabeledStatement> labels_;
  BlockMap<SwitchCase> switch_cases_;
};

class Writer {
 public:
  Writer(OutputFile* file) : out_(file), offset_(0) {}

  void WriteUInt32(uint32_t value) {
    uint8_t buffer[4] = {
      static_cast<uint8_t>((value >> 24) & 0xff),
      static_cast<uint8_t>((value >> 16) & 0xff),
      static_cast<uint8_t>((value >> 8) & 0xff),
      static_cast<uint8_t>((value >> 0) & 0xff),
    };
    WriteBytes(buffer, 4);
  }

  void WriteUInt31(uint32_t value) {
    if (value < 0x80) {
      WriteByte(static_cast<uint8_t>(value));
    } else {
      // Ensure the highest bit is not used for anything (we use it to for
      // encoding).
      ASSERT(static_cast<uint8_t>((value >> 24) & 0x80) == 0);
      uint8_t buffer[4] = {
        static_cast<uint8_t>(((value >> 24) & 0x7f) | 0x80),
        static_cast<uint8_t>((value >> 16) & 0xff),
        static_cast<uint8_t>((value >> 8) & 0xff),
        static_cast<uint8_t>((value >> 0) & 0xff),
      };
      WriteBytes(buffer, 4);
    }
  }

  void WriteListLength(intptr_t value) {
    return WriteUInt31(value);
  }

  void WriteByte(uint8_t value) {
    out_->WriteByte(value);
    offset_++;
  }

  void WriteBool(bool value) {
    WriteByte(value ? 1 : 0);
  }

  void WriteFlags(uint8_t value) {
    WriteByte(value);
  }

  void WriteTag(Tag tag) {
    WriteByte(static_cast<uint8_t>(tag));
  }

  void WriteBytes(uint8_t* bytes, int length) {
    out_->WriteBytes(bytes, length);
    offset_ += length;
  }

  template<typename T>
  void WriteOptional(T* object) {
    if (object == NULL) {
      WriteTag(kNothing);
    } else {
      WriteTag(kSomething);
      object->WriteTo(this);
    }
  }

  void DumpOffset(const char* str) {
    fprintf(stderr, "@%ld %s\n", offset_, str);
  }

  WriterHelper* helper() { return &helper_; }

  void Flush() { out_->Flush(); }

 private:
  OutputFile* out_;
  WriterHelper helper_;
  long offset_;
};

template<typename T>
template<typename IT>
void List<T>::ReadFrom(Reader* reader, TreeNode* parent) {
  TRACE_READ_OFFSET();
  ASSERT(parent != NULL);
  int length = reader->ReadListLength();
  EnsureInitialized(length);

  for (int i = 0; i < length_; i++) {
    IT* object = GetOrCreate<IT>(i, parent);
    object->ReadFrom(reader);
  }
}

template<typename T>
template<typename IT>
void List<T>::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  int length = reader->ReadListLength();
  EnsureInitialized(length);

  for (int i = 0; i < length_; i++) {
    GetOrCreate<IT>(i)->ReadFrom(reader);
  }
}

template<typename T>
template<typename IT>
void List<T>::ReadFromStatic(Reader* reader) {
  TRACE_READ_OFFSET();
  int length = reader->ReadListLength();
  EnsureInitialized(length);

  for (int i = 0; i < length_; i++) {
    ASSERT(array_[i] == NULL);
    array_[i] = IT::ReadFrom(reader);
  }
}

template<typename T>
void List<T>::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();

  // NOTE: We only support dense lists.
  writer->WriteListLength(length_);
  for (int i = 0; i < length_; i++) {
    T* object = array_[i];
    ASSERT(object != NULL);
    object->WriteTo(writer);
  }
}

template<typename A, typename B>
Tuple<A, B>* Tuple<A, B>::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  A* first = A::ReadFrom(reader);
  B* second = B::ReadFrom(reader);
  return new Tuple<A, B>(first, second);
}

template<typename A, typename B>
void Tuple<A, B>::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  first_->WriteTo(writer);
  second_->WriteTo(writer);
}

template<typename B, typename S>
class DowncastReader {
 public:
  static S* ReadFrom(Reader* reader) {
    TRACE_READ_OFFSET();
    return S::Cast(B::ReadFrom(reader));
  }
};

String* String::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  uint32_t bytes = reader->ReadUInt31();
  String* string = new String(reader->Consume(bytes), bytes);
  return string;
}

void String::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteUInt31(size_);
  writer->WriteBytes(buffer_, size_);
}

Library* Library::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  name_ = String::ReadFrom(reader);

  int num_classes = reader->ReadUInt31();
  classes().EnsureInitialized(num_classes);
  for (int i = 0; i < num_classes; i++) {
    Tag tag = reader->ReadTag();
    if (tag == kNormalClass) {
      NormalClass* klass = classes().GetOrCreate<NormalClass>(i, this);
      klass->ReadFrom(reader);
    } else {
      ASSERT(tag == kMixinClass);
      MixinClass* klass = classes().GetOrCreate<MixinClass>(i, this);
      klass->ReadFrom(reader);
    }
  }

  fields().ReadFrom<Field>(reader, this);
  procedures().ReadFrom<Procedure>(reader, this);
  return this;
}

void Library::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  name_->WriteTo(writer);
  writer->WriteUInt31(classes_.length());
  for (int i = 0; i < classes_.length(); i++) {
    Class* klass = classes_[i];
    if (klass->IsNormalClass()) {
      writer->WriteTag(kNormalClass);
      NormalClass::Cast(klass)->WriteTo(writer);
    } else {
      writer->WriteTag(kMixinClass);
      MixinClass::Cast(klass)->WriteTo(writer);
    }
  }
  fields().WriteTo(writer);
  procedures().WriteTo(writer);
}

Class* Class::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();

  is_abstract_ = reader->ReadBool();
  name_ = String::ReadFrom(reader);

  return this;
}

void Class::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteBool(is_abstract_);  
  name_->WriteTo(writer);
}

NormalClass* NormalClass::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Class::ReadFrom(reader);
  TypeParameterScope<ReaderHelper> scope(reader->helper());

  type_parameters_.ReadFromStatic<TypeParameter>(reader);
  DartType* type = reader->ReadOptional<DartType>();

  super_class_ = InterfaceType::Cast(type);
  implemented_classes_.ReadFromStatic<DowncastReader<DartType, InterfaceType> >(reader);
  fields_.ReadFrom<Field>(reader, this);
  constructors_.ReadFrom<Constructor>(reader, this);
  procedures_.ReadFrom<Procedure>(reader, this);

  return this;
}

void NormalClass::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  Class::WriteTo(writer);
  TypeParameterScope<WriterHelper> scope(writer->helper());

  type_parameters().WriteTo(writer);
  writer->WriteOptional<DartType>(super_class_);
  implemented_classes().WriteTo(writer);
  fields_.WriteTo(writer);
  constructors_.WriteTo(writer);
  procedures_.WriteTo(writer);
}

MixinClass* MixinClass::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();

  TypeParameterScope<ReaderHelper> scope(reader->helper());

  is_abstract_ = reader->ReadBool();
  name_ = String::ReadFrom(reader);
  type_parameters_.ReadFromStatic<TypeParameter>(reader);
  first_ = InterfaceType::Cast(DartType::ReadFrom(reader));
  second_ = InterfaceType::Cast(DartType::ReadFrom(reader));
  implemented_classes_.ReadFromStatic<InterfaceType>(reader);
  constructors_.ReadFrom<Constructor>(reader, this);
  return this;
}

void MixinClass::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  TypeParameterScope<WriterHelper> scope(writer->helper());

  writer->WriteBool(is_abstract_);
  name_->WriteTo(writer);
  type_parameters_.WriteTo(writer);
  first_->WriteTo(writer);
  second_->WriteTo(writer);
  implemented_classes_.WriteTo(writer);
  constructors_.WriteTo(writer);
}

Member* Reference::ReadMemberFrom(Reader* reader) {
  TRACE_READ_OFFSET();

  Program* program = reader->helper()->program();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kLibraryFieldReference: {
      int library_idx = reader->ReadUInt31();
      int field_idx = reader->ReadUInt31();
      Library* library = program->libraries().GetOrCreate<Library>(library_idx);
      return library->fields().GetOrCreate<Field>(field_idx, library);
    }
    case kLibraryProcedureReference: {
      int library_idx = reader->ReadUInt31();
      int procedure_idx = reader->ReadUInt31();
      Library* library = program->libraries().GetOrCreate<Library>(library_idx);
      return library->procedures().GetOrCreate<Procedure>(procedure_idx, library);
    }
    case kClassFieldReference:
    case kClassConstructorReference:
    case kClassProcedureReference: {
      Class* klass = Reference::ReadClassFrom(reader);
      if (tag == kClassFieldReference) {
        int field_idx = reader->ReadUInt31();
        return klass->fields().GetOrCreate<Field>(field_idx, klass);
      } else if (tag == kClassConstructorReference) {
        int constructor_idx = reader->ReadUInt31();
        return klass->constructors().GetOrCreate<Constructor>(constructor_idx, klass);
      } else {
        ASSERT(tag == kClassProcedureReference);
        int procedure_idx = reader->ReadUInt31();
        return klass->procedures().GetOrCreate<Procedure>(procedure_idx, klass);
      }
    }
    default:
      UNREACHABLE();
      break;
  }

  UNREACHABLE();
  return NULL;
}

void Reference::WriteMemberTo(Writer* writer, Member* member) {
  TRACE_WRITE_OFFSET();
  TreeNode* node = member->parent();

  WriterHelper* helper = writer->helper();

  if (node->IsLibrary()) {
    Library* library = Library::Cast(node);
    if (member->IsField()) {
      Field* field = Field::Cast(member);
      writer->WriteTag(kLibraryFieldReference);
      writer->WriteUInt31(helper->libraries().Lookup(library));
      writer->WriteUInt31(helper->fields().Lookup(field));
    } else {
      Procedure* procedure = Procedure::Cast(member);
      writer->WriteTag(kLibraryProcedureReference);
      writer->WriteUInt31(helper->libraries().Lookup(library));
      writer->WriteUInt31(helper->procedures().Lookup(procedure));
    }
  } else {
    Class* klass = Class::Cast(node);

    if (member->IsField()) {
      Field* field = Field::Cast(member);
      writer->WriteTag(kClassFieldReference);
      Reference::WriteClassTo(writer, klass);
      writer->WriteUInt31(helper->fields().Lookup(field));
    } else if (member->IsConstructor()) {
      Constructor* constructor = Constructor::Cast(member);
      writer->WriteTag(kClassConstructorReference);
      Reference::WriteClassTo(writer, klass);
      writer->WriteUInt31(helper->constructors().Lookup(constructor));
    } else {
      Procedure* procedure = Procedure::Cast(member);
      writer->WriteTag(kClassProcedureReference);
      Reference::WriteClassTo(writer, klass);
      writer->WriteUInt31(helper->procedures().Lookup(procedure));
    }
  }
}

Class* Reference::ReadClassFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Program* program = reader->helper()->program();

  Tag klass_member_tag = reader->ReadTag();
  int library_idx = reader->ReadUInt31();
  int class_idx = reader->ReadUInt31();

  Library* library = program->libraries().GetOrCreate<Library>(library_idx);
  Class* klass;
  if (klass_member_tag == kNormalClassReference) {
    klass = library->classes().GetOrCreate<NormalClass>(class_idx, library);
  } else {
    ASSERT(klass_member_tag == kMixinClassReference);
    klass = library->classes().GetOrCreate<MixinClass>(class_idx, library);
  }
  return klass;
}

void Reference::WriteClassTo(Writer* writer, Class* klass) {
  TRACE_WRITE_OFFSET();
  if (klass->IsNormalClass()) {
    writer->WriteTag(kNormalClassReference);
  } else {
    ASSERT(klass->IsMixinClass());
    writer->WriteTag(kMixinClassReference);
  }

  writer->WriteUInt31(writer->helper()->libraries().Lookup(klass->parent()));
  writer->WriteUInt31(writer->helper()->classes().Lookup(klass));
}

Field* Field::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kField);

  // TODO: Do we need a variable scope here?
  flags_ = reader->ReadFlags();
  name_ = Name::ReadFrom(reader);
  type_ = DartType::ReadFrom(reader);
  initializer_ = reader->ReadOptional<Expression>();
  return this;
}

void Field::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kField);
  writer->WriteFlags(flags_);
  name_->WriteTo(writer);
  type_->WriteTo(writer);
  writer->WriteOptional<Expression>(initializer_);
}

Constructor* Constructor::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kConstructor);

  VariableScope<ReaderHelper> vars(reader->helper());
  is_const_ = (reader->ReadFlags() & 1) == 1;
  name_ = Name::ReadFrom(reader);
  function_ = FunctionNode::ReadFrom(reader);
  initializers_.ReadFromStatic<Initializer>(reader);
  return this;
}

void Constructor::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kConstructor);

  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteBool(is_const_);
  name_->WriteTo(writer);
  function_->WriteTo(writer);
  initializers_.WriteTo(writer);
}

Procedure* Procedure::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kProcedure);

  VariableScope<ReaderHelper> vars(reader->helper());
  kind_ = static_cast<ProcedureKind>(reader->ReadByte());
  flags_ = reader->ReadFlags();
  name_ = Name::ReadFrom(reader);
  function_ = reader->ReadOptional<FunctionNode>();
  return this;
}

void Procedure::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kProcedure);

  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteByte(kind_);
  writer->WriteFlags(flags_);
  name_->WriteTo(writer);
  writer->WriteOptional<FunctionNode>(function_);
}

Initializer* Initializer::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
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
  TRACE_READ_OFFSET();
  return new InvalidInitializer();
}

void InvalidInitializer::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kInvalidInitializer);
}

FieldInitializer* FieldInitializer::ReadFromImpl(Reader* reader) {
  TRACE_READ_OFFSET();
  FieldInitializer* initializer = new FieldInitializer();
  initializer->field_ = Field::Cast(Reference::ReadMemberFrom(reader));
  initializer->value_ = Expression::ReadFrom(reader);
  return initializer;
}

void FieldInitializer::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kFieldInitializer);
  Reference::WriteMemberTo(writer, field_);
  value_->WriteTo(writer);
}

SuperInitializer* SuperInitializer::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SuperInitializer* init = new SuperInitializer();
  init->target_ = Constructor::Cast(Reference::ReadMemberFrom(reader));
  init->arguments_ = Arguments::ReadFrom(reader);
  return init;
}

void SuperInitializer::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kSuperInitializer);
  Reference::WriteMemberTo(writer, target_);
  arguments_->WriteTo(writer);
}

RedirectingInitializer* RedirectingInitializer::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  RedirectingInitializer* init = new RedirectingInitializer();
  init->target_ = Constructor::Cast(Reference::ReadMemberFrom(reader));
  init->arguments_ = Arguments::ReadFrom(reader);
  return init;
}

void RedirectingInitializer::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kRedirectingInitializer);
  Reference::WriteMemberTo(writer, target_);
  arguments_->WriteTo(writer);
}

Expression* Expression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Tag tag = reader->ReadTag();
  switch (tag) {
    case kInvalidExpression:
      return InvalidExpression::ReadFrom(reader);
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
      return BigintLiteral::ReadFrom(reader);
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
  TRACE_READ_OFFSET();
  return new InvalidExpression();
}

void InvalidExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kInvalidExpression);
}

VariableGet* VariableGet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableGet* get = new VariableGet();
  get->variable_ = reader->helper()->variables().Lookup(reader->ReadUInt31());
  return get;
}

void VariableGet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kVariableGet);
  writer->WriteUInt31(writer->helper()->variables().Lookup(variable_));
}

VariableSet* VariableSet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableSet* set = new VariableSet();
  set->variable_ = reader->helper()->variables().Lookup(reader->ReadUInt31());
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

void VariableSet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kVariableSet);
  writer->WriteUInt31(writer->helper()->variables().Lookup(variable_));
  expression_->WriteTo(writer); 
}

PropertyGet* PropertyGet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  PropertyGet* get = new PropertyGet();
  get->receiver_ = Expression::ReadFrom(reader);
  get->name_ = Name::ReadFrom(reader);
  return get;
}

void PropertyGet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kPropertyGet);
  receiver_->WriteTo(writer);
  name_->WriteTo(writer);
}

PropertySet* PropertySet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  PropertySet* set = new PropertySet();
  set->receiver_ = Expression::ReadFrom(reader);
  set->name_ = Name::ReadFrom(reader);
  set->value_= Expression::ReadFrom(reader);
  return set;
}

void PropertySet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kPropertySet);
  receiver_->WriteTo(writer);
  name_->WriteTo(writer);
  value_->WriteTo(writer);
}

SuperPropertyGet* SuperPropertyGet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SuperPropertyGet* get = new SuperPropertyGet();
  get->target_ = Reference::ReadMemberFrom(reader);
  return get;
}

void SuperPropertyGet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kSuperPropertyGet);
  Reference::WriteMemberTo(writer, target_);
}

SuperPropertySet* SuperPropertySet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SuperPropertySet* set = new SuperPropertySet();
  set->target_ = Reference::ReadMemberFrom(reader);
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

void SuperPropertySet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kSuperPropertySet);
  Reference::WriteMemberTo(writer, target_);
  expression_->WriteTo(writer);
}

StaticGet* StaticGet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  StaticGet* get = new StaticGet();
  get->target_ = Reference::ReadMemberFrom(reader);
  return get;
}

void StaticGet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kStaticGet);
  Reference::WriteMemberTo(writer, target_);
}

StaticSet* StaticSet::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  StaticSet* set = new StaticSet();
  set->target_ = Reference::ReadMemberFrom(reader);
  set->expression_ = Expression::ReadFrom(reader);
  return set;
}

void StaticSet::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kStaticSet);
  Reference::WriteMemberTo(writer, target_);
  expression_->WriteTo(writer);
}

Arguments* Arguments::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Arguments* arguments = new Arguments();
  arguments->types().ReadFromStatic<DartType>(reader);
  arguments->positional().ReadFromStatic<Expression>(reader);
  arguments->named().ReadFromStatic<NamedExpression>(reader);
  return arguments;
}

void Arguments::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  types().WriteTo(writer);
  positional().WriteTo(writer);
  named().WriteTo(writer);
}

NamedExpression* NamedExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  String* name = String::ReadFrom(reader);
  Expression* expression = Expression::ReadFrom(reader);
  return new NamedExpression(name, expression);
}

void NamedExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  name_->WriteTo(writer);
  expression_->WriteTo(writer);
}

MethodInvocation* MethodInvocation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  MethodInvocation* invocation = new MethodInvocation();
  invocation->receiver_ = Expression::ReadFrom(reader);
  invocation->name_ = Name::ReadFrom(reader);
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

void MethodInvocation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kMethodInvocation);
  receiver_->WriteTo(writer);
  name_->WriteTo(writer);
  arguments_->WriteTo(writer);
}

SuperMethodInvocation* SuperMethodInvocation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SuperMethodInvocation* invocation = new SuperMethodInvocation();
  invocation->target_ = Procedure::Cast(Reference::ReadMemberFrom(reader));
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

void SuperMethodInvocation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kSuperMethodInvocation);
  Reference::WriteMemberTo(writer, target_);
  arguments_->WriteTo(writer);
}

StaticInvocation* StaticInvocation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();

  Member* member = Reference::ReadMemberFrom(reader);
  Arguments* args = Arguments::ReadFrom(reader);

  return new StaticInvocation(Procedure::Cast(member), args);
}

void StaticInvocation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kStaticInvocation);
  Reference::WriteMemberTo(writer, procedure_);
  arguments_->WriteTo(writer);
}

FunctionInvocation* FunctionInvocation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  FunctionInvocation* invocation = new FunctionInvocation();
  invocation->function_ = Expression::ReadFrom(reader);
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

void FunctionInvocation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kFunctionInvocation);
  function_->WriteTo(writer);
  arguments_->WriteTo(writer);
}

ConstructorInvocation* ConstructorInvocation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  ConstructorInvocation* invocation = new ConstructorInvocation();
  invocation->is_const_ = reader->ReadBool();
  invocation->target_ = Constructor::Cast(Reference::ReadMemberFrom(reader));
  invocation->arguments_ = Arguments::ReadFrom(reader);
  return invocation;
}

void ConstructorInvocation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kConstructorInvocation);
  writer->WriteBool(is_const_);
  Reference::WriteMemberTo(writer, target_);
  arguments_->WriteTo(writer);
}

Not* Not::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Not* n = new Not();
  n->expression_ = Expression::ReadFrom(reader);
  return n;
}

void Not::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kNot);
  expression_->WriteTo(writer);
}

LogicalExpression* LogicalExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  LogicalExpression* expr = new LogicalExpression();
  expr->left_ = Expression::ReadFrom(reader);
  expr->operator_ = reader->ReadByte();
  expr->right_ = Expression::ReadFrom(reader);
  return expr;
}

void LogicalExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kLogicalExpression);
  left_->WriteTo(writer);
  writer->WriteByte(operator_);
  right_->WriteTo(writer);
}

ConditionalExpression* ConditionalExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  ConditionalExpression* expr = new ConditionalExpression();
  expr->condition_ = Expression::ReadFrom(reader);
  expr->then_ = Expression::ReadFrom(reader);
  expr->otherwise_ = Expression::ReadFrom(reader);
  return expr;
}

void ConditionalExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kConditionalExpression);
  condition_->WriteTo(writer);
  then_->WriteTo(writer);
  otherwise_->WriteTo(writer);
}

StringConcatenation* StringConcatenation::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  StringConcatenation* concat = new StringConcatenation();
  concat->expressions_.ReadFromStatic<Expression>(reader);
  return concat;
}

void StringConcatenation::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kStringConcatenation);
  expressions_.WriteTo(writer);
}

IsExpression* IsExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  IsExpression* expr = new IsExpression();
  expr->operand_ = Expression::ReadFrom(reader);
  expr->type_ = DartType::ReadFrom(reader);
  return expr;
}

void IsExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kIsExpression);
  operand_->WriteTo(writer);
  type_->WriteTo(writer);
}

AsExpression* AsExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  AsExpression* expr = new AsExpression();
  expr->operand_ = Expression::ReadFrom(reader);
  expr->type_ = DartType::ReadFrom(reader);
  return expr;
}

void AsExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kAsExpression);
  operand_->WriteTo(writer);
  type_->WriteTo(writer);
}

StringLiteral* StringLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new StringLiteral(String::ReadFrom(reader));
}

void StringLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kStringLiteral);
  value_->WriteTo(writer);
}

BigintLiteral* BigintLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new BigintLiteral(String::ReadFrom(reader));
}

void BigintLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kBigIntLiteral);
  value_->WriteTo(writer);
}

IntLiteral* IntLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  IntLiteral* literal = new IntLiteral();
  bool negative = reader->ReadBool();
  literal->value_ = negative ? -static_cast<int64_t>(reader->ReadUInt31()) : reader->ReadUInt31();
  return literal;
}

void IntLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kIntLiteral);
  writer->WriteBool(value_ < 0);
  writer->WriteUInt31(static_cast<uint32_t>(value_ < 0 ? -value_ : value_));
}

DoubleLiteral* DoubleLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  DoubleLiteral* literal = new DoubleLiteral();
  literal->value_ = String::ReadFrom(reader);
  return literal;
}

void DoubleLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kDoubleLiteral);
  value_->WriteTo(writer);
}

BoolLiteral* BoolLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  BoolLiteral* lit = new BoolLiteral();
  lit->value_ = reader->ReadBool();
  return lit;
}

void BoolLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kBoolLiteral);
  writer->WriteBool(value_);
}

NullLiteral* NullLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new NullLiteral();
}

void NullLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kNullLiteral);
}

SymbolLiteral* SymbolLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SymbolLiteral* lit = new SymbolLiteral();
  lit->value_ = String::ReadFrom(reader);
  return lit;
}

void SymbolLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kSymbolLiteral);
  value_->WriteTo(writer);
}

TypeLiteral* TypeLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  TypeLiteral* literal = new TypeLiteral();
  literal->type_ = DartType::ReadFrom(reader);
  return literal;
}

void TypeLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kTypeLiteral);
  type_->WriteTo(writer);
}

ThisExpression* ThisExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new ThisExpression();
}

void ThisExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kThisExpression);
}

Rethrow* Rethrow::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new Rethrow();
}

void Rethrow::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kRethrow);
}

Throw* Throw::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Throw* t = new Throw();
  t->expression_ = Expression::ReadFrom(reader);
  return t;
}

void Throw::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kThrow);
  expression_->WriteTo(writer);
}

ListLiteral* ListLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  ListLiteral* literal = new ListLiteral();
  literal->is_const_ = reader->ReadBool();
  literal->type_ = DartType::ReadFrom(reader);
  literal->expressions_.ReadFromStatic<Expression>(reader);
  return literal;
}

void ListLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kListLiteral);
  writer->WriteBool(is_const_);
  type_->WriteTo(writer);
  expressions_.WriteTo(writer);
}

MapLiteral* MapLiteral::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  MapLiteral* literal = new MapLiteral();
  literal->is_const_ = reader->ReadBool();
  literal->key_type_ = DartType::ReadFrom(reader);
  literal->value_type_ = DartType::ReadFrom(reader);
  literal->entries_.ReadFromStatic<MapEntry>(reader);
  return literal;
}

void MapLiteral::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kMapLiteral);
  writer->WriteBool(is_const_);
  key_type_->WriteTo(writer);
  value_type_->WriteTo(writer);
  entries_.WriteTo(writer);
}

MapEntry* MapEntry::ReadFrom(Reader* reader) {
  MapEntry* entry = new MapEntry();
  entry->key_ = Expression::ReadFrom(reader);
  entry->value_ = Expression::ReadFrom(reader);
  return entry;
}

void MapEntry::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  key_->WriteTo(writer);
  value_->WriteTo(writer);
}

AwaitExpression* AwaitExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  AwaitExpression* await = new AwaitExpression();
  await->operand_ = Expression::ReadFrom(reader);
  return await;
}

void AwaitExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kAwaitExpression);
  operand_->WriteTo(writer);
}

FunctionExpression* FunctionExpression::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  FunctionExpression* expr = new FunctionExpression();
  expr->function_ = FunctionNode::ReadFrom(reader);
  return expr;
}

void FunctionExpression::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kFunctionExpression);
  function_->WriteTo(writer);
}

Let* Let::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  Let* let = new Let();
  let->variable_ = VariableDeclaration::ReadFrom(reader);
  let->body_ = Expression::ReadFrom(reader);
  return let;
}

void Let::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteTag(kLet);
  variable_->WriteTo(writer);
  body_->WriteTo(writer);
}

Statement* Statement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
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
  TRACE_READ_OFFSET();
  return new InvalidStatement();
}

void InvalidStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kInvalidStatement);
}

ExpressionStatement* ExpressionStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new ExpressionStatement(Expression::ReadFrom(reader));
}

void ExpressionStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kExpressionStatement);
  expression_->WriteTo(writer);
}

Block* Block::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  Block* block = new Block();
  block->statements().ReadFromStatic<Statement>(reader);
  return block;
}

void Block::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteTag(kBlock);
  statements_.WriteTo(writer);
}

EmptyStatement* EmptyStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new EmptyStatement();
}

void EmptyStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kEmptyStatement);
}

AssertStatement* AssertStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  AssertStatement* stmt = new AssertStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  stmt->message_ = reader->ReadOptional<Expression>();
  return stmt;
}

void AssertStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kAssertStatement);
  condition_->WriteTo(writer);
  writer->WriteOptional<Expression>(message_);
}

LabeledStatement* LabeledStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  LabeledStatement* stmt = new LabeledStatement();
  reader->helper()->lables().Push(stmt);
  stmt->body_ = Statement::ReadFrom(reader);
  reader->helper()->lables().Pop(stmt);
  return stmt;
}

void LabeledStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kLabeledStatement);
  writer->helper()->lables().Push(this);
  body_->WriteTo(writer);
  writer->helper()->lables().Pop(this);
}

BreakStatement* BreakStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  BreakStatement* stmt = new BreakStatement();
  stmt->target_ = reader->helper()->lables().Lookup(reader->ReadUInt31());
  return stmt;
}

void BreakStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kBreakStatement);
  writer->WriteUInt31(writer->helper()->lables().Lookup(target_));
}

WhileStatement* WhileStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  WhileStatement* stmt = new WhileStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  stmt->body_ = Statement::ReadFrom(reader);
  return stmt;
}

void WhileStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kWhileStatement);
  condition_->WriteTo(writer);
  body_->WriteTo(writer);
}

DoStatement* DoStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  DoStatement* dostmt = new DoStatement();
  dostmt->body_ = Statement::ReadFrom(reader);
  dostmt->condition_ = Expression::ReadFrom(reader);
  return dostmt;
}

void DoStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kDoStatement);
  body_->WriteTo(writer);
  condition_->WriteTo(writer);
}

ForStatement* ForStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  ForStatement* forstmt = new ForStatement();
  forstmt->variables_.ReadFromStatic<VariableDeclaration>(reader);
  forstmt->condition_ = reader->ReadOptional<Expression>();
  forstmt->updates_.ReadFromStatic<Expression>(reader);
  forstmt->body_ = Statement::ReadFrom(reader);
  return forstmt;
}

void ForStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kForStatement);
  VariableScope<WriterHelper> vars(writer->helper());
  variables_.WriteTo(writer);
  writer->WriteOptional<Expression>(condition_);
  updates_.WriteTo(writer);
  body_->WriteTo(writer);
}

ForInStatement* ForInStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  ForInStatement* forinstmt = new ForInStatement();
  forinstmt->is_async_ = reader->ReadBool();
  forinstmt->variable_ = VariableDeclaration::ReadFrom(reader);
  forinstmt->iterable_ = Expression::ReadFrom(reader);
  forinstmt->body_ = Statement::ReadFrom(reader);
  return forinstmt;
}

void ForInStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kForInStatement);
  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteBool(is_async_);
  variable_->WriteTo(writer);
  iterable_->WriteTo(writer);
  body_->WriteTo(writer);
}

SwitchStatement* SwitchStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  SwitchCaseScope<ReaderHelper> scope(reader->helper());
  SwitchStatement* stmt = new SwitchStatement();
  stmt->condition_ = Expression::ReadFrom(reader);
  // We need to explicitly create empty [SwitchCase]s first in order to add them
  // to the [SwitchCaseScope]. This is necessary since a [Statement] in a switch
  // case can refer to one defined later on.
  int count = reader->ReadUInt31();
  for (int i = 0; i < count; i++) {
    SwitchCase* sc = stmt->cases_.GetOrCreate<SwitchCase>(i);
    reader->helper()->switch_cases().Push(sc);
  }
  for (int i = 0; i < count; i++) {
    SwitchCase* sc = stmt->cases_[i];
    sc->ReadFrom(reader);
  }
  return stmt;
}

void SwitchStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  SwitchCaseScope<WriterHelper> scope(writer->helper());
  writer->WriteTag(kSwitchStatement);
  condition_->WriteTo(writer);
  for (int i = 0; i < cases_.length(); i++) {
    writer->helper()->switch_cases().Push(cases_[i]);
  }
  cases_.WriteTo(writer);
}

SwitchCase* SwitchCase::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  expressions_.ReadFromStatic<Expression>(reader);
  is_default_ = reader->ReadBool();
  body_ = Statement::ReadFrom(reader);
  return this;
}

void SwitchCase::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  expressions_.WriteTo(writer);
  writer->WriteBool(is_default_);
  body_->WriteTo(writer);
}

ContinueSwitchStatement* ContinueSwitchStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  ContinueSwitchStatement* stmt =  new ContinueSwitchStatement();
  stmt->target_ = reader->helper()->switch_cases().Lookup(reader->ReadUInt31());
  return stmt;
}

void ContinueSwitchStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kContinueSwitchStatement);
  writer->WriteUInt31(writer->helper()->switch_cases().Lookup(target_));
}

IfStatement* IfStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  IfStatement* ifstmt = new IfStatement();
  ifstmt->condition_ = Expression::ReadFrom(reader);
  ifstmt->then_= Statement::ReadFrom(reader);
  ifstmt->otherwise_ = Statement::ReadFrom(reader);
  return ifstmt;
}

void IfStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kIfStatement);
  condition_->WriteTo(writer);
  then_->WriteTo(writer);
  otherwise_->WriteTo(writer);
}

ReturnStatement* ReturnStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  ReturnStatement* ret = new ReturnStatement();
  ret->expression_ = reader->ReadOptional<Expression>();
  return ret;
}

void ReturnStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kReturnStatement);
  writer->WriteOptional<Expression>(expression_);
}

TryCatch* TryCatch::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  TryCatch* tc = new TryCatch();
  tc->body_ = Statement::ReadFrom(reader);
  tc->catches_.ReadFromStatic<Catch>(reader);
  return tc;
}

void TryCatch::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteTag(kTryCatch);
  body_->WriteTo(writer);
  catches_.WriteTo(writer);
}

Catch* Catch::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableScope<ReaderHelper> vars(reader->helper());
  Catch* c = new Catch();
  c->guard_ = reader->ReadOptional<DartType>();
  c->exception_ = reader->ReadOptional<VariableDeclaration>();
  c->stack_trace_ = reader->ReadOptional<VariableDeclaration>();
  c->body_ = Statement::ReadFrom(reader);
  return c;
}

void Catch::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteOptional<DartType>(guard_);
  writer->WriteOptional<VariableDeclaration>(exception_);
  writer->WriteOptional<VariableDeclaration>(stack_trace_);
  body_->WriteTo(writer);
}

TryFinally* TryFinally::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  TryFinally* tf = new TryFinally();
  tf->body_ = Statement::ReadFrom(reader);
  tf->finalizer_ = Statement::ReadFrom(reader);
  return tf;
}

void TryFinally::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kTryFinally);
  body_->WriteTo(writer);
  finalizer_->WriteTo(writer);
}

YieldStatement* YieldStatement::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  YieldStatement* stmt = new YieldStatement();
  stmt->is_yield_star_ = reader->ReadBool();
  stmt->expression_ = Expression::ReadFrom(reader);
  return stmt;
}

void YieldStatement::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kYieldStatement);
  writer->WriteBool(is_yield_star_);
  expression_->WriteTo(writer);
}

VariableDeclaration* VariableDeclaration::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Tag tag = reader->ReadTag();
  ASSERT(tag == kVariableDeclaration);
  return VariableDeclaration::ReadFromImpl(reader);
}

VariableDeclaration* VariableDeclaration::ReadFromImpl(Reader* reader) {
  TRACE_READ_OFFSET();
  VariableDeclaration* decl = new VariableDeclaration();
  reader->helper()->variables().Push(decl);
  decl->flags_ = reader->ReadFlags();
  decl->name_ = String::ReadFrom(reader);
  decl->type_ = reader->ReadOptional<DartType>();
  decl->initializer_ = reader->ReadOptional<Expression>();
  return decl;
}

void VariableDeclaration::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kVariableDeclaration);
  WriteToImpl(writer);
}

void VariableDeclaration::WriteToImpl(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->helper()->variables().Push(this);
  writer->WriteFlags(flags_);
  name_->WriteTo(writer);
  writer->WriteOptional<DartType>(type_);
  writer->WriteOptional<Expression>(initializer_);
}

FunctionDeclaration* FunctionDeclaration::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  FunctionDeclaration* decl = new FunctionDeclaration();
  decl->variable_ = VariableDeclaration::ReadFrom(reader);
  decl->function_ = FunctionNode::ReadFrom(reader);
  return decl;
}

void FunctionDeclaration::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kFunctionDeclaration);
  variable_->WriteTo(writer);
  function_->WriteTo(writer);
}

Name* Name::ReadFrom(Reader* reader) {
  String* string = String::ReadFrom(reader);
  int lib_index = reader->ReadUInt31();
  Library* library = reader->helper()->program()->libraries().GetOrCreate<Library>(lib_index);
  return new Name(string, library);
}

void Name::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  string_->WriteTo(writer);
  writer->WriteUInt31(writer->helper()->libraries().Lookup(library_));
}

DartType* DartType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
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
  TRACE_READ_OFFSET();
  return new InvalidType();
}

void InvalidType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kInvalidType);
}

DynamicType* DynamicType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new DynamicType();
}

void DynamicType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kDynamicType);
}

VoidType* VoidType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  return new VoidType();
}

void VoidType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kVoidType);
}

InterfaceType* InterfaceType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  Class* klass = Reference::ReadClassFrom(reader);
  InterfaceType* type = new InterfaceType(klass);
  type->type_arguments().ReadFromStatic<DartType>(reader);
  return type;
}

void InterfaceType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kInterfaceType);
  Reference::WriteClassTo(writer, klass_);
  type_arguments_.WriteTo(writer);
}

FunctionType* FunctionType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  FunctionType* type = new FunctionType();
  type->type_parameters().ReadFromStatic<TypeParameter>(reader);
  type->required_parameter_count_ = reader->ReadUInt31();
  type->positional_parameters().ReadFromStatic<DartType>(reader);
  type->named_parameters().ReadFromStatic<Tuple<String, DartType> >(reader);
  type->return_type_ = DartType::ReadFrom(reader);
  return type;
}

void FunctionType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kFunctionType);
  type_parameters_.WriteTo(writer);
  writer->WriteUInt31(required_parameter_count_);
  positional_parameters_.WriteTo(writer);
  named_parameters_.WriteTo(writer);
  return_type_->WriteTo(writer);
}

TypeParameterType* TypeParameterType::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  TypeParameterType* type = new TypeParameterType();
  type->parameter_ = reader->helper()->type_parameters().Lookup(reader->ReadUInt31());
  return type;
}

void TypeParameterType::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteTag(kTypeParameterType);
  writer->WriteUInt31(writer->helper()->type_parameters().Lookup(parameter_));
}

Program* Program::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  uint32_t magic = reader->ReadUInt32();
  if (magic != kMagicProgramFile) FATAL("Invalid magic identifier");

  Program* program = new Program();
  reader->helper()->set_program(program);

  int libraries = reader->ReadUInt31();
  program->libraries().EnsureInitialized(libraries);
  for (int i = 0; i < libraries; i++) {
    program->libraries().GetOrCreate<Library>(i)->ReadFrom(reader);
  }

  program->main_method_ = Procedure::Cast(Reference::ReadMemberFrom(reader));

  return program;
}

void Program::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();

  writer->helper()->SetProgram(this);

  writer->WriteUInt32(kMagicProgramFile);
  libraries_.WriteTo(writer);
  Reference::WriteMemberTo(writer, main_method_);
}

FunctionNode* FunctionNode::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  FunctionNode* function = new FunctionNode();
  function->async_marker_ = static_cast<FunctionNode::AsyncMarker>(reader->ReadByte());
  function->type_parameters().ReadFromStatic<TypeParameter>(reader);
  function->required_parameter_count_ = reader->ReadUInt31();
  function->positional_parameters().ReadFromStatic<VariableDeclaration>(reader);
  function->named_parameters().ReadFromStatic<VariableDeclaration>(reader);
  function->return_type_ = reader->ReadOptional<DartType>();

  VariableScope<ReaderHelper> vars(reader->helper());
  function->body_ = reader->ReadOptional<Statement>();
  return function;
}

void FunctionNode::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->WriteByte(static_cast<uint8_t>(async_marker_));
  type_parameters().WriteTo(writer);
  writer->WriteUInt31(required_parameter_count());
  positional_parameters().WriteTo(writer);
  named_parameters().WriteTo(writer);
  writer->WriteOptional<DartType>(return_type_);

  VariableScope<WriterHelper> vars(writer->helper());
  writer->WriteOptional<Statement>(body_);
}

TypeParameter* TypeParameter::ReadFrom(Reader* reader) {
  TRACE_READ_OFFSET();
  TypeParameter* param = new TypeParameter();
  reader->helper()->type_parameters().Push(param);
  param->name_ = String::ReadFrom(reader);
  param->bound_ = DartType::ReadFrom(reader);
  return param;
}

void TypeParameter::WriteTo(Writer* writer) {
  TRACE_WRITE_OFFSET();
  writer->helper()->type_parameters().Push(this);
  name_->WriteTo(writer);
  bound_->WriteTo(writer);
}

}  // namespace dil

dil::Program* ReadPrecompiledDil(const char* filename) {
  if (filename != NULL) {
    InputFile file(filename);
    if (file.ReadAll()) {
      dil::Reader reader(file.buffer(), file.size());
      return dil::Program::ReadFrom(&reader);
    }
  }
  return NULL;
}

bool WritePrecompiledDil(const char* filename, dil::Program* program) {
  ASSERT(filename != NULL);

  {
    OutputFile file(filename);
    if (!file.Open()) return false;

    dil::Writer writer(&file);
    program->WriteTo(&writer);
    return true;
  }
  return false;
}

}  // namespace dart
