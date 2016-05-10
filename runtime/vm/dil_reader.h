// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_READER_H_
#define VM_DIL_READER_H_

#include <map>

#include "vm/dil.h"
#include "vm/object.h"

namespace dart {

namespace dil {

template<typename DilType, typename VmType>
class Mapping {
 public:
  bool Lookup(DilType* node, VmType** handle) {
    typename MapType::iterator value = map_.find(node);
    if (value != map_.end()) {
      *handle = value->second;
      return true;
    }
    return false;
  }

  void Insert(DilType* node, VmType& object) {
    map_[node] = &object;
  }

 private:
  typedef typename std::map<DilType*, VmType*> MapType;
  MapType map_;
};

class DilReader {
 public:
  DilReader(const uint8_t* buffer, intptr_t len)
      : zone_(dart::Thread::Current()->zone()),
        isolate_(dart::Thread::Current()->isolate()),
        buffer_(buffer),
        buffer_length_(len) {}

  // Returns either a library or a failure object.
  dart::Object& ReadProgram();

 private:
  void ReadLibrary(Library* dil_library);
  void ReadClass(dart::Library& library, Class* dil_klass);
  void ReadProcedure(dart::Library& library,
                     dart::Class& owner,
                     Procedure* procedure,
                     Class* dil_klass = NULL);

  void SetupFunctionParameters(dart::Class& owner,
                               dart::Function& function,
                               FunctionNode* dil_function,
                               bool is_method);

  dart::Library& LookupLibrary(Library* library);
  dart::Class& LookupClass(Class* klass);

  dart::RawFunction::Kind GetFunctionType(Procedure* dil_procedure);

  dart::String& DartString(const char* content);
  dart::String& DartString(String* content);

  dart::String& DartSymbol(const char* content);
  dart::String& DartSymbol(String* content);

  dart::RawString* DartConstructorName(Constructor* node);

  dart::Zone* zone_;
  dart::Isolate* isolate_;
  const uint8_t* buffer_;
  intptr_t buffer_length_;

  Mapping<Library, dart::Library> libraries_;
  Mapping<Class, dart::Class> classes_;
  Mapping<String, dart::String> strings_;
};

}  // namespace dil

}  // namespace dart

#endif  // VM_DIL_READER_H_
