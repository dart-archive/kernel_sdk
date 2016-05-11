// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_READER_H_
#define VM_DIL_READER_H_

#include <map>

#include "vm/dil.h"
#include "vm/dil_to_il.h"
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

  void Insert(DilType* node, VmType* object) {
    map_[node] = object;
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
        translation_helper_(zone_),
        buffer_(buffer),
        buffer_length_(len) {}

  // Returns either a library or a failure object.
  dart::Object& ReadProgram();

 private:
  void ReadLibrary(Library* dil_library);
  void ReadClass(const dart::Library& library, Class* dil_klass);
  void ReadProcedure(const dart::Library& library,
                     const dart::Class& owner,
                     Procedure* procedure,
                     Class* dil_klass = NULL);

  void GenerateFieldAccessors(const dart::Class& klass,
                              Class* dil_klass,
                              Field* field);

  void SetupFunctionParameters(const dart::Class& owner,
                               const dart::Function& function,
                               FunctionNode* dil_function,
                               bool is_method);

  void SetupFieldAccessorFunction(const dart::Class& klass,
                                  const dart::Function& function);

  dart::Library& LookupLibrary(Library* library);
  dart::Class& LookupClass(Class* klass);

  dart::RawFunction::Kind GetFunctionType(Procedure* dil_procedure);

  dart::Zone* zone_;
  dart::Isolate* isolate_;
  TranslationHelper translation_helper_;

  const uint8_t* buffer_;
  intptr_t buffer_length_;

  Mapping<Library, dart::Library> libraries_;
  Mapping<Class, dart::Class> classes_;
};

}  // namespace dil
}  // namespace dart

#endif  // VM_DIL_READER_H_
