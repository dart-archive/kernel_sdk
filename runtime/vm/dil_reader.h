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

class DilReader;

class BuildingTranslationHelper : public TranslationHelper {
 public:
  BuildingTranslationHelper(
      DilReader* reader, dart::Thread* thread, dart::Zone* zone,
      Isolate* isolate)
      : TranslationHelper(thread, zone, isolate), reader_(reader) {}
  virtual ~BuildingTranslationHelper() {}

  virtual void SetFinalize(bool finalize);

  virtual RawLibrary* LookupLibraryByDilLibrary(Library* library);
  virtual RawClass* LookupClassByDilClass(Class* klass);

 private:
  DilReader* reader_;
};

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
  DilReader(const uint8_t* buffer, intptr_t len, bool bootstrapping = false)
      : thread_(dart::Thread::Current()),
        zone_(thread_->zone()),
        isolate_(thread_->isolate()),
        scripts_(Array::ZoneHandle(zone_)),
        program_(NULL),
        translation_helper_(this, thread_, zone_, isolate_),
        type_translator_(&translation_helper_, &active_class_, !bootstrapping),
        bootstrapping_(bootstrapping),
        finalize_(!bootstrapping),
        buffer_(buffer),
        buffer_length_(len) {}

  // Returns either pointer to a program or null.
  Program* ReadPrecompiledProgram();

  // Returns either a library or a failure object.
  dart::Object& ReadProgram();

  static void SetupFunctionParameters(TranslationHelper translation_helper_,
                                      DartTypeTranslator type_translator_,
                                      const dart::Class& owner,
                                      const dart::Function& function,
                                      FunctionNode* dil_function,
                                      bool is_method,
                                      bool is_closure);

  void ReadLibrary(Library* dil_library);

 private:
  friend class BuildingTranslationHelper;

  void ReadPreliminaryClass(dart::Class* klass, Class* dil_klass);
  void ReadClass(const dart::Library& library, Class* dil_klass);
  void ReadProcedure(const dart::Library& library,
                     const dart::Class& owner,
                     Procedure* procedure,
                     Class* dil_klass = NULL);

  // If klass's script is not the script at the uri index, return a PatchClass
  // for klass whose script corresponds to the uri index.
  // Otherwise return klass.
  const Object& ClassForScriptAt(const dart::Class& klass,
        intptr_t source_uri_index);
  Script& ScriptAt(intptr_t source_uri_index);

  void GenerateFieldAccessors(const dart::Class& klass,
                              const dart::Field& field,
                              Field* dil_field);

  void SetupFieldAccessorFunction(const dart::Class& klass,
                                  const dart::Function& function);

  dart::Library& LookupLibrary(Library* library);
  dart::Class& LookupClass(Class* klass);

  dart::RawFunction::Kind GetFunctionType(Procedure* dil_procedure);

  dart::Thread* thread_;
  dart::Zone* zone_;
  dart::Isolate* isolate_;
  Array& scripts_;
  Program* program_;
  ActiveClass active_class_;
  BuildingTranslationHelper translation_helper_;
  DartTypeTranslator type_translator_;

  bool bootstrapping_;

  // Should created classes be finalized when they are created?
  bool finalize_;

  const uint8_t* buffer_;
  intptr_t buffer_length_;

  Mapping<Library, dart::Library> libraries_;
  Mapping<Class, dart::Class> classes_;
};

}  // namespace dil
}  // namespace dart

#endif  // VM_DIL_READER_H_
