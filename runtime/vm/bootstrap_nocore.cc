// Copyright (c) 2012, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/bootstrap.h"

#include "include/dart_api.h"

#include "vm/class_finalizer.h"
#include "vm/compiler.h"
#include "vm/dil_reader.h"
#include "vm/object.h"
#include "vm/object_store.h"

namespace dart {

#define MAKE_PROPERTIES(CamelName, hacker_name)                                \
  { ObjectStore::k##CamelName, "dart:"#hacker_name },


struct bootstrap_lib_props {
  ObjectStore::BootstrapLibraryId index_;
  const char* uri_;
};


static bootstrap_lib_props bootstrap_libraries[] = {
  FOR_EACH_BOOTSTRAP_LIBRARY(MAKE_PROPERTIES)
};


static const intptr_t bootstrap_library_count = ARRAY_SIZE(bootstrap_libraries);


void Finish(Thread* thread, bool from_dilfile) {
  Bootstrap::SetupNativeResolver();
  ClassFinalizer::ProcessPendingClasses(from_dilfile);

  // Eagerly compile the _Closure class as it is the class of all closure
  // instances. This allows us to just finalize function types without going
  // through the hoops of trying to compile their scope class.
  ObjectStore* object_store = thread->isolate()->object_store();
  Class& cls = Class::Handle(thread->zone(), object_store->closure_class());
  Compiler::CompileClass(cls);
  // Eagerly compile Bool class, bool constants are used from within compiler.
  cls = object_store->bool_class();
  Compiler::CompileClass(cls);
}


RawError* BootstrapFromDil(Thread* thread,
                           const uint8_t* dilfile,
                           intptr_t dilfile_length) {
  Zone* zone = thread->zone();
  dil::DilReader reader(dilfile, dilfile_length, true);
  dil::Program* program = reader.ReadPrecompiledProgram();
  if (program == NULL) {
    const String& message =
        String::Handle(zone, String::New("Failed to read .dill file"));
    return ApiError::New(message);
  }

  Isolate* isolate = thread->isolate();
  // Mark the already-pending classes.  This mark bit will be used to avoid
  // adding classes to the list more than once.
  GrowableObjectArray& pending_classes = GrowableObjectArray::Handle(zone,
      isolate->object_store()->pending_classes());
  dart::Class& pending = dart::Class::Handle(zone);
  for (intptr_t i = 0; i < pending_classes.Length(); ++i) {
    pending ^= pending_classes.At(i);
    pending.set_is_marked_for_parsing();
  }

  Library& library = Library::Handle(zone);
  String& dart_name = String::Handle(zone);
  String& dil_name = String::Handle(zone);
  for (intptr_t i = 0; i < bootstrap_library_count; ++i) {
    ObjectStore::BootstrapLibraryId id = bootstrap_libraries[i].index_;
    library = isolate->object_store()->bootstrap_library(id);
    dart_name = library.url();
    for (intptr_t j = 0; j < program->libraries().length(); ++j) {
      dil::Library* dil_library = program->libraries()[j];
      dil::String* uri = dil_library->import_uri();
      dil_name = Symbols::FromUTF8(thread, uri->buffer(), uri->size());
      if (dil_name.Equals(dart_name)) {
        reader.ReadLibrary(dil_library);
        library.SetLoaded();
        break;
      }
    }
  }

  Finish(thread, true);
  return Error::null();
}


RawError* Bootstrap::DoBootstrapping(const uint8_t* dilfile,
                                     intptr_t dilfile_length) {
  Thread* thread = Thread::Current();
  Isolate* isolate = thread->isolate();
  Zone* zone = thread->zone();
  String& uri = String::Handle(zone);
  Library& lib = Library::Handle(zone);

  HANDLESCOPE(thread);

  // Ensure there are library objects for all the bootstrap libraries.
  for (intptr_t i = 0; i < bootstrap_library_count; ++i) {
    ObjectStore::BootstrapLibraryId id = bootstrap_libraries[i].index_;
    uri = Symbols::New(thread, bootstrap_libraries[i].uri_);
    lib = isolate->object_store()->bootstrap_library(id);
    ASSERT(lib.raw() == Library::LookupLibrary(thread, uri));
    if (lib.IsNull()) {
      lib = Library::NewLibraryHelper(uri, false);
      lib.SetLoadRequested();
      lib.Register(thread);
      isolate->object_store()->set_bootstrap_library(id, lib);
    }
  }

  ASSERT(dilfile != NULL);
  return BootstrapFromDil(thread, dilfile, dilfile_length);
}

}  // namespace dart
