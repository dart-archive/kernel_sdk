// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/dil_reader.h"

#include <string.h>

#include "vm/dart_api_impl.h"
#include "vm/object_store.h"
#include "vm/symbols.h"

namespace dart {

namespace dil {

#define Z (zone_)
#define I (isolate_)

// FIXME(kustermann): We should add support for type conversion to annotate
// fields/parameters/variables with proper types.

Object& DilReader::ReadProgram() {
  Program* program = ReadPrecompiledDilFromBuffer(buffer_, buffer_length_);
  if (program == NULL) {
    dart::String& error = DartString("Failed to read .dill file");
    return Object::Handle(Z, ApiError::New(error));
  }

  Procedure* main = program->main_method();
  Library* dil_main_library = Library::Cast(main->parent());

  intptr_t length = program->libraries().length();
  for (int i = 0; i < length; i++) {
    Library* dil_library = program->libraries()[i];
    ReadLibrary(dil_library);
  }

  dart::Library& library = LookupLibrary(dil_main_library);

  // Sanity check that we can find the main entrypoint.
  Object& main_obj = Object::Handle(Z,
      library.LookupObjectAllowPrivate(DartSymbol("main")));
  ASSERT(!main_obj.IsNull());

  return library;
}

void DilReader::ReadLibrary(Library* dil_library) {
  dart::Library& library = LookupLibrary(dil_library);
  if (!dil_library->IsCorelibrary()) {
    // Setup toplevel class (which contains library fields/procedures).

    // FIXME(kustermann): Figure out why we need this script stuff here.
    Script& script = Script::Handle(Z,
        Script::New(DartString(""), DartString(""), RawScript::kScriptTag));
    script.SetLocationOffset(0, 0);
    script.Tokenize(DartString("nop() {}"));
    dart::Class& toplevel_class = dart::Class::Handle(dart::Class::New(
          Symbols::TopLevel(), script, TokenPosition::kNoSource));
    toplevel_class.set_library(library);
    library.set_toplevel_class(toplevel_class);

    // Load toplevel fields.
    for (int i = 0; i < dil_library->fields().length(); i++) {
      Field* dil_field = dil_library->fields()[i];
      dart::String& name = DartSymbol(dil_field->name()->string());
      dart::Field& field = dart::Field::Handle(Z, dart::Field::NewTopLevel(
            name,
            dil_field->IsConst(),
            dil_field->IsStatic(),
            toplevel_class,
            TokenPosition::kNoSource));
      toplevel_class.AddField(field);
      library.AddObject(field, name);
    }

    // Load toplevel procedures.
    for (int i = 0; i < dil_library->procedures().length(); i++) {
      Procedure* dil_procedure = dil_library->procedures()[i];
      ReadProcedure(library, toplevel_class, dil_procedure);
    }

    // Load all classes.
    for (int i = 0; i < dil_library->classes().length(); i++) {
      Class* dil_klass = dil_library->classes()[i];
      ReadClass(library, dil_klass);
    }
  }
}

void DilReader::ReadClass(dart::Library& library, Class* dil_klass) {
  dart::Class& klass = LookupClass(dil_klass);

  klass.set_library(library);
  library.AddClass(klass);

  TokenPosition pos(0);

  Type& klass_type = Type::Handle(Type::NewNonParameterizedType(klass));

  for (int i = 0; i < dil_klass->fields().length(); i++) {
    Field* dil_field = dil_klass->fields()[i];
    dart::String& name = DartSymbol(dil_field->name()->string());
    const AbstractType& type = AbstractType::dynamic_type();
    dart::Field& field = dart::Field::Handle(Z,
        dart::Field::New(name,
                   dil_field->IsStatic(),
                   dil_field->IsFinal(),
                   dil_field->IsConst(),
                   false,  // is_reflectable
                   klass,
                   type,
                   pos));
    klass.AddField(field);
  }

  for (int i = 0; i < dil_klass->constructors().length(); i++) {
    Constructor* dil_constructor = dil_klass->constructors()[i];

    dart::String& name =
        dart::String::ZoneHandle(DartConstructorName(dil_constructor));
    Function& function = dart::Function::Handle(Z, dart::Function::New(
          name,
          RawFunction::kConstructor,
          false,  // is_static
          false,  // is_const
          false,  // is_abstract
          false,  // is_external
          false,  // is_native
          klass,
          pos));
    klass.AddFunction(function);
    function.set_dil_function(reinterpret_cast<intptr_t>(dil_constructor));
    function.set_result_type(klass_type);
    SetupFunctionParameters(klass, function, dil_constructor->function(), true);
    library.AddObject(function, name);
  }

  for (int i = 0; i < dil_klass->procedures().length(); i++) {
    Procedure* dil_procedure = dil_klass->procedures()[i];
    dart::String& name = DartSymbol(dil_procedure->name()->string());
    Function& function = Function::Handle(Z, Function::New(
          name,
          GetFunctionType(dil_procedure),
          dil_procedure->IsStatic(),  // is_static
          false,  // is_const
          dil_procedure->IsAbstract(),
          dil_procedure->IsExternal(),
          false,  // is_native
          klass,
          pos));
    klass.AddFunction(function);
    function.set_dil_function(reinterpret_cast<intptr_t>(dil_procedure));
    function.set_result_type(AbstractType::dynamic_type());
    SetupFunctionParameters(klass, function, dil_procedure->function(), true);
    library.AddObject(function, name);
  }

  // TODO(kustermann): Support inheritance.
  dart::AbstractType& super_type =
      dart::Type::Handle(Z, dart::Type::ObjectType());
  klass.set_super_type(super_type);

  klass.set_is_finalized();
}

void DilReader::ReadProcedure(dart::Library& library,
                              dart::Class& owner,
                              Procedure* dil_procedure,
                              Class* dil_klass) {
  dart::String& name = DartSymbol(dil_procedure->name()->string());
  TokenPosition pos(0);
  bool is_method = dil_klass != NULL && !dil_procedure->IsStatic();
  dart::Function& function = dart::Function::Handle(Z, Function::New(
        name,
        GetFunctionType(dil_procedure),
        !is_method,  // is_static
        false,  // is_const
        dil_procedure->IsAbstract(),
        dil_procedure->IsExternal(),
        false,  // is_native
        owner,
        pos));
  owner.AddFunction(function);
  function.set_dil_function(reinterpret_cast<intptr_t>(dil_procedure));
  function.set_result_type(AbstractType::dynamic_type());

  SetupFunctionParameters(
      owner, function, dil_procedure->function(), is_method);

  library.AddObject(function, name);
  ASSERT(!Object::Handle(library.LookupObjectAllowPrivate(
          DartSymbol(dil_procedure->name()->string()))).IsNull());
}

void DilReader::SetupFunctionParameters(dart::Class& klass,
                                        dart::Function& function,
                                        FunctionNode* node,
                                        bool is_method) {
  int this_parameter = is_method ? 1 : 0;

  function.set_num_fixed_parameters(
      this_parameter + node->required_parameter_count());
  if (node->named_parameters().length() > 0) {
    function.SetNumOptionalParameters(
        node->named_parameters().length(), false);
  } else {
    function.SetNumOptionalParameters(
        node->positional_parameters().length() -
        node->required_parameter_count(),
        true);
  }
  int num_parameters =
      this_parameter +
      node->positional_parameters().length() +
      node->named_parameters().length();
  function.set_parameter_types(
      Array::Handle(Array::New(num_parameters, Heap::kOld)));
  function.set_parameter_names(
      Array::Handle(Array::New(num_parameters, Heap::kOld)));
  Type& klass_type = Type::Handle(Type::NewNonParameterizedType(klass));
  int pos = 0;
  if (is_method) {
    function.SetParameterTypeAt(pos, klass_type);
    function.SetParameterNameAt(pos, DartSymbol("this"));
    pos++;
  }
  for (int i = 0; i < node->positional_parameters().length(); i++, pos++) {
    function.SetParameterTypeAt(pos, AbstractType::dynamic_type());
    function.SetParameterNameAt(pos, DartSymbol("positional_parameter_x"));
  }
  for (int i = 0; i < node->named_parameters().length(); i++, pos++) {
    VariableDeclaration* named_expression = node->named_parameters()[i];
    function.SetParameterTypeAt(pos, AbstractType::dynamic_type());
    function.SetParameterNameAt(pos, DartSymbol(named_expression->name()));
  }
}

dart::Library& DilReader::LookupLibrary(Library* library) {
  dart::Library* handle = NULL;
  if (!libraries_.Lookup(library, &handle)) {
    handle = &dart::Library::Handle(Z);

    // If this is a core library, we'll use the VM version.
    if (library->IsCorelibrary()) {
      *handle = dart::Library::LookupLibrary(DartSymbol(library->name()));
    } else {
      *handle = dart::Library::New(DartSymbol(library->name()));
    }
    libraries_.Insert(library, *handle);
  }
  return *handle;
}

dart::Class& DilReader::LookupClass(Class* klass) {
  dart::Class* handle = NULL;
  if (!classes_.Lookup(klass, &handle)) {
    if (klass->parent()->IsCorelibrary()) {
      dart::Library& library = LookupLibrary(klass->parent());
      handle = &dart::Class::Handle(Z,
          library.LookupClass(DartSymbol(klass->name())));
    } else {
      TokenPosition pos(0);
      Script& script = Script::Handle(Z,
          Script::New(DartString(""), DartString(""), RawScript::kScriptTag));
      handle = &dart::Class::Handle(Z,
          dart::Class::New(DartSymbol(klass->name()), script, pos));
    }
    classes_.Insert(klass, *handle);
  }
  return *handle;
}

RawFunction::Kind DilReader::GetFunctionType(Procedure* dil_procedure) {
  // TODO(kustermann): Are these correct?
  int lookuptable[] = {
    RawFunction::kRegularFunction,  // Procedure::kMethod
    RawFunction::kGetterFunction,   // Procedure::kGetter
    RawFunction::kSetterFunction,   // Procedure::kSetter
    RawFunction::kRegularFunction,  // Procedure::kIndexGetter
    RawFunction::kRegularFunction,  // Procedure::kOperator
    RawFunction::kRegularFunction,  // Procedure::kFactory
  };
  int kind = static_cast<int>(dil_procedure->kind());
  if (kind == Procedure::kIncompleteProcedure) {
    // TODO(kustermann): Is this correct?
    return RawFunction::kSignatureFunction;
  } else {
    ASSERT(0 <= kind && kind <= Procedure::kFactory);
    return static_cast<RawFunction::Kind>(lookuptable[kind]);
  }
}

dart::String& DilReader::DartString(const char* string) {
  return dart::String::Handle(Z, dart::String::New(string));
}

dart::String& DilReader::DartString(String* string) {
  RawString* raw_string;
  if (string == NULL) {
    raw_string = dart::String::New("");
  } else {
    raw_string = dart::String::FromUTF8(string->buffer(), string->size());
  }
  return dart::String::Handle(Z, raw_string);
}

dart::String& DilReader::DartSymbol(const char* string) {
  return dart::String::Handle(Z, Symbols::New(string));
}

dart::String& DilReader::DartSymbol(String* string) {
  RawString* raw_string;
  if (string == NULL) {
    raw_string = Symbols::New("");
  } else {
    raw_string = Symbols::FromUTF8(string->buffer(), string->size());
  }
  return dart::String::Handle(Z, raw_string);
}

dart::RawString* DilReader::DartConstructorName(Constructor* node) {
  Class* klass = Class::Cast(node->parent());

  // We build a String which looks like <classname>.<constructor-name>.
  dart::String& temp = DartString(klass->name());
  temp ^= dart::String::Concat(temp, Symbols::Dot());
  temp ^= dart::String::Concat(
      temp, DartString(node->name()->string()));  // NOLINT
  return Symbols::New(temp);
}

}  // namespace dil

}  // namespace dart
