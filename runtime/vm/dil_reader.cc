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
#define H (translation_helper_)

// FIXME(kustermann): We should add support for type conversion to annotate
// fields/parameters/variables with proper types.

class SimpleExpressionConverter : public ExpressionVisitor {
 public:
  explicit SimpleExpressionConverter(Zone* zone)
      : translation_helper_(zone),
        zone_(zone), is_simple_(false), simple_value_(NULL) {}

  virtual void VisitDefaultExpression(Expression* node) {
    is_simple_ = false;
  }

  virtual void VisitIntLiteral(IntLiteral* node) {
    is_simple_ = true;
    simple_value_ = &dart::Integer::ZoneHandle(
        Z, dart::Integer::New(node->value(), Heap::kOld));
  }

  virtual void VisitDoubleLiteral(DoubleLiteral* node) {
    is_simple_ = true;
    simple_value_ = &Double::ZoneHandle(
        Z, dart::Double::New(H.DartString(node->value()), Heap::kOld));
  }

  virtual void VisitBoolLiteral(BoolLiteral* node) {
    is_simple_ = true;
    simple_value_ = &dart::Bool::Get(node->value());
  }

  virtual void VisitNullLiteral(NullLiteral* node) {
    is_simple_ = true;
    simple_value_ = &dart::Instance::ZoneHandle(Z, dart::Instance::null());
  }

  virtual void VisitStringLiteral(StringLiteral* node) {
    is_simple_ = true;
    simple_value_ = &H.DartString(node->value(), Heap::kOld);
  }

  bool IsSimple(Expression* expression) {
    expression->AcceptExpressionVisitor(this);
    return is_simple_;
  }

  const dart::Instance& SimpleValue() { return *simple_value_; }

 private:
  TranslationHelper translation_helper_;
  dart::Zone* zone_;
  bool is_simple_;
  const dart::Instance* simple_value_;
};


Object& DilReader::ReadProgram() {
  Program* program = ReadPrecompiledDilFromBuffer(buffer_, buffer_length_);
  if (program == NULL) {
    const dart::String& error = H.DartString("Failed to read .dill file");
    return Object::Handle(Z, ApiError::New(error));
  }

  Procedure* main = program->main_method();
  Library* dil_main_library = Library::Cast(main->parent());

  intptr_t length = program->libraries().length();
  for (int i = 0; i < length; i++) {
    Library* dil_library = program->libraries()[i];
    ReadLibrary(dil_library);
  }

  // We finalize classes after we've constructed all classes since we currently
  // don't construct them in pre-order of the class hierarchy (and finalization
  // of a class needs all of its superclasses to be finalized).
  for (int i = 0; i < length; i++) {
    Library* dil_library = program->libraries()[i];
    if (!dil_library->IsCorelibrary()) {
      for (int i = 0; i < dil_library->classes().length(); i++) {
        Class* dil_klass = dil_library->classes()[i];
        ClassFinalizer::FinalizeClass(LookupClass(dil_klass));
      }
    }
  }

  dart::Library& library = LookupLibrary(dil_main_library);

  // Sanity check that we can find the main entrypoint.
  Object& main_obj = Object::Handle(Z,
      library.LookupObjectAllowPrivate(H.DartSymbol("main")));
  ASSERT(!main_obj.IsNull());

  return library;
}

void DilReader::ReadLibrary(Library* dil_library) {
  dart::Library& library = LookupLibrary(dil_library);
  if (!dil_library->IsCorelibrary()) {
    // Setup toplevel class (which contains library fields/procedures).

    // FIXME(kustermann): Figure out why we need this script stuff here.
    Script& script = Script::Handle(Z,
        Script::New(H.DartString(""), H.DartString(""), RawScript::kScriptTag));
    script.SetLocationOffset(0, 0);
    script.Tokenize(H.DartString("nop() {}"));
    dart::Class& toplevel_class = dart::Class::Handle(dart::Class::New(
          Symbols::TopLevel(), script, TokenPosition::kNoSource));
    toplevel_class.set_library(library);
    library.set_toplevel_class(toplevel_class);

    // Load toplevel fields.
    for (int i = 0; i < dil_library->fields().length(); i++) {
      Field* dil_field = dil_library->fields()[i];

      const dart::String& name = H.DartSymbol(dil_field->name()->string());
      dart::Field& field = dart::Field::Handle(Z, dart::Field::NewTopLevel(
            name,
            dil_field->IsConst(),
            dil_field->IsStatic(),
            toplevel_class,
            TokenPosition::kNoSource));
      GenerateStaticFieldInitializer(field, dil_field);
      GenerateFieldAccessors(toplevel_class, field, dil_field);
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

void DilReader::ReadPreliminaryClass(dart::Class* klass, Class* dil_klass) {
  if (dil_klass->IsNormalClass()) {
    NormalClass* dil_normal_class = NormalClass::Cast(dil_klass);
    InterfaceType* super_class_type = dil_normal_class->super_class();
    Class* dil_super_class = super_class_type->klass();
    dart::Class& super_class = LookupClass(dil_super_class);

    TypeArguments& type_args = TypeArguments::Handle(Z);
    Type& super_type = Type::Handle(Z,
        Type::New(super_class, type_args, TokenPosition::kNoSource));
    klass->set_super_type(super_type);
  } else {
    MixinClass* dil_mixin = MixinClass::Cast(dil_klass);
    dart::Class& base = LookupClass(dil_mixin->first()->klass());
    dart::Class& mixin = LookupClass(dil_mixin->second()->klass());

    // Make types for [base] and [mixin]
    TypeArguments& null_type_args = TypeArguments::Handle(Z);
    Type& base_type = Type::Handle(Z,
        Type::New(base, null_type_args, TokenPosition::kNoSource));
    Type& mixin_type = Type::Handle(Z,
        Type::New(mixin, null_type_args, TokenPosition::kNoSource));

    // Tell the rest of the system there is nothing to resolve.
    base_type.SetIsResolved();

    // Build implemented interface types
    const dart::Array& interfaces = dart::Array::Handle(Z, dart::Array::New(1));
    interfaces.SetAt(0, mixin_type);

    klass->set_super_type(base_type);
    klass->set_mixin(mixin_type);
    klass->set_interfaces(interfaces);
  }
  ClassFinalizer::FinalizeTypesInClass(*klass);
}

void DilReader::ReadClass(const dart::Library& library, Class* dil_klass) {
  // This will trigger a call to [ReadPreliminaryClass] if not already done.
  dart::Class& klass = LookupClass(dil_klass);

  klass.set_library(library);
  library.AddClass(klass);

  TokenPosition pos(0);

  Type& klass_type = Type::Handle(Type::NewNonParameterizedType(klass));

  for (int i = 0; i < dil_klass->fields().length(); i++) {
    Field* dil_field = dil_klass->fields()[i];

    const dart::String& name = H.DartSymbol(dil_field->name()->string());
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

    if (dil_field->IsStatic()) {
      GenerateStaticFieldInitializer(field, dil_field);
    }
    GenerateFieldAccessors(klass, field, dil_field);

    klass.AddField(field);
  }

  for (int i = 0; i < dil_klass->constructors().length(); i++) {
    Constructor* dil_constructor = dil_klass->constructors()[i];

    const dart::String& name = H.DartConstructorName(dil_constructor);
    Function& function = dart::Function::ZoneHandle(Z, dart::Function::New(
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
    ReadProcedure(library, klass, dil_procedure, dil_klass);
  }
}

void DilReader::ReadProcedure(const dart::Library& library,
                              const dart::Class& owner,
                              Procedure* dil_procedure,
                              Class* dil_klass) {
  const dart::String& name = H.DartProcedureName(dil_procedure);
  TokenPosition pos(0);
  bool is_method = dil_klass != NULL && !dil_procedure->IsStatic();
  dart::Function& function = dart::Function::ZoneHandle(Z, Function::New(
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
  function.set_is_debuggable(false);

  SetupFunctionParameters(
      owner, function, dil_procedure->function(), is_method);

  if (dil_klass == NULL) {
    library.AddObject(function, name);
    ASSERT(!Object::Handle(library.LookupObjectAllowPrivate(
        H.DartProcedureName(dil_procedure))).IsNull());
  }
}

void DilReader::GenerateFieldAccessors(const dart::Class& klass,
                                       const dart::Field& field,
                                       Field* dil_field) {
  TokenPosition pos(0);

  // For static fields we only need the getter if the field is lazy-initialized.
  if (dil_field->IsStatic() && !field.has_initializer()) return;

  const dart::String& getter_name =
      H.DartGetterName(dil_field->name()->string());
  dart::Function& getter = dart::Function::ZoneHandle(Z, dart::Function::New(
      getter_name,
      dart::RawFunction::kImplicitGetter,
      dil_field->IsStatic(),
      dil_field->IsFinal(),
      false,  // is_abstract
      false,  // is_external
      false,  // is_native
      klass,
      pos));
  klass.AddFunction(getter);
  getter.set_dil_function(reinterpret_cast<intptr_t>(dil_field));
  getter.set_result_type(AbstractType::dynamic_type());
  getter.set_is_debuggable(false);
  SetupFieldAccessorFunction(klass, getter);

  if (!dil_field->IsStatic() && !dil_field->IsFinal()) {
    const dart::String& setter_name =
        H.DartSetterName(dil_field->name()->string());  // NOLINT
    dart::Function& setter = dart::Function::ZoneHandle(Z, dart::Function::New(
          setter_name,
          RawFunction::kImplicitSetter,
          dil_field->IsStatic(),
          dil_field->IsFinal(),
          false,  // is_abstract
          false,  // is_external
          false,  // is_native
          klass,
          pos));
    klass.AddFunction(setter);
    setter.set_dil_function(reinterpret_cast<intptr_t>(dil_field));
    setter.set_result_type(Object::void_type());
    setter.set_is_debuggable(false);
    SetupFieldAccessorFunction(klass, setter);
  }
}

void DilReader::SetupFunctionParameters(const dart::Class& klass,
                                        const dart::Function& function,
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
    function.SetParameterNameAt(pos, H.DartSymbol("this"));
    pos++;
  }
  for (int i = 0; i < node->positional_parameters().length(); i++, pos++) {
    VariableDeclaration* dil_variable = node->positional_parameters()[i];
    function.SetParameterTypeAt(pos, AbstractType::dynamic_type());
    function.SetParameterNameAt(pos, H.DartSymbol(dil_variable->name()));
  }
  for (int i = 0; i < node->named_parameters().length(); i++, pos++) {
    VariableDeclaration* named_expression = node->named_parameters()[i];
    function.SetParameterTypeAt(pos, AbstractType::dynamic_type());
    function.SetParameterNameAt(pos, H.DartSymbol(named_expression->name()));
  }
}

void DilReader::SetupFieldAccessorFunction(const dart::Class& klass,
                                           const dart::Function& function) {
  bool is_setter = function.IsImplicitSetterFunction();
  bool is_method = !function.IsStaticFunction();
  int num_parameters = (is_method ? 1 : 0) + (is_setter ? 1 : 0);

  function.SetNumOptionalParameters(0, false);
  function.set_num_fixed_parameters(num_parameters);
  function.set_parameter_types(
      Array::Handle(Array::New(num_parameters, Heap::kOld)));
  function.set_parameter_names(
      Array::Handle(Array::New(num_parameters, Heap::kOld)));

  Type& klass_type = Type::Handle(Type::NewNonParameterizedType(klass));

  int pos = 0;
  if (is_method) {
    function.SetParameterTypeAt(pos, klass_type);
    function.SetParameterNameAt(pos, H.DartSymbol("this"));
    pos++;
  }
  if (is_setter) {
    function.SetParameterTypeAt(pos, AbstractType::dynamic_type());
    function.SetParameterNameAt(pos, H.DartSymbol("value"));
    pos++;
  }
}

void DilReader::GenerateStaticFieldInitializer(const dart::Field& field,
                                               Field* dil_field) {
  Expression* initializer = dil_field->initializer();
  if (initializer != NULL) {
    field.set_has_initializer(true);

    SimpleExpressionConverter converter(Z);
    if (converter.IsSimple(initializer)) {
      field.SetStaticValue(converter.SimpleValue(), true);
    } else {
      field.SetStaticValue(Object::null_instance(), true);

      // Create a static final getter.
      dart::Class& owner = dart::Class::Handle(Z, field.Owner());
      const dart::String& getter_name =
          H.DartGetterName(dil_field->name()->string());  // NOLINT
      dart::Function& getter = dart::Function::Handle(Z,
          dart::Function::New(getter_name,
                              RawFunction::kImplicitStaticFinalGetter,
                              true,  // is_static
                              false,  // is_const
                              false,  // is_abstract
                              false,  // is_external
                              false,  // is_native
                              owner,
                              TokenPosition::kNoSource));
      getter.set_dil_function(reinterpret_cast<intptr_t>(dil_field));
      getter.set_result_type(AbstractType::dynamic_type());
      getter.set_is_debuggable(false);
      getter.set_is_reflectable(false);
      owner.AddFunction(getter);
    }
  }
}

dart::Library& DilReader::LookupLibrary(Library* library) {
  dart::Library* handle = NULL;
  if (!libraries_.Lookup(library, &handle)) {
    handle = &dart::Library::Handle(Z);

    // If this is a core library, we'll use the VM version.
    const dart::String& url = H.DartSymbol(library->import_uri());
    if (library->IsCorelibrary()) {
      *handle = dart::Library::LookupLibrary(url);
    } else {
      *handle = dart::Library::New(url);
      handle->Register();
    }
    ASSERT(!handle->IsNull());
    libraries_.Insert(library, handle);
  }
  return *handle;
}

dart::Class& DilReader::LookupClass(Class* klass) {
  dart::Class* handle = NULL;
  if (!classes_.Lookup(klass, &handle)) {
    const dart::String& name = H.DartClassName(klass);
    if (klass->parent()->IsCorelibrary()) {
      dart::Library& library = LookupLibrary(klass->parent());
      handle = &dart::Class::Handle(Z, library.LookupClass(name));
      classes_.Insert(klass, handle);
    } else {
      TokenPosition pos(0);
      Script& script = Script::Handle(Z, Script::New(H.DartString(""),
          H.DartString(""), RawScript::kScriptTag));
      handle = &dart::Class::Handle(Z, dart::Class::New(name, script, pos));
      classes_.Insert(klass, handle);

      ReadPreliminaryClass(handle, klass);
    }
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

}  // namespace dil
}  // namespace dart
