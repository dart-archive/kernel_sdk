// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:kernel/ast.dart' as ir;

import '../compiler.dart';
import '../constants/values.dart';
import '../diagnostics/invariant.dart';
import '../elements/elements.dart';
import '../js_backend/js_backend.dart';
import '../resolution/tree_elements.dart';
import '../tree/tree.dart' as ast;
import '../types/masks.dart';
import '../universe/call_structure.dart';
import '../universe/selector.dart';
import '../universe/side_effects.dart';

import 'types.dart';

/// A helper class that abstracts all accesses of the AST from Kernel nodes.
///
/// The goal is to remove all need for the AST from the Kernel SSA builder.
class KernelAstAdapter {
  final JavaScriptBackend _backend;
  final ResolvedAst _resolvedAst;
  final Map<ir.Node, ast.Node> _nodeToAst;
  final Map<ir.Node, Element> _nodeToElement;

  KernelAstAdapter(
      this._backend,
      this._resolvedAst,
      this._nodeToAst,
      this._nodeToElement,
      Map<FunctionElement, ir.Member> functions,
      Map<LibraryElement, ir.Library> libraries) {
    for (FunctionElement functionElement in functions.keys) {
      _nodeToElement[functions[functionElement]] = functionElement;
    }
    for (LibraryElement libraryElement in libraries.keys) {
      _nodeToElement[libraries[libraryElement]] = libraryElement;
    }
  }

  Compiler get _compiler => _backend.compiler;
  TreeElements get _elements => _resolvedAst.elements;

  ConstantValue getConstantForSymbol(ir.SymbolLiteral node) {
    ast.Node astNode = getNode(node);
    ConstantValue constantValue = _backend.constants
        .getConstantValueForNode(astNode, _resolvedAst.elements);
    assert(invariant(astNode, constantValue != null,
        message: 'No constant computed for $node'));
    return constantValue;
  }

  Element getElement(ir.Node node) {
    Element result = _nodeToElement[node];
    assert(result != null);
    return result;
  }

  ast.Node getNode(ir.Node node) {
    ast.Node result = _nodeToAst[node];
    assert(result != null);
    return result;
  }

  bool getCanThrow(ir.Procedure procedure) {
    FunctionElement function = getElement(procedure);
    return !_compiler.world.getCannotThrow(function);
  }

  TypeMask returnTypeOf(ir.Procedure node) {
    return TypeMaskFactory.inferredReturnTypeForElement(
        getElement(node), _compiler);
  }

  SideEffects getSideEffects(ir.Node node) {
    return _compiler.world.getSideEffectsOfElement(getElement(node));
  }

  // TODO(het): Create the selector directly from the invocation
  Selector getSelector(ir.MethodInvocation invocation) {
    SelectorKind kind = Elements.isOperatorName(invocation.name.name)
        ? SelectorKind.OPERATOR
        : SelectorKind.CALL;

    ir.Name irName = invocation.name;
    Name name = new Name(
        irName.name, irName.isPrivate ? getElement(irName.library) : null);

    int argumentCount = invocation.arguments.positional.length +
        invocation.arguments.named.length;
    List<String> namedArguments =
        invocation.arguments.named.map((e) => e.name).toList();
    CallStructure callStructure =
        new CallStructure(argumentCount, namedArguments);

    return new Selector(kind, name, callStructure);
  }

  TypeMask typeOfInvocation(ir.MethodInvocation invocation) {
    return _compiler.globalInference.results
        .typeOfSend(getNode(invocation), _elements);
  }

  TypeMask selectorTypeOf(ir.MethodInvocation invocation) {
    return TypeMaskFactory.inferredTypeForSelector(
        getSelector(invocation), typeOfInvocation(invocation), _compiler);
  }

  bool isIntercepted(ir.MethodInvocation invocation) {
    return _backend.isInterceptedSelector(getSelector(invocation));
  }
}
