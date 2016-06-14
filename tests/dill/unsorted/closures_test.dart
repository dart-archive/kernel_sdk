// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'expect.dart';

class Foo {
  final int base;
  Foo(this.base);

  nestedAdderFunction(a, b, c, d, e) {
    var result = a + b;
    return () {
      var result2 = c + d;
      return () {
        return base + result + result2 + e;
      };
    };
  }
}

nestedAdderFunction(a, b, c, d, e) {
  var result = a + b;
  return () {
    var result2 = c + d;
    return () {
      return result + result2 + e;
    };
  };
}

main() {
  Expect.isTrue(nestedAdderFunction(1, 2, 3, 4, 5)()() == 15);

  var foo = new Foo(100);
  Expect.isTrue(foo.nestedAdderFunction(1, 2, 3, 4, 5)()() == 115);
}
