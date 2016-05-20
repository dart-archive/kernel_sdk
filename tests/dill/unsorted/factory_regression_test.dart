import 'dart:typed_data';

import 'expect.dart';

var list = [1, 2, 3];

class Foo {
  final value;
  Foo(this.value) {}

  factory Foo.fac(value) {
    return new Foo(value);
  }
}

main() {
  Expect.isTrue(new Uint8List.fromList(list)[1] == 2);
  Expect.isTrue(new Foo.fac(10).value == 10);
}
