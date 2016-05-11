import 'expect.dart';

class A {
  getThis() => this;
}

main() {
  var a = new A();
  Expect.isTrue(a == a.getThis());
  Expect.isTrue(identical(a, a.getThis()));
}
