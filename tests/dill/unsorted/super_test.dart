import 'expect.dart';

class A {
  var field = 9;
  var called = false;

  superMethod() {
    Expect.isTrue(field == 10);
    called = true;
    return true;
  }
}

class B extends A {
  doit() {
    Expect.isTrue((super.field = 10) == 10);
    Expect.isTrue(super.superMethod());
    if (called) {
      Expect.isTrue((super.field = 11) == 11);
    }
    return super.field;
  }
}

class C extends B {
  set field(v) {
    throw 'should not happen';
  }
}

main() {
  var c = new C();
  Expect.isTrue(c.field == 9);
  Expect.isTrue(c.doit() == 11);
  Expect.isTrue(c.field == 11);
}
