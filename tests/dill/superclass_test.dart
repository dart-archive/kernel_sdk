import 'expect.dart';

class A {
  A() {
    print("A");
  }

  hello() {
    print("A.hello()");
    return 1;
  }

  hello1(a) {
    print("A.hello1()");
    print(a);
    return 1;
  }

  foo(a, [b]) {
    print("A.foo()");
    print(a);
    print(b);
    return 1;
  }

  bar(a, {b}) {
    print("A.bar()");
    print(a);
    print(b);
    return 1;
  }
}

class B extends A {
  hello() {
    print("B.hello()");
    return 2;
  }

  bar(a, {b}) {
    print("B.bar()");
    print(a);
    print(b);
    return 2;
  }
}

main() {
  var o = new B();

  // Base class methods.
  Expect.isTrue(o.hello1(1) == 1);
  Expect.isTrue(o.foo(1) == 1);
  Expect.isTrue(o.foo(1, 2) == 1);

  // Overwritten methods.
  Expect.isTrue(o.hello() == 2);
  Expect.isTrue(o.bar(1) == 2);
  Expect.isTrue(o.bar(1, b: 2) == 2);
}
