class A {
  A() {
    print("A");
  }

  hello() {
    print("A.hello()");
  }

  hello1(a) {
    print("A.hello1()");
    print(a);
  }

  foo(a, [b]) {
    print("A.foo()");
    print(a);
    print(b);
  }

  bar(a, {b}) {
    print("A.bar()");
    print(a);
    print(b);
  }
}

main() {
  print("before constructor");
  var a = new A();
  print("============");
  a.hello();
  print('-----(obj):');
  print(a);
  print('-----');
  a.hello1(1);
  print('-----');
  a.foo(1);
  print('-----');
  a.foo(1, 2);
  print('-----');
  print("============");
  a.bar(1);
  print('-----');
  a.bar(1, b: 2);
  print("============");
}

