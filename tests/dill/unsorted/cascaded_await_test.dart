import 'dart:async';

Future<int> something() async => 45;

class Foo {
  var field;
  Foo() {
    field = this;
  }
  doSomething(x) {
    print(x);
  }
  blah() {}
}

main() async {
  var obj = new Foo();
  obj.field
    ..blah()
    ..doSomething(await something());
}
