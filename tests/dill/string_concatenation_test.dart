import 'expect.dart';

class A {
  toString() => 'hello';
}

main() {
  Expect.isTrue('begin ${new A()} end' == 'begin hello end');
}
