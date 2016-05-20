import 'expect.dart';

main() {
  Expect.isTrue('${#abc}' == 'Symbol("abc")');
  Expect.isTrue('${#abc.xzy}' == 'Symbol("abc.xzy")');
}
