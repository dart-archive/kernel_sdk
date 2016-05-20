import 'expect.dart';

main() {
  var a = [1];
  Expect.isTrue((a..add(42))[1] == 42);
}
