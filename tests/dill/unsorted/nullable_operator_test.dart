import 'expect.dart';

mkOne() => 1;
mkNull() => null;

main() {
  Expect.isTrue((mkOne() ?? 2) == 1);
  Expect.isTrue((mkNull() ?? 2) == 2);
}
