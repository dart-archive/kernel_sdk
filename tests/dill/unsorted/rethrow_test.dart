import 'expect.dart';

testNormalRethrow() {
  var x = 0;
  try {
    try {
      throw x++;
    } catch (e) {
      Expect.isTrue(e == 0);
      x++;
      rethrow;
    }
  } catch (e) {
    Expect.isTrue(e == 0);
    x++;
  }
  Expect.isTrue(x == 3);
}

testNormalRethrow2() {
  var x = 0;
  try {
    try {
      throw x++;
    } on int catch (e) {
      Expect.isTrue(e == 0);
      x++;
      rethrow;
    }
  } catch (e) {
    Expect.isTrue(e == 0);
    x++;
  }
  Expect.isTrue(x == 3);
}

testRethrowWithinTryRethrow() {
  var x = 0;
  try {
    try {
      throw x++;
    } on int catch (e) {
      Expect.isTrue(e == 0);
      x++;
      try {
        x++;
        rethrow;
      } finally {
        x++;
      }
    }
  } catch (e) {
    Expect.isTrue(e == 0);
    x++;
  }
  Expect.isTrue(x == 5);
}

main() {
  testNormalRethrow();
  testNormalRethrow2();
  testRethrowWithinTryRethrow();
}
