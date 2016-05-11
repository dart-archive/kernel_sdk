import 'dart:io' as io;

// We use this class until we support more language features to use
// `package:expect`.
class Expect {
  static void isTrue(bool condition) {
    if (!condition) {
      print("Expect.isTrue(cond) failed. io.exit(1)ing");
      io.exit(1);
    }
  }
}
