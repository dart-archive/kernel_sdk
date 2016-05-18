import 'expect.dart';

class A {
  var field;

  get getField {
    return field;
  }

  set setField(value) {
    field = value;
    return null;
  }
}

main() {
  var result;
  var a = new A();

  Expect.isTrue(a.field == null);
  Expect.isTrue(a.getField == null);

  result = (a.field = 42);
  Expect.isTrue(result == 42);
  Expect.isTrue(a.field == 42);
  Expect.isTrue(a.getField == 42);

  result = (a.setField = 99);
  Expect.isTrue(result == 99);
  Expect.isTrue(a.field == 99);
  Expect.isTrue(a.getField == 99);
}
