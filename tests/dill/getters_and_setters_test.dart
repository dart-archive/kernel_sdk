import 'expect.dart';

var field;

get getField {
  print('B.getField');
  return field;
}

set setField(value) {
  print('B.setField');
  field = value;
  return null;
}

main() {
  var result;

  result = (field = 42);
  Expect.isTrue(result == 42);
  Expect.isTrue(field == 42);
  Expect.isTrue(getField == 42);

  result = (setField = 99);
  Expect.isTrue(result == 99);
  Expect.isTrue(field == 99);
  Expect.isTrue(getField == 99);
}
