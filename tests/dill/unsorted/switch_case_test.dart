import 'expect.dart';

mkOne() => 1;
mkTwo() => 2;

testNormal() {
  int result;
  switch(mkOne()) {
    case 0:
      result = 0;
      break;
    case 1: 
      result = 1;
      break;
    case 2:
      result = 2;
      break;
    default:
      result = 3;
      break;
  }
  Expect.isTrue(result == 1);
}

testDefault() {
  int result;
  switch(null) {
    case 0:
      result = 0;
      break;
    case 1: 
      result = 1;
      break;
    case 2:
      result = 2;
      break;
    default:
      result = 3;
      break;
  }
  Expect.isTrue(result == 3);
}

testFallThrough() {
  int result;
  switch(mkOne()) {
    case 0:
      result = 0;
      break;
    case 1: 
    case 2:
      result = 2;
      break;
    default:
      result = 3;
      break;
  }
  Expect.isTrue(result == 2);
}

testContinue() {
  int result;
  switch(mkTwo()) {
    case 0:
      result = 0;
      break;

    setitto1:
    case 1: 
      result = 1;
      continue setitto3;

    case 2:
      result = 2;
      continue setitto1;

    setitto3:
    default:
      result = 3;
      break;
  }
  Expect.isTrue(result == 3);
}

testOnlyDefault() {
  int result;
  switch(mkTwo()) {
    default:
      result = 42;
  }
  Expect.isTrue(result == 42);
}

testOnlyDefaultWithBreak() {
  int result;
  switch(mkTwo()) {
    default:
      result = 42;
  }
  Expect.isTrue(result == 42);
}

String testReturn() {
  switch(mkOne()) {
    case 0: return "bad";
    case 1: return "good";
    default: return "bad";
  }
}

main() {
  testNormal();
  testDefault();
  testFallThrough();
  testContinue();
  testOnlyDefault();
  testOnlyDefaultWithBreak();

  var result = testReturn();
  Expect.isTrue(result == "good");
}
