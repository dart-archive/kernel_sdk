main() {
  foo(1, 2);
  print('---');
  foo(1, 2, d: 12);
  print('---');
  foo(1, 2, c: 11, e: 13);

  print('=====');

  bar(1, 2);
  print('---');
  bar(1, 2, 3);
  print('---');
  bar(1, 2, 3, 4);
}

foo(a, b, {c, d, e}) {
  print(a);
  print(b);
  print(c);
  print(d);
  print(e);
}

bar(a, b, [c, d]) {
  print(a);
  print(b);
  print(c);
  print(d);
}

