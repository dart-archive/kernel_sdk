// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'expect.dart';

const X = const bool.fromEnvironment('foobar');

main() {
  Expect.isTrue(X == false);
}
