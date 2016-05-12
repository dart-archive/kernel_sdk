// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

class ClassID {
  static int getID(Object value) native "ClassID_getID";

  static int get cidArray => throw "bork";

  static int get cidExternalOneByteString => throw "bork";

  static int get cidGrowableObjectArray => throw "bork";

  static int get cidImmutableArray => throw "bork";

  static int get cidOneByteString => throw "bork";

  static int get cidTwoByteString => throw "bork";
}
