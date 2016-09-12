// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

class ClassID {
  static int getID(Object value) native "ClassID_getID";

  static int get cidArray native "ClassID_getCidArray";

  static int get cidExternalOneByteString
      native "ClassID_getCidExternalOneByteString";

  static int get cidGrowableObjectArray
      native "ClassID_getCidGrowableObjectArray";

  static int get cidImmutableArray native "ClassID_getCidImmutableArray";

  static int get cidOneByteString native "ClassID_getCidOneByteString";

  static int get cidTwoByteString native "ClassID_getCidTwoByteString";

  static int get cidBigint native "ClassID_getCidBigint";
}
