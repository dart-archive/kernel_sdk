// Copyright (c) 2012, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library dart._internal;

import 'dart:collection';

import 'dart:core' hide Symbol;
import 'dart:core' as core;
import 'dart:math' show Random;

part 'iterable.dart';
part 'list.dart';
part 'lists.dart';
part 'print.dart';
part 'sort.dart';
part 'symbol.dart';

// Powers of 10 up to 10^22 are representable as doubles.
// Powers of 10 above that are only approximate due to lack of precission.
// Used by double-parsing.
const POWERS_OF_TEN = const [
                        1.0,  /*  0 */
                       10.0,
                      100.0,
                     1000.0,
                    10000.0,
                   100000.0,  /*  5 */
                  1000000.0,
                 10000000.0,
                100000000.0,
               1000000000.0,
              10000000000.0,  /* 10 */
             100000000000.0,
            1000000000000.0,
           10000000000000.0,
          100000000000000.0,
         1000000000000000.0,  /*  15 */
        10000000000000000.0,
       100000000000000000.0,
      1000000000000000000.0,
     10000000000000000000.0,
    100000000000000000000.0,  /*  20 */
   1000000000000000000000.0,
  10000000000000000000000.0,
];

/**
 * An [Iterable] of the UTF-16 code units of a [String] in index order.
 */
class CodeUnits extends UnmodifiableListBase<int> {
  /** The string that this is the code units of. */
  final String _string;

  CodeUnits(this._string);

  int get length => _string.length;
  int operator[](int i) => _string.codeUnitAt(i);

  static String stringOf(CodeUnits u) => u._string;

  static final int cid = ClassID.getID(new CodeUnits(""));
}

class VMLibraryHooks {
  // Example: "dart:isolate _Timer._factory"
  static var timerFactory;

  // Example: "dart:io _EventHandler._sendData"
  static var eventHandlerSendData;

  // A nullary closure that answers the current clock value in milliseconds.
  // Example: "dart:io _EventHandler._timerMillisecondClock"
  static var timerMillisecondClock;

  // Implementation of Resource.readAsBytes.
  static var resourceReadAsBytes;

  // Implementation of package root/map provision.
  static var packageRootString;
  static var packageConfigString;
  static var packageRootUriFuture;
  static var packageConfigUriFuture;
  static var resolvePackageUriFuture;

  static var platformScript;
}

final bool is64Bit = _inquireIs64Bit();

external bool _inquireIs64Bit();

class ClassID {
  external static int getID(Object value);

  external static int get cidArray;

  external static int get cidExternalOneByteString;

  external static int get cidGrowableObjectArray;

  external static int get cidImmutableArray;

  external static int get cidOneByteString;

  external static int get cidTwoByteString;
}
