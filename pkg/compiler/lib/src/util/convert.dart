// Copyright (c) 2016, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.


/// Always import this library instead of `dart:convert` to ensure the compiler
/// recovers from encoding errors in input.
library dart2js.utf8;

import 'dart:convert' show Utf8Codec;

export 'dart:convert' hide UTF8, Utf8Codec, Utf8Decoder;

const Utf8Codec UTF8 = const Utf8Codec(allowMalformed: true);
