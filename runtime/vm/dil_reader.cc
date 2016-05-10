// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include "vm/dil_reader.h"

namespace dart {

#define Z (zone_)
#define I (isolate_)

Object& DilReader::ReadProgram() {
  String& error = NewString("Dill file reading not implemented yet.");
  return Object::Handle(Z, ApiError::New(error));
}

String& DilReader::NewString(const char* content) {
  return String::Handle(Z, String::New(content));
}

}  // namespace dart
