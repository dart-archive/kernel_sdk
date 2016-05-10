// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_DIL_READER_H_
#define VM_DIL_READER_H_

#include "vm/dil.h"
#include "vm/object.h"

namespace dart {

class DilReader {
 public:
  DilReader(const uint8_t* buffer, intptr_t len)
      : zone_(Thread::Current()->zone()),
        isolate_(Thread::Current()->isolate()),
        buffer_(buffer),
        buffer_length_(len) {}

  // Returns either a library or a failure object.
  Object& ReadProgram();

 private:
  String& NewString(const char* content);

  Zone* zone_;
  Isolate* isolate_;
  const uint8_t* buffer_;
  intptr_t buffer_length_;
};

}  // namespace dart

#endif  // VM_DIL_READER_H_
