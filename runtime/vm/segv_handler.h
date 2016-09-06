// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#ifndef VM_SEGV_HANDLER_H_
#define VM_SEGV_HANDLER_H_

#include <signal.h>

#include "vm/object.h"

namespace dart {

#if defined(USE_STACKOVERFLOW_TRAPS) && defined(DART_PRECOMPILED_RUNTIME)
class SegvHandler {
 public:
  static void InitOnce();
  static void TearDown();

 private:
  static void SignalHandler(int signal, siginfo_t* siginfo, void* context);
};
#endif  // defined(USE_STACKOVERFLOW_TRAPS) && defined(DART_PRECOMPILED_RUNTIME)

}  // namespace dart

#endif  // VM_SEGV_HANDLER_H_
