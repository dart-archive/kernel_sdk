// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

#include <unistd.h>

#include "vm/dil.h"
#include "vm/timer.h"

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("Usage: %s <input.dil> <output.dil>\n", argv[0]);
    return 1;
  }

  char* filename = argv[1];
  char* out_filename = argv[2];

  dart::dil::Program* dil = NULL;
  dart::Timer reader_timer(true, "");
  {
    dart::TimerScope ts(true, &reader_timer);
    dil = dart::ReadPrecompiledDil(filename);
    if (dil == NULL) {
      FATAL("Failed to read input file.");
    }
  }

  dart::Timer writer_time(true, "");
  {
    dart::TimerScope ts(true, &writer_time);
    if (!dart::WritePrecompiledDil(out_filename, dil)) {
      FATAL("Failed to write output file.");
    }
  }

  dart::Timer delete_timer(true, "");
  {
    dart::TimerScope ts(true, &delete_timer);
    delete dil;
  }

  printf("Took %4.2lf ms to read\n", reader_timer.TotalElapsedTime() / 1000.0);
  printf("Took %4.2lf ms to write\n", writer_time.TotalElapsedTime() / 1000.0);
  printf("Took %4.2lf ms to delete\n", delete_timer.TotalElapsedTime() / 1000.0);

  return 0;
}
