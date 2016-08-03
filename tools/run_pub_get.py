#!/usr/bin/env python
# Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
# for details. All rights reserved. Use of this source code is governed by a
# BSD-style license that can be found in the LICENSE file.

import os
import subprocess
import sys
import utils

DART_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
BUILD_OS = utils.GuessOS()
PUB = os.path.join(
    DART_DIR, 'tools', 'sdks', BUILD_OS, 'dart-sdk', 'bin', 'pub')

def die(msg):
  print msg
  sys.exit(1)

def main():
  if len(sys.argv) != 2:
    die("Usage: %s <directory>" % sys.argv[0])

  directory = os.path.abspath(sys.argv[1])

  if not os.path.isdir(directory):
    die("Directory '%s' does not exist!" % directory)

  subprocess.check_call([PUB, 'get'], cwd=directory);

if __name__ == '__main__':
  main()
