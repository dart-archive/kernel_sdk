#!/usr/bin/env python
# Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
# for details. All rights reserved. Use of this source code is governed by a
# BSD-style license that can be found in the LICENSE file.

import os
import string
import subprocess
import sys

import utils

DART_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
BUILD_OS = utils.GuessOS()
DART_BIN = os.path.join(DART_DIR, 'tools', 'sdks', BUILD_OS, 'dart-sdk', 'bin')

def Main():
  args = sys.argv[1:]
  tools_dir = os.path.dirname(os.path.realpath(__file__))
  dart_script_name = 'test.dart'
  dart_test_script = string.join([tools_dir, dart_script_name], os.sep)
  command = [utils.CheckedInSdkExecutable(),
             '--checked', dart_test_script] + args

  # The testing script potentially needs the android platform tools in PATH so
  # we do that in ./tools/test.py (a similar logic exists in ./tools/build.py).
  android_platform_tools = os.path.normpath(os.path.join(
      tools_dir,
      '../third_party/android_tools/sdk/platform-tools'))
  if os.path.isdir(android_platform_tools):
    os.environ['PATH'] = '%s%s%s' % (
        os.environ['PATH'], os.pathsep, android_platform_tools)

  # We give the pinpointed version of the dart-sdk priority, so everybody
  # (inlcuding the buildbot) is using the same dart binary.
  # TODO(kustermann): For supporting windows we need to rewrite our
  # shebang-using dart scripts to using shell/bat scripts.
  os.environ['PATH'] = '%s%s%s' % (DART_BIN, os.pathsep, os.environ['PATH'])

  exit_code = subprocess.call(command)
  utils.DiagnoseExitCode(exit_code, command)
  return exit_code


if __name__ == '__main__':
  sys.exit(Main())
