#!/usr/bin/env python
# Copyright (c) 2011, the Dart project authors.  Please see the AUTHORS file
# for details. All rights reserved. Use of this source code is governed by a
# BSD-style license that can be found in the LICENSE file.

import os
import string
import subprocess
import sys

import utils

def die(message):
  print >> sys.stderr, message
  print >> sys.stderr, 'Usage: %s <path-to-git-checkout>' % sys.argv[0]
  sys.exit(1)

def main():
  args = sys.argv[1:]
  if len(args) != 1:
    die('Invalid number of arguments');

  directory = args[0]
  git_directory = os.path.join(directory, '.git')
  if not os.path.exists(git_directory):
    die('The directory "%s" does not exist' % git_directory)

  subprocess.check_call(
      ['git', 'submodule', 'update', '--init'],
      cwd=directory)

if __name__ == '__main__':
  sys.exit(main())
