#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SDK_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"

for co19dir in $(echo LibTest Language); do
  CO19_DIR="$SDK_ROOT/tests/co19/src/$co19dir"

  # NOTE: co19 tests don't end in _test.dart but the dill file will have a
  # _test.dill ending so ./tools/test.py can detect them.
  for testfile in $(find "$CO19_DIR" -type f -name '*.dart'); do
    testdir=$(dirname $testfile | sed "s#$CO19_DIR/#co19/$co19dir/#g")
    dillfile=$(echo ${testfile%.dart}_test.dill | sed "s#$CO19_DIR/#co19/$co19dir/#g")

    # We only use positive tests for now.
    dart $testfile &> /dev/null
    exitcode=$?
    if [ $exitcode -eq 0 ]; then
      mkdir -p $testdir
      ./compile.sh $testfile $dillfile
    fi
  done
done
