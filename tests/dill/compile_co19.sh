#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SDK_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
INSTRUCTIONS=/tmp/batch_compilation_instructions.txt
KERNEL_DIR=$SDK_ROOT/third_party/kernel

rm -f $INSTRUCTIONS
touch $INSTRUCTIONS

echo "Discovering tests"
for co19dir in $(echo LibTest Language); do
  CO19_DIR="$SDK_ROOT/tests/co19/src/$co19dir"

  # NOTE: co19 tests don't end in _test.dart but the dill file will have a
  # _test.dill ending so ./tools/test.py can detect them.
  for testfile in $(find "$CO19_DIR" -type f -name '*.dart'); do
    testdir=$(dirname $testfile | sed "s#$CO19_DIR/#co19/$co19dir/#g")
    dillfile=$CURRENT_DIR/$(echo ${testfile%.dart}_test.dill | sed "s#$CO19_DIR/#co19/$co19dir/#g")
    echo $testfile $dillfile >> $INSTRUCTIONS
  done
done

echo "Generating Kernel IR for tests"
dart --package-root=$KERNEL_DIR/packages $CURRENT_DIR/compile_co19.dart $INSTRUCTIONS

