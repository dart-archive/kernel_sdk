#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SDK_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
DILC=$SDK_ROOT/third_party/kernel/bin/dartk.dart
DART_SDK="$(dirname $(dirname $(which dart)))";
FILE=$1

function compile {
  dartfile=$1
  dillfile=${dartfile%.dart}.dill

  echo "Compiling $dartfile to $dillfile"
  dart $DILC $dartfile --format=bin --link --sdk="$DART_SDK" --out=$dillfile
}

if [ -f "$FILE" ]; then
  compile $FILE
else
  for file in $CURRENT_DIR/*_test.dart; do
    compile $file
  done
fi

