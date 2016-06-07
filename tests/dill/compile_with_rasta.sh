#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SDK_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
RASTA_ROOT=$SDK_ROOT/third_party/rasta
DILC=bin/rastak
DART_SDK="$(dirname $(dirname $(which dart)))";
FILE=$1
OUTPUT_FILE=$2

function compile {
  dartfile=$(realpath $1)
  if [ -z "$2" ]; then
    dillfile=${dartfile%.dart}.dill
  else
    dillfile=$2
  fi

  echo "Compiling $dartfile to $dillfile"
  pushd $RASTA_ROOT
  $DILC $dartfile $dillfile
  popd
}

if [ -f "$FILE" ]; then
  compile $FILE $OUTPUT_FILE
else
  for file in $CURRENT_DIR/unsorted/*_test.dart; do
    compile $file
  done
fi

