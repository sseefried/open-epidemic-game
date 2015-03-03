#!/bin/bash

if [ "$EPIDEMIC_IOS_BUILD_I386_LIB_DIR" = "" ]; then
  echo "You must set set EPIDEMIC_IOS_BUILD_I386_LIB_DIR"
  exit 1
fi

echo Copying to Epidemic to $EPIDEMIC_IOS_BUILD_I386_LIB_DIR
cp dist/i386-apple-darwin11/build/EpidemicStaticLib/EpidemicStaticLib.a \
  "$EPIDEMIC_IOS_BUILD_I386_LIB_DIR/libEpidemic.a"
