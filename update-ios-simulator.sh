#!/bin/bash

if [ "$EPIDEMIC_IOS_BUILD_I386_LIB_DIR" = "" ]; then
  echo "You must set set EPIDEMIC_IOS_BUILD_I386_LIB_DIR"
  exit 1
fi

i386-apple-darwin11-cabal configure -fios && \
  i386-apple-darwin11-cabal build exe:EpidemicStaticLib
[ $? -eq 0 ] || exit 1

echo
echo Copying to Epidemic to $EPIDEMIC_IOS_BUILD_I386_LIB_DIR
echo
cp dist/i386-apple-darwin11/build/EpidemicStaticLib/EpidemicStaticLib.a \
  "$EPIDEMIC_IOS_BUILD_I386_LIB_DIR/libEpidemic.a"
