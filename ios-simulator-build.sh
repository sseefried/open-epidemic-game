#!/bin/bash

#
# This script builds for iOS and then copies the static library to the Xcode
# project. This requires that an environment variable has been set
#

if [ "$EPIDEMIC_IOS_BUILD_DIR" = "" ]; then
  echo "You must set set EPIDEMIC_IOS_BUILD_LIB_DIR"
  exit 1
fi

i386-apple-darwin11-cabal configure -fios && \
  i386-apple-darwin11-cabal build exe:EpidemicStaticLib
[ $? -eq 0 ] || exit 1

echo
echo Copying to Epidemic to $EPIDEMIC_IOS_BUILD_DIR
echo
cp dist/i386-apple-darwin11/build/EpidemicStaticLib/EpidemicStaticLib.a \
  "$EPIDEMIC_IOS_BUILD_DIR/lib/i386/libEpidemic.a"
cp assets/* "$EPIDEMIC_IOS_BUILD_DIR/assets"