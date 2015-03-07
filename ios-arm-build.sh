#!/bin/bash

#
# This script builds for iOS and then copies the static library to the Xcode
# project. This requires that an environment variable has been set
#

ARCH_OS=arm-apple-darwin10

if [ "$IOS_SCRIPTS" = "" -o "$EPIDEMIC_IOS_BUILD_DIR" = "" ]; then
  echo "Either IOS_SCRIPTS or EPIDEMIC_IOS_BUILD_DIR is not set"
  exit 1
fi

source $IOS_SCRIPTS/ios-arm-env.rc


$ARCH_OS-cabal configure -fios $@ && \
  $ARCH_OS-cabal build exe:EpidemicStaticLib
[ $? -eq 0 ] || exit 1

echo
echo Copying to Epidemic to $EPIDEMIC_IOS_BUILD_DIR
echo
cp dist/$ARCH_OS/build/EpidemicStaticLib/EpidemicStaticLib.a \
  "$EPIDEMIC_IOS_BUILD_DIR/lib/armv7/libEpidemic.a"
cp assets/* "$EPIDEMIC_IOS_BUILD_DIR/assets"