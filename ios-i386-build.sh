#!/bin/bash

#
# This script builds for iOS and then copies the static library to the Xcode
# project. This requires that an environment variable has been set
#

if [ "$IOS_SCRIPTS" = "" ]; then
  echo "IOS_SCRIPTS environment variable is not set"
  exit 1
fi

source $IOS_SCRIPTS/ios-i386-env.rc

i386-apple-darwin11-cabal configure -fios $@ && \
  i386-apple-darwin11-cabal build exe:EpidemicStaticLib
[ $? -eq 0 ] || exit 1

echo
echo Copying to Epidemic to $EPIDEMIC_IOS_BUILD_DIR
echo
cp dist/i386-apple-darwin11/build/EpidemicStaticLib/EpidemicStaticLib.a \
  "$EPIDEMIC_IOS_BUILD_DIR/lib/i386/libEpidemic.a"
cp assets/* "$EPIDEMIC_IOS_BUILD_DIR/assets"