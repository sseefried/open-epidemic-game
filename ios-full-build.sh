#!/bin/bash

#
# This script builds for iOS on all 3 architectures and then copies each version of the
# static library to the Xcode

if [ "$IOS_SCRIPTS" = "" -o "$EPIDEMIC_IOS_BUILD_DIR" = "" ]; then
  echo "Either IOS_SCRIPTS or EPIDEMIC_IOS_BUILD_DIR is not set"
  exit 1
fi

for arch in i386 arm aarch64; do
  env -i IOS_SCRIPTS=$IOS_SCRIPTS EPIDEMIC_IOS_BUILD_DIR=$EPIDEMIC_IOS_BUILD_DIR ./ios-$arch-build.sh $@
  [ $? -eq 0 ] || exit 1
done