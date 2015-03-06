#!/bin/bash

#
# This script builds for iOS on all 3 architectures and then copies each version of the
# static library to the Xcode

for arch in i386 arm aarch64; do
  env -i IOS_SCRIPTS=$IOS_SCRIPTS ./ios-$arch-build.sh
done