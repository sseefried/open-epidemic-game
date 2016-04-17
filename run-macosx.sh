#!/bin/bash

BIN_PATH=$(stack path | grep 'local-install-root' | sed 's/local-install-root: //')/bin
if [ -d $BIN_PATH ]; then
  cp assets/* $BIN_PATH
  stack exec Epidemic
fi