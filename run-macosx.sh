#!/bin/bash

if [ $# -gt 0 ]; then
  case "$1" in
    --debug)
      DEBUG_FLAG=-fdebug-game
    ;;
  esac

fi

cabal configure $DEBUG_FLAG && \
  cabal build exe:Epidemic && \
  macosx-app dist/build/Epidemic/Epidemic && \
  open Epidemic.app

cp assets/* Epidemic.app/Contents/Resources