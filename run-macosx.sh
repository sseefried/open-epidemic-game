#!/bin/bash

if [ $# -gt 0 ]; then
  for i in $@; do
    case "$i" in
      --debug)
        DEBUG_FLAG=-fdebug-game
      ;;
      --profile-graphics)
        DEBUG_FLAG=-fprofile-graphics
      ;;
      --profile)
        EXECUTABLE_FLAGS="+RTS -p -h -RTS"
        CONFIGURE_FLAGS="--enable-executable-profiling -fprofile"
      ;;
    esac
  done

fi

cabal configure $CONFIGURE_FLAGS $DEBUG_FLAG && \
  cabal build exe:Epidemic && \
  macosx-app dist/build/Epidemic/Epidemic && \
  Epidemic.app/Contents/MacOS/Epidemic $EXECUTABLE_FLAGS


cp assets/* Epidemic.app/Contents/Resources