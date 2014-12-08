#!/bin/bash

#
# sseefried: We are now using Stackage (http://www.stackage.org) to build Epidemic.
# In particular we are using this particular repo:
#
#    http://www.stackage.org/snapshot/2014-12-04-ghc78hp-inc
#

DIR=$HOME/code/forked-git/hsSDL2
[ -d $DIR ] && HS_SDL2_PATH=$DIR

DIR=$HOME/code/forked-git/hs-sdl2-mixer
[ -d $DIR ] && HS_SDL2_MIXER_PATH=$DIR

if [ "$HS_SDL2_PATH" = "" ]; then
  echo "Please set HS_SDL2_PATH to point to a checked out version of"
  echo "https://github.com/sseefried/hsSDL2"
  exit 1
fi

if [ "$HS_SDL2_MIXER_PATH" = "" ]; then
  echo "Please set HS_SDL2_PATH to point to a checked out version of"
  echo "https://github.com/sseefried/hs-sdl2-mixer"
  exit 1
fi

export PATH=$HOME/.cabal/bin:$PATH
cabal install alex happy
cabal install gtk2hs-buildtools
(cd $HS_SDL2_PATH && cabal install)
(cd $HS_SDL2_MIXER_PATH && cabal install)
cabal install  -j8 -fnosound
