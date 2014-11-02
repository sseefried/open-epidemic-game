#!/bin/bash

rm -f cabal.sandbox.config
rm -rf .cabal-sandbox
cabal sandbox init
cabal sandbox add-source /Users/sseefried/code/forked-git/hs-sdl2-mixer
cabal sandbox add-source /Users/sseefried/code/forked-git/hsSDL2
cabal install -j8
