#!/bin/bash

rm -f cabal.sandbox.config
rm -rf .cabal-sandbox
cabal sandbox init
cabal intall gtk2hs-buildtools
cabal sandbox add-source $HOME/code/forked-git/hs-sdl2-mixer
cabal sandbox add-source $HOME/code/forked-git/hsSDL2
cabal install -p --enable-executable-profiling -j8
