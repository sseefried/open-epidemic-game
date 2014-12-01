#!/bin/bash

export PATH=$HOME/.cabal/bin:$PATH
rm -f cabal.sandbox.config
rm -rf .cabal-sandbox
cabal sandbox init
cabal install alex happy
cabal install gtk2hs-buildtools
cabal sandbox add-source $HOME/code/forked-git/hs-sdl2-mixer
cabal sandbox add-source $HOME/code/forked-git/hsSDL2
cabal install  -j8 -fnosound
