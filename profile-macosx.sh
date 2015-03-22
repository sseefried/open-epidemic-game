#!/bin/bash

cabal configure  && \
  cabal build exe:ProfileGraphics && \
  macosx-app dist/build/ProfileGraphics/ProfileGraphics && \
  open ProfileGraphics.app

cp assets/* ProfileGraphics.app/Contents/Resources