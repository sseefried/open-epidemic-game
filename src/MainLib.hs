{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module MainLib where

-- friends
import Game
import Backend.SDL as B

foreign export ccall "haskell_main" main :: IO ()

main :: IO ()
main = do
  besRef <- B.initialize "Epidemic"
  B.mainLoop besRef handleEvent