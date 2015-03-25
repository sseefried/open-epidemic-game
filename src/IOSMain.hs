{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module IOSMain where

-- friends
import Game
import Backend.SDL as B
import CUtil
import ProfileGraphics

foreign export ccall "haskell_main" main :: IO ()

main :: IO ()
main = if profilingGraphics then profileGraphics Nothing else gameMain

gameMain ::  IO ()
gameMain = do
  besRef <- B.initialize "Epidemic" Nothing
  B.mainLoop besRef handleEvent

