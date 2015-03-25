{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main where


-- friends
import Game
import Backend.SDL as B
import CUtil
import ProfileGraphics

main :: IO ()
main = if profilingGraphics then profileGraphics Nothing else gameMain

gameMain ::  IO ()
gameMain = do
  besRef <- B.initialize "Epidemic" Nothing
  B.mainLoop besRef handleEvent