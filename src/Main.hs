{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main where


-- friends
import Game
import Backend.SDL as B

main :: IO ()
main = do
  besRef <- B.initialize "Epidemic" Nothing
  B.mainLoop besRef handleEvent