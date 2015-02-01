{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main where


-- friends
import Game
import Backend.SDL as B
import CUtil

main :: IO ()
main = do
  s <- resourcePath
  putStrLn s
  besRef <- B.initialize "Epidemic"
  B.mainLoop besRef handleEvent