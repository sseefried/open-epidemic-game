{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module MainLib where

import System.Environment

-- friends
import Game
import Backend.SDL as B

foreign export ccall "haskell_main" main :: IO ()

main :: IO ()
main = do
  args <- getArgs
  let mbResourcePath = case args of
                       p:_ -> Just p
                       []  -> Nothing
  besRef <- B.initialize "Epidemic" mbResourcePath
  B.mainLoop besRef handleEvent