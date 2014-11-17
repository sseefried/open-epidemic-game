{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android build
--
module MainLib where

-- friends
import Game
import Backend.SDL as B

screenWidth, screenHeight :: Int
screenWidth  = 640
screenHeight = 360

w, h :: Double
w = fromIntegral screenWidth
h = fromIntegral screenHeight

foreign export ccall "haskell_main" main :: IO ()

main :: IO ()
main = do
  gs     <- newGameState (screenWidth, screenHeight)
  besRef <- B.initialize "Epidemic" screenWidth screenHeight gs
  B.mainLoop besRef handleEvent