{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main where


-- friends
import Game
import Backend.SDL as B

screenWidth, screenHeight :: Int
screenWidth  = 800
screenHeight = 800

w, h :: Double
w = fromIntegral screenWidth
h = fromIntegral screenHeight

main :: IO ()
main = do
  gs     <- newGameState (screenWidth, screenHeight)
  besRef <- B.initialize "Epidemic" screenWidth screenHeight gs
  B.mainLoop besRef handleEvent