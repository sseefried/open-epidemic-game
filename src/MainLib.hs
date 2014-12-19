{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module MainLib where

-- friends
import Game
import Backend.SDL as B
import Platform

screenWidth, screenHeight :: Int
(screenWidth, screenHeight) = case platform of
  Android -> (1280,720)
  IOSPlatform -> (640, 960)
  _ -> (1280,720)

w, h :: Double
w = fromIntegral screenWidth
h = fromIntegral screenHeight

foreign export ccall "haskell_main" main :: IO ()

main :: IO ()
main = do
  gs     <- newGameState (screenWidth, screenHeight)
  besRef <- B.initialize "Epidemic" screenWidth screenHeight gs
  B.mainLoop besRef handleEvent