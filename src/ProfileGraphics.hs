module Main where

--
-- This module just profiles graphics. I hope to answer the question of whether I should
-- use one big shader or smaller shaders, as well as have a benchmark to full back to
-- any time I change the shader code.
--
-- I also hope to test this on several bits of hardware, as graphics cards vary widely.
--

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import           Data.IORef

-- friends
import Platform
import GraphicsGL
import Util

----------------------------------------------------------------------------------------------------
data S = S { sWindow :: S.Window }

----------------------------------------------------------------------------------------------------
initialize :: IO (IORef S)
initialize = do
  S.init [S.InitVideo, S.InitAudio]
  (w,h) <- case screenDimensions of
    Just (w',h') -> return (w', h')
    Nothing -> do
      mode <- S.getCurrentDisplayMode 0
      return (fromIntegral (S.displayModeWidth mode), fromIntegral (S.displayModeHeight mode))
  when (w < h) $ exitWithError $
    printf "Width of screen (%d) must be greater than or equal to height (%d)" w h
  window  <- S.createWindow "Profile Graphics" (S.Position 0 0) (S.Size w h) [S.WindowShown]
  newIORef $ S window

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  sRef <- initialize
  mainLoop sRef

----------------------------------------------------------------------------------------------------
mainLoop :: IORef S -> IO ()
mainLoop sRef = go
  where
    go :: IO ()
    go = do
      s <- readIORef sRef
      mbE <- S.pollEvent
      let quit = maybe False (checkForQuit . S.eventData) mbE
      when quit $ exitWith ExitSuccess
      go



----------------------------------------------------------------------------------------------------
checkForQuit :: S.EventData -> Bool
checkForQuit ed = case ed of
      S.Quit                    -> True
      _ | b <- isKeyDown ed SK.Q -> b

----------------------------------------------------------------------------------------------------
isKeyDown :: S.EventData -> SK.Keycode -> Bool
isKeyDown ed code = case ed of
  S.Keyboard {  S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ code' _ } -> code == code'
  _ -> False

