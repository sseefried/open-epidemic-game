module Main where

--
-- This module just profiles graphics. I hope to answer the question of whether I should
-- use one big shader or smaller shaders, as well as have a benchmark to full back to
-- any time I change the shader code.
--
-- I also hope to test this on several bits of hardware, as graphics cards vary widely.
--

import           Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import           Data.IORef
import           Control.Monad.Random
import           Control.Monad
import           Data.Bits

-- friends
import Types
import Game.Types
import GLM
import Graphics
import Platform
import CUtil
import GraphicsGL
import Util

----------------------------------------------------------------------------------------------------
data S = S { sWindow :: S.Window
           , sGfxState :: GfxState
           , sGerms :: GLMW [ GermGL ]
           , sGLContext :: S.GLContext  }

type GLMW a = GLM WorldGLSL a

nGerms = 50

----------------------------------------------------------------------------------------------------
initialize :: IO (IORef S)
initialize = do
  S.init [S.InitVideo]
  (w,h) <- case screenDimensions of
    Just (w',h') -> return (w', h')
    Nothing -> do
      mode <- S.getCurrentDisplayMode 0
      return (fromIntegral (S.displayModeWidth mode), fromIntegral (S.displayModeHeight mode))
  when (w < h) $ exitWithError $
    printf "Width of screen (%d) must be greater than or equal to height (%d)" w h
  window <- S.createWindow "Profile Graphics" (S.Position 0 0) (S.Size w h) [S.WindowShown]

  let glAttrs = case True of
                _ | platform `elem` [Android, IOSPlatform] ->
                       [ (S.GLDepthSize,           24)
                       , (S.GLContextProfileMask,  S.glContextProfileES)
                       , (S.GLContextMajorVersion, 2) ]
                _ -> [ (S.GLDepthSize,           24) ]
  mapM_ (uncurry S.glSetAttribute) glAttrs
  context <- S.glCreateContext window

--  when (platform == MacOSX) $ S.glSetSwapInterval S.ImmediateUpdates

  resourcePath <- iOSResourcePath
  gfxState <- initGfxState (w,h) resourcePath

--  glUseProgram (worldGLSLProgramId $ gfxWorldGLSL gfxState)


  let germs = replicateM 50 newGermGLM
  newIORef $ S { sWindow    = window
               , sGfxState  = gfxState
               , sGerms     = germs
               , sGLContext = context
               }

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  sRef <- initialize
  mainLoop sRef
  where


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
      ---
      let gfxs = sGfxState s
      germGLs <- runGLMIO gfxs $ sGerms s
      let draw :: GermGL -> IO ()
          draw germGL = runGLMIO gfxs $ do
            germGLFun germGL 0 (R2 0 0) 0 30 1
      glBindFramebuffer gl_FRAMEBUFFER $ gfxScreenFBId gfxs
      glClearColor 1 1 1 1 -- here it must be opaque
      glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)
      sequence $ map draw germGLs
      glFlush
      S.glSwapWindow (sWindow s)
      ---
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

----------------------------------------------------------------------------------------------------
newGermGLM :: GLMW GermGL
newGermGLM = join foo
  where
    foo :: GLMW (GLMW GermGL)
    foo = do
      germGfx <- liftGLM $ evalRandIO randomGermGfx
      return $ germGfxToGermGL germGfx