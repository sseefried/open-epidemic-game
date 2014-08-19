{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
module MainSDL where

import           Control.Monad.Random
import           Data.Time
import           System.Exit
import           Graphics.Rendering.Cairo as C
import qualified Graphics.UI.SDL as S
import           Data.IORef
import           Data.Word (Word8)
import           Control.Monad
import           Text.Printf
import           Foreign.Ptr

-- friends
import Game

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()

data LoopState = LoopState { lsStartTime :: UTCTime, lsFrames :: Integer
                           , lsSurface :: S.Surface, lsCanvas :: C.Surface }

screenWidth, screenHeight :: Int
screenWidth = 512
screenHeight = 512

--
-- You need to do a few things to write a Haskell SDL application
-- 1. Add the LANGUAGE ForeignFunctionInterface pragma
-- 2. foreign export ccall "haskell_mainSDL" <your_main> :: IO ()
-- 3. Compile with GHC with "-main-is" option.
-- 4. Link against mainc.o
--

bitsPerPixel, bytesPerPixel :: Int
bitsPerPixel  = 32
bytesPerPixel = bitsPerPixel `div` 8

initLoopState :: IO (IORef LoopState)
initLoopState = do
  t <- getCurrentTime
  --
  -- Getting the RGB masks correct and also setting the alpha mask to 0
  -- is important. Look into the Cairo SDL backend if you want to do alpha.
  --
  surface <- S.createRGBSurface [S.SWSurface] screenWidth screenHeight bitsPerPixel
                                         0x00FF0000 0x0000FF00 0x000000FF 0
  pixels <- fmap castPtr $ S.surfaceGetPixels surface
  canvas <- createImageSurfaceForData pixels FormatRGB24 screenWidth screenHeight
             (screenWidth * bytesPerPixel)

  newIORef $ LoopState t 0 surface canvas

mainSDL :: IO ()
mainSDL = S.withInit [S.InitVideo] $ do
  let r = fromIntegral (min screenWidth screenHeight) / 2
  _ <- S.setVideoMode screenWidth screenHeight bitsPerPixel
              [S.HWSurface, S.DoubleBuf]
  S.setCaption "Epidemic" ""
  S.enableUnicode True
  sRef <- initLoopState
  germ <- evalRandIO $ randomGerm r
  let draw = display sRef (drawGerm germ)
  loop sRef draw

loop :: IORef LoopState -> IO () -> IO ()
loop sRef display' = do
  let logFrameRate = do
         s <- readIORef sRef
         let n = lsFrames s
             t = lsStartTime s
         when (n `mod` 30 == 29) $ do
           t' <- getCurrentTime
           let d = diffUTCTime t' t
           printf "Framerate = %.2f frames/s\n" (fromIntegral n / ((fromRational . toRational) d) :: Float)
  event <- S.pollEvent
  case event of
    S.Quit -> exitWith ExitSuccess
    S.KeyDown (S.Keysym _ _ 'q') -> exitWith ExitSuccess
    _ -> display'
  logFrameRate
  loop sRef display'


display :: IORef LoopState -> Render () -> IO ()
display sRef render = do
  let w = fromIntegral screenWidth
      h = fromIntegral screenHeight
  s <- readIORef sRef
  screen <- S.getVideoSurface
  let format = S.surfaceGetPixelFormat screen
  white <- S.mapRGB format 0xFF 0xFF 0xFF
  S.fillRect screen Nothing white
  -- Cairo
  renderWith (lsCanvas s) $ renderCenter (w/2) (h/2) render
  _ <- S.blitSurface (lsSurface s) Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  writeIORef sRef $ s { lsFrames = lsFrames s + 1}