{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
module MainSDL where

import           Graphics.Rasterific
import           Graphics.Rasterific.Transformations
import           Control.Monad.Random
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr
import           Codec.Picture
import           Data.Time
import           System.Exit
import qualified Graphics.UI.SDL as S
import           Data.IORef
import           Data.Word (Word8)
import           Control.Monad
import           Text.Printf

-- friends
import Game

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()

data LoopState = LoopState { lsStartTime :: UTCTime, lsFrames :: Integer }

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
  newIORef $ LoopState t 0

mainSDL :: IO ()
mainSDL = S.withInit [S.InitVideo] $ do
  let r = fromIntegral (min screenWidth screenHeight) / 2
  _ <- S.setVideoMode screenWidth screenHeight bitsPerPixel
              [S.HWSurface, S.DoubleBuf]
  S.setCaption "Epidemic" ""
  S.enableUnicode True
  sRef <- initLoopState
  germ <- evalRandIO $ randomGerm r
  let draw = display sRef germ
  loop sRef draw

germToImage :: Germ -> Image PixelRGBA8
germToImage g = do
  let white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
      w     = screenWidth
      h     = screenHeight
  renderDrawing w h white $
    withTransformation (translate (V2 (fromIntegral w/2) (fromIntegral h/2))) $
    drawGerm g


imagePtr :: Image PixelRGBA8 -> ForeignPtr Word8
imagePtr = fst . V.unsafeToForeignPtr0 . imageData

imageToSurface :: Image PixelRGBA8 -> IO S.Surface
imageToSurface image = do
  withForeignPtr (imagePtr image) $ \ptr -> S.createRGBSurfaceFrom
                    ptr
                    screenWidth
                    screenHeight
                    bitsPerPixel
                    (screenWidth * bytesPerPixel)
                    0x000000FF
                    0x0000FF00
                    0x00FF0000
                    0xFF000000

loop :: IORef LoopState -> IO () -> IO ()
loop sRef display' = do
  let logFrameRate = do
         s <- readIORef sRef
         let n = lsFrames s
             t = lsStartTime s
         when (n `mod` 50 == 49) $ do
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


display :: IORef LoopState -> Germ -> IO ()
display sRef germ = do
  screen <- S.getVideoSurface
  surface <- imageToSurface . germToImage $ germ
  _ <- S.blitSurface surface Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  modifyIORef sRef $ \s -> s { lsFrames = lsFrames s + 1}