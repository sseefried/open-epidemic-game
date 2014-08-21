{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
module MainSDL where

import           Control.Monad.Random
import           Data.Time
import           System.Exit
import           Graphics.Rendering.Cairo as C
import qualified Graphics.UI.SDL as S
import           Data.IORef
import           Control.Monad
import           Text.Printf
import           Foreign.Ptr
import           Control.Applicative

-- friends
import Game

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()

data LoopState = LoopState { lsStartTime   :: UTCTime
                           , lsFrames      :: Integer
                           , lsSurface     :: S.Surface
                           , lsCanvas      :: C.Surface
                           , lsGermAnim    :: Time -> C.Render ()
                           }

screenWidth, screenHeight :: Int
screenWidth = 512
screenHeight = 512
tileGermsPerRow = 10

w, h :: Double
w = fromIntegral screenWidth
h = fromIntegral screenHeight

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
  surface <- S.createRGBSurface [S.HWSurface] screenWidth screenHeight bitsPerPixel
                                         0x00FF0000 0x0000FF00 0x000000FF 0
  pixels <- fmap castPtr $ S.surfaceGetPixels surface
  canvas <- createImageSurfaceForData pixels FormatRGB24 screenWidth screenHeight
             (screenWidth * bytesPerPixel)
  germAnim <- newGermAnim
  newIORef $ LoopState t 0 surface canvas germAnim

newSingleGermAnim :: IO (Time -> Render ())
newSingleGermAnim = do
  g <- evalRandIO $ randomGerm (fromIntegral (min screenWidth screenHeight) / 2)
  return $ \t -> do
    translate (w/2) (h/2)
    drawGerm g t

-- newGermAnim = newSingleGermAnim
newGermAnim = evalRandIO $ tiledGerms tileGermsPerRow screenWidth screenHeight

renderOnWhite :: Render () -> Render ()
renderOnWhite drawing = do
  setAntialias AntialiasSubpixel
  drawBackground
  asGroup drawing
  where
    drawBackground = do
      setColor white
      rectangle 0 0 w h
      fill

mainSDL :: IO ()
mainSDL = S.withInit [S.InitVideo] $ do
  _ <- S.setVideoMode screenWidth screenHeight bitsPerPixel
              [S.HWSurface, S.DoubleBuf]
  S.setCaption "Epidemic" ""
  S.enableUnicode True
  sRef <- initLoopState
  let draw = display sRef
  loop sRef draw

toDouble :: NominalDiffTime -> Double
toDouble = fromRational . toRational

loop :: IORef LoopState -> IO () -> IO ()
loop sRef display' = do
  s <- readIORef sRef
  let logFrameRate = do
         let n = lsFrames s
             t = lsStartTime s
         when (n `mod` 30 == 29) $ do
           t' <- getCurrentTime
           let d = diffUTCTime t' t
           printf "Framerate = %.2f frames/s\n" (fromIntegral n / (toDouble d) :: Double)
  event <- S.pollEvent
  case event of
    S.Quit -> exitWith ExitSuccess
    S.KeyDown (S.Keysym _ _ 'q') -> exitWith ExitSuccess
    S.KeyDown _ -> do
      germAnim <- newGermAnim
      writeIORef sRef $ s { lsGermAnim = germAnim}

    _ -> display'
  logFrameRate
  loop sRef display'

display :: IORef LoopState -> IO ()
display sRef = do
  let w = fromIntegral screenWidth
      h = fromIntegral screenHeight
  s <- readIORef sRef
  screen <- S.getVideoSurface
  -- Cairo
  t <- diffUTCTime <$> getCurrentTime <*> return (lsStartTime s)
  renderWith (lsCanvas s) $ renderOnWhite $ lsGermAnim s . toDouble $ t
  _ <- S.blitSurface (lsSurface s) Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  writeIORef sRef $ s { lsFrames = lsFrames s + 1}