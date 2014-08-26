module Backend.SDL (
  initialize,
  mainLoop
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.Rendering.Cairo as C
import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           Foreign.Ptr

-- friends
import Game

{-

All backends must render from C.Render () to the backend's screen somehow.

-}

data BackendState = BackendState { besStartTime    :: UTCTime
                                 , besLastTime     :: UTCTime
                                 , besSurface      :: S.Surface
                                 , besCairoSurface :: C.Surface
                                 , besGameState    :: GameState
                                 , besDimensions   :: (Int,Int)
                                 , besFrames       :: Integer
                                 }


bitsPerPixel, bytesPerPixel :: Int
bitsPerPixel  = 32
bytesPerPixel = bitsPerPixel `div` 8

initialize :: String -> Int -> Int -> GameState -> IO (IORef BackendState)
initialize title screenWidth screenHeight gs = S.withInit [S.InitVideo] $ do
  _ <- S.setVideoMode screenWidth screenHeight bitsPerPixel
              [S.HWSurface, S.DoubleBuf]
  S.setCaption title ""
  S.enableUnicode True
  t    <- getCurrentTime
  surf <- S.createRGBSurface [S.HWSurface] screenWidth screenHeight bitsPerPixel
                             0x00FF0000 0x0000FF00 0x000000FF 0
  pixels <- fmap castPtr $ S.surfaceGetPixels surf
  csurf  <- C.createImageSurfaceForData pixels C.FormatRGB24 screenWidth screenHeight
             (screenWidth * bytesPerPixel)
  newIORef $ BackendState t t surf csurf gs (screenWidth, screenHeight) 0

mainLoop :: IORef BackendState -> (GameInput -> GameState -> GameState) -> IO ()
mainLoop besRef gameFun = do
  bes <- readIORef besRef
  let logFrameRate = do
         let n = besFrames bes
             t = besStartTime bes
         when (n `mod` 30 == 29) $ do
           t' <- getCurrentTime
           let d = diffUTCTime t' t
           printf "Framerate = %.2f frames/s\n" (fromIntegral n / (toDouble d) :: Double)
  sdlEvent <- S.pollEvent
  t <- getCurrentTime
  let event = case sdlEvent of
                S.Quit                       -> Quit
                S.KeyDown (S.Keysym _ _ 'q') -> Quit
                S.KeyDown _                  -> KeyDown 666 -- FIXME
                S.KeyUp   _                  -> KeyUp 666 -- FIXME
      duration = toDouble $ diffUTCTime t (besStartTime bes)
      sinceStart = toDouble $ diffUTCTime t (besLastTime bes)
  -- draw a single frame
      gs' =  gameFun (GameInput duration sinceStart [event]) (besGameState bes)
      (w,h) = besDimensions bes
  screen <- S.getVideoSurface
  C.renderWith (besCairoSurface bes) $ renderOnWhite w h $ gsRender gs'
  _ <- S.blitSurface (besSurface bes) Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  logFrameRate
  writeIORef besRef $ bes { besGameState = gs', besLastTime = t }
  mainLoop besRef gameFun
  where
    toDouble = fromRational . toRational