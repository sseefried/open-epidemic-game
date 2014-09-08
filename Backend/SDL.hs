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
import           System.Exit
import           Data.Maybe (catMaybes)
import           Control.Applicative

-- friends
import Game
import Graphics

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
initialize title screenWidth screenHeight gs = do
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

debugPrintKey sdlEvent = case sdlEvent of
  S.KeyDown (S.Keysym key mods unicode) ->
    printf "Key: %s %s %s\n" (show key) (show mods) (show unicode)
  _ -> return ()


sdlEventToEvent :: FSMState -> S.Event -> Maybe Event
sdlEventToEvent fsmState sdlEvent =
  -- events that can occur in any FSM State
  case sdlEvent of
    S.KeyDown _ -> Just Reset
    _           -> (case fsmState of -- events that occur in specific FSM states
                      _ -> Nothing)

--
-- Reads the current backend state, runs [f], writes the backend state back with modified GameState,
-- and then runs the continuation [cont] with the latest GameState
--
runOnGameState :: IORef BackendState -> (GameState -> GameM GameState) -> (GameState -> IO ()) -> IO ()
runOnGameState besRef f cont = do
  bes <- readIORef besRef
  gs <- runGameM . f $ besGameState bes
  writeIORef besRef $ bes { besGameState = gs}
  cont gs

-- Like [runOnGameState] but without the continuation
runOnGameState' :: IORef BackendState -> (GameState -> GameM GameState) -> IO ()
runOnGameState' besRef f = runOnGameState besRef f (const $ return ())

mainLoop :: IORef BackendState
         -> ([Event] -> GameState -> GameM GameState) -- event handler
         -> (Time -> Time -> GameState -> GameM GameState) -- frame update
         -> IO ()
mainLoop besRef handleEvent frameUpdate = do
  t <- getCurrentTime
  bes <- readIORef besRef
  let duration   = toDouble $ diffUTCTime t (besLastTime bes)
      sinceStart = toDouble $ diffUTCTime t (besStartTime bes)
      (w,h)      = besDimensions bes
  let logFrameRate = do
         let n = besFrames bes
             t = besStartTime bes
         when (n `mod` 30 == 29) $ do
           t' <- getCurrentTime
           let d = diffUTCTime t' t
           printf "Framerate = %.2f frames/s\n" (fromIntegral n / (toDouble d) :: Double)
           return ()
  checkForQuit
  events <- catMaybes . map (sdlEventToEvent . gsFSMState . besGameState $ bes) <$> getSDLEvents
  runOnGameState' besRef (handleEvent events) -- FIXME: Put in separate thread
  -- draw a single frame
  runOnGameState besRef (frameUpdate duration sinceStart) $ \gs' -> do
  screen <- S.getVideoSurface
  C.renderWith (besCairoSurface bes) $ renderOnWhite w h $ gsRender gs' sinceStart
  _ <- S.blitSurface (besSurface bes) Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  logFrameRate
  writeIORef besRef $ bes { besGameState = gs', besLastTime = t, besFrames = besFrames bes + 1 }
  mainLoop besRef handleEvent frameUpdate -- continue playing
  where
    toDouble = fromRational . toRational
    checkForQuit = do
      e <- S.pollEvent
      let quit = exitWith ExitSuccess
      case e of
        S.Quit                            -> quit
        S.KeyDown (S.Keysym S.SDLK_q _ _) -> quit
        _                                 -> return ()
      S.pushEvent e -- push the event back on the queue

    -- polls repeatedly until NoEvent is received and returns a list of events
    getSDLEvents :: IO [S.Event]
    getSDLEvents = do
      e <- S.pollEvent
      case e of
        S.NoEvent -> return []
        _         -> do es <- getSDLEvents
                        return $ e:es