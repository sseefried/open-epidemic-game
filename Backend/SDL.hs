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
import           Control.Concurrent
import           Control.Concurrent.MVar

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
                                 , besSemaphore    :: MVar () -- required because of event handling.
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
  semaphore <- newMVar ()
  newIORef $ BackendState t t surf csurf gs (screenWidth, screenHeight) 0 semaphore

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

runAtomic :: IORef BackendState -> String -> IO () -> IO ()
runAtomic besRef desc io = do
  mvar <- besSemaphore <$> readIORef besRef
  () <- takeMVar mvar
  io
  putMVar mvar ()

runFrameUpdate :: IORef BackendState -> (Time -> Time -> GameState -> GameM GameState) -> IO ()
runFrameUpdate besRef frameUpdate = runAtomic besRef "frameupdate" $ do
    bes <- readIORef besRef
    t <- getCurrentTime
    let duration   = toDouble $ diffUTCTime t (besLastTime bes)
        sinceStart = toDouble $ diffUTCTime t (besStartTime bes)
        (w,h)      = besDimensions bes
        logFrameRate = do
           let n = besFrames bes
               t = besStartTime bes
           when (n `mod` 30 == 29) $ do
             t' <- getCurrentTime
             let d = diffUTCTime t' t
             printf "Framerate = %.2f frames/s\n" (fromIntegral n / (toDouble d) :: Double)
             return ()
    -- draw a single frame
    runOnGameState besRef (frameUpdate duration sinceStart) $ \gs' -> do
    screen <- S.getVideoSurface
    C.renderWith (besCairoSurface bes) $ renderOnWhite w h $ gsRender gs' sinceStart
    _ <- S.blitSurface (besSurface bes) Nothing screen (Just (S.Rect 0 0 0 0))
    S.flip screen
    logFrameRate
    writeIORef besRef $ bes { besGameState = gs', besLastTime = t, besFrames = besFrames bes + 1 }
  where
    toDouble = fromRational . toRational

--
-- Runs [handleEvent] until an SDL "Quit" event is received. Otherwise loops forever.
--
runEventHandler :: IORef BackendState -> ([Event] -> GameState -> GameM GameState) -> ThreadId -> IO ()
runEventHandler besRef handleEvent frameUpdateId = do
  sdlEvent <- S.waitEvent
  case checkForQuit sdlEvent of
    True  -> do
      killThread frameUpdateId
      return () -- return
    False -> do
      bes <- readIORef besRef
      case sdlEventToEvent (gsFSMState . besGameState $ bes) sdlEvent of
        Just e -> do
          runAtomic besRef "handleEvent" $ runOnGameState' besRef $ handleEvent [e]
        Nothing -> return ()
      runEventHandler besRef handleEvent frameUpdateId -- loop again
  where
    quit = exitWith ExitSuccess
    checkForQuit e = case e of
        S.Quit                            -> True
        S.KeyDown (S.Keysym S.SDLK_q _ _) -> True
        _                                 -> False

mainLoop :: IORef BackendState
         -> ([Event] -> GameState -> GameM GameState) -- event handler
         -> (Time -> Time -> GameState -> GameM GameState) -- frame update
         -> IO ()
mainLoop besRef handleEvent frameUpdate = do
  frameUpdateId <- forkIO $ loop $ runFrameUpdate besRef frameUpdate -- run as fast as possible
  runEventHandler besRef handleEvent frameUpdateId
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io