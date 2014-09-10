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

runFrameUpdate :: IORef BackendState -> (Time -> Time -> GameState -> GameM GameState) -> IO ()
runFrameUpdate besRef frameUpdate = do
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
runEventHandler :: IORef BackendState -> ([Event] -> GameState -> GameM GameState) -> IO ()
runEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  let gs       = besGameState bes
      fsmState = gsFSMState gs
  mbEvents <- getEvents fsmState
  case mbEvents of
    Just []       -> return () -- do nothing
    Just es@(_:_) -> runOnGameState' besRef (handleEvent es)
    Nothing       -> exitWith ExitSuccess -- quit the game

--
-- Repeatedly polls until an SDL [NoEvent] is received and returns all events received
-- (except for [NoEvent]). Can return an empty list.
--
getSDLEvents :: IO [S.Event]
getSDLEvents = do
  e <- S.pollEvent
  case e of
    S.NoEvent -> return []
    _         -> do { es <- getSDLEvents; return (e:es) }

--
-- Given an FSMState [getEvents] returns [Nothing] if a quit event was received
-- or it returns a (possibly empty) list of game events.
--
getEvents :: FSMState -> IO (Maybe [Event])
getEvents fsmState = do
  sdlEvents <- getSDLEvents
  return $ case any checkForQuit sdlEvents of
    True  -> Nothing
    False -> Just . catMaybes . map (sdlEventToEvent fsmState) $ sdlEvents
  where
    -- Checks for a Quit event (caused by closing the window) or whether the Q key is pressed
    checkForQuit e = case e of
      S.Quit                            -> True
      S.KeyDown (S.Keysym S.SDLK_q _ _) -> True
      _                                 -> False

mainLoop :: IORef BackendState
         -> ([Event] -> GameState -> GameM GameState) -- event handler
         -> (Time -> Time -> GameState -> GameM GameState) -- frame update
         -> IO ()
mainLoop besRef handleEvent frameUpdate = loop $ do
  runEventHandler besRef handleEvent
  runFrameUpdate  besRef frameUpdate
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io