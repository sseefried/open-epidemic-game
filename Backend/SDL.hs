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

----------------------------------------------------------------------------------------------------
data BackendState = BackendState { besStartTime    :: UTCTime
                                 , besLastTime     :: UTCTime
                                 , besSurface      :: S.Surface
                                 , besCairoSurface :: C.Surface
                                 , besGameState    :: GameState
                                 , besDimensions   :: (Int,Int)
                                 , besFrames       :: Integer
                                 , besFSMState     :: FSMState
                                 }

----------------------------------------------------------------------------------------------------
bitsPerPixel, bytesPerPixel :: Int
bitsPerPixel  = 32
bytesPerPixel = bitsPerPixel `div` 8

----------------------------------------------------------------------------------------------------
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
  newIORef $ BackendState t t surf csurf gs (screenWidth, screenHeight) 0 (FSMLevel 1)

debugPrintKey sdlEvent = case sdlEvent of
  S.KeyDown (S.Keysym key mods unicode) ->
    printf "Key: %s %s %s\n" (show key) (show mods) (show unicode)
  _ -> return ()

----------------------------------------------------------------------------------------------------
--
-- Returns Nothing if the SDL event is not understood by the game in this FSMState.
--
sdlEventToEvent :: FSMState -> S.Event -> Maybe Event
sdlEventToEvent fsmState sdlEvent =
  -- events that can occur in any FSM State
  case sdlEvent of
    S.KeyDown _ -> Just Reset
    _           -> (case fsmState of -- events that occur in specific FSM states
                      _ -> Nothing)

----------------------------------------------------------------------------------------------------
--
-- Reads the current backend state, runs [f], writes the backend state back with modified GameState,
-- and then runs the continuation [cont] with the latest GameState
--
runOnGameState :: (a -> BackendState -> BackendState)
               -> IORef BackendState
               -> GameM a
               -> (GameState -> IO ())
               -> IO ()
runOnGameState upd besRef gameM cont  = do
  bes     <- readIORef besRef
  (a, gs) <- runGameM gameM (besGameState bes)
  writeIORef besRef $ upd a $ bes { besGameState = gs }
  cont gs

----------------------------------------------------------------------------------------------------
--
-- Executes an IO action, times it and then updates the BackendState with that duration.
runAndTime :: IORef BackendState -> (Time -> BackendState -> BackendState) -> IO a -> IO a
runAndTime besRef upd io = do
  bes <- readIORef besRef
  t <- getCurrentTime
  result <- io
  t' <- getCurrentTime
  writeIORef besRef $ upd (toDouble $ diffUTCTime t' t) bes
  return result
  where
    toDouble = fromRational . toRational

----------------------------------------------------------------------------------------------------
runFrameUpdate :: IORef BackendState -> (Time -> Time -> GameM ()) -> IO ()
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
    runOnGameState (const id) besRef (frameUpdate duration sinceStart) $ \gs' -> do
    screen <- S.getVideoSurface
    C.renderWith (besCairoSurface bes) $ renderOnWhite w h $ gsRender gs' sinceStart
    _ <- S.blitSurface (besSurface bes) Nothing screen (Just (S.Rect 0 0 0 0))
    S.flip screen
    logFrameRate
    writeIORef besRef $ bes { besGameState = gs', besLastTime = t, besFrames = besFrames bes + 1 }
  where
    toDouble = fromRational . toRational

----------------------------------------------------------------------------------------------------
--
-- Runs [handleEvent] until an SDL "Quit" event is received. Otherwise loops forever.
--
runEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  let gs       = besGameState bes
      fsmState = besFSMState bes
  mbEvent <- getEvent fsmState
  case mbEvent of
    Left ()         -> exitWith ExitSuccess
    Right Nothing   -> return () -- do nothing
    Right (Just ev) -> runOnGameState1 besRef (handleEvent fsmState ev)
  where
    runOnGameState1 b c = runOnGameState (\fsmState bes -> bes { besFSMState = fsmState }) b c (const $ return ())

----------------------------------------------------------------------------------------------------
--
-- Repeatedly polls until an SDL [NoEvent] is received and returns all events received
-- (except for [NoEvent]). Can return an empty list.
--
--getSDLEvents :: IO [S.Event]
--getSDLEvents = do
--  e <- S.pollEvent
--  case e of
--    S.NoEvent -> return []
--    _         -> do { es <- getSDLEvents; return (e:es) }

----------------------------------------------------------------------------------------------------
--
-- We want to return *at most* one event per frame (as we want the finite state machine to
-- evolve by one state at most per frame). We want to skip any events that the game does
-- not understand (so that we do not have frames where nothing happened because
-- an SDL event that was not understood was processed into returning Nothing)
--
-- Left ()        = SDL quit event occurred
-- Right Nothing  = no game event
-- Right ev       = event [ev] returned
getEvent :: FSMState -> IO (Either () (Maybe Event))
getEvent fsmState = do
  sdlEvent <- S.pollEvent
  case checkForQuit sdlEvent of
    True -> return $ Left ()
    False -> (case sdlEvent of
                S.NoEvent -> return $ Right Nothing
                _ -> (case sdlEventToEvent fsmState sdlEvent of
                        Nothing -> getEvent fsmState -- keep polling
                        Just ev -> return $ Right $ Just ev))
  where
    -- Checks for a Quit event (caused by closing the window) or whether the Q key is pressed
    checkForQuit e = case e of
      S.Quit                            -> True
      S.KeyDown (S.Keysym S.SDLK_q _ _) -> True
      _                                 -> False
----------------------------------------------------------------------------------------------------
mainLoop :: IORef BackendState
         -> (FSMState -> Event -> GameM FSMState) -- event handler
         -> (Time -> Time -> GameM ()) -- frame update
         -> IO ()
mainLoop besRef handleEvent frameUpdate = loop $ do
  runEventHandler besRef handleEvent
  runFrameUpdate  besRef frameUpdate
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io

