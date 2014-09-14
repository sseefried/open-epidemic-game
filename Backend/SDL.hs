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


toDouble :: Real a => a -> Double
toDouble = fromRational . toRational

----------------------------------------------------------------------------------------------------
runFrameUpdate :: IORef BackendState -> IO ()
runFrameUpdate besRef = do
  bes <- readIORef besRef
  t <- getCurrentTime
  let (w,h)      = besDimensions bes
      gs         = besGameState bes
      sinceStart = toDouble $ diffUTCTime t (besStartTime bes)
  screen <- S.getVideoSurface
  C.renderWith (besCairoSurface bes) $ gsRender gs
  _ <- S.blitSurface (besSurface bes) Nothing screen (Just (S.Rect 0 0 0 0))
  S.flip screen
  writeIORef besRef $ bes { besFrames = besFrames bes + 1 }

----------------------------------------------------------------------------------------------------
--
-- [runInputEventHandler] tries to retrieve a game event. If it finds one then
-- [handleEvent] is called on this event and the FSMState in the BackEndState is updated.
--
runInputEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runInputEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  let gs       = besGameState bes
      fsmState = besFSMState bes
  mbEvent <- getEvent fsmState
  case mbEvent of
    Left ()         -> exitWith ExitSuccess
    Right Nothing   -> return () -- do nothing
    Right (Just ev) -> runOnGameState' besRef (handleEvent fsmState ev)
  where
    runOnGameState' b c = runOnGameState updFSMState b c (const $ return ())
    updFSMState fsmState bes = bes { besFSMState = fsmState }

----------------------------------------------------------------------------------------------------
runPhysicsEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runPhysicsEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  t <- getCurrentTime
  let gs = besGameState bes
      duration = toDouble $ diffUTCTime t (besLastTime bes)
      fsmState = besFSMState bes
  printf "%.2f\n" duration
  (fsmState', gs') <- runGameM (handleEvent fsmState (Physics duration)) gs
  writeIORef besRef $ bes { besGameState = gs', besLastTime = t, besFSMState = fsmState' }

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
         -> IO ()
mainLoop besRef handleEvent = loop $ do
  runFrameUpdate       besRef
  runInputEventHandler besRef handleEvent
  runPhysicsEventHandler besRef handleEvent
  logFrameRate besRef
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io
--    let duration   = toDouble $ diffUTCTime t (besLastTime bes)
--        sinceStart = toDouble $ diffUTCTime t (besStartTime bes)


logFrameRate :: IORef BackendState -> IO ()
logFrameRate besRef = do
      bes <- readIORef besRef
      let n = besFrames bes
          t = besStartTime bes
      when (n `mod` 30 == 29) $ do
        t' <- getCurrentTime
        let d = toDouble $ diffUTCTime t' t
        printf "Framerate = %.2f frames/s\n" (fromIntegral n / d :: Double)
        return ()

