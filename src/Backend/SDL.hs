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
import           GHC.Int
import           Foreign.C.String (withCAString)
import           Foreign.Ptr
import           Foreign.Storable (peek)
import           Foreign.Marshal (alloca)
import           System.Exit
import           Data.Maybe (catMaybes)
import           Control.Applicative
import           GHC.Word
import           System.Endian (fromBE32)
import           Data.Bits

-- friends
import Types
import Game
import Graphics

{-
All backends must render from C.Render () to the backend's screen somehow.
-}

----------------------------------------------------------------------------------------------------
data BackendState = BackendState { besStartTime      :: UTCTime
                                 , besLastTime       :: UTCTime
                                 , besSDLRenderer    :: S.Renderer
                                 , besGameState      :: GameState
                                 , besDimensions     :: (Int,Int)
                                 , besBackendToWorld :: BackendToWorld
                                 , besFrames         :: Integer
                                 , besFSMState       :: FSMState
                                 }

data BackendToWorld = BackendToWorld { backendPtToWorldPt :: (Int32, Int32) -> R2 }

backendToWorld ::  (Int, Int) -> BackendToWorld
backendToWorld (w,h) =
  BackendToWorld { backendPtToWorldPt = \(x,y) -> R2 ((fromIntegral x - w'/2)  * scale)
                                                      ((h'/2 - fromIntegral y) * scale) }
  where
    minor = min w' h'
    scale = worldMajor / minor
    w' = fromIntegral w
    h' = fromIntegral h


----------------------------------------------------------------------------------------------------
bitsPerPixel, bytesPerPixel :: Int
bitsPerPixel  = 32
bytesPerPixel = bitsPerPixel `div` 8

----------------------------------------------------------------------------------------------------
initialize :: String -> Int -> Int -> GameState -> IO (IORef BackendState)
initialize title screenWidth screenHeight gs = withCAString title $ \ctitle -> do
  window <- S.createWindow ctitle 0 0 w h wflags
  renderer <- S.createRenderer window (-1) rflags
  t    <- getCurrentTime
  let dims = (screenWidth, screenHeight)
  newIORef $ BackendState t t renderer gs dims (backendToWorld dims) 0 (FSMLevel 1)
  where
    wflags = S.windowFlagShown
    rflags = {-S.rendererFlagPresentVSync .|.-} S.rendererFlagAccelerated
    w = fromIntegral screenWidth
    h = fromIntegral screenHeight

--debugPrintKey sdlEvent = case sdlEvent of
--  S.KeyboardEvent (S.Keysym key mods unicode) ->
--    printf "Key: %s %s %s\n" (show key) (show mods) (show unicode)
--  _ -> return ()
----------------------------------------------------------------------------------------------------
--
-- Returns Nothing if the SDL event is not understood by the game in this FSMState.
--
sdlEventToEvent :: BackendToWorld -> FSMState -> S.Event -> Maybe Event
sdlEventToEvent b2w fsmState sdlEvent =
  -- events that can occur in any FSM State
  case sdlEvent of
--    S.KeyDown _ -> Just Reset
    _           -> (case fsmState of -- events that occur in specific FSM states
                      FSMPlayingLevel -> playingLevel sdlEvent
                      _               -> Nothing)
  where
    playingLevel e = case e of
      _ | Just (x,y) <- isMouseDown e -> Just $ Tap (backendPtToWorldPt b2w (x,y))
      _                       -> Nothing

--
-- True if any mouse button is down
--
isMouseDown :: S.Event -> Maybe (Int32, Int32)
isMouseDown e = case e of
  (S.MouseButtonEvent _ _ _ _ _ s _ _ _) | s == 1 ->
    Just (S.mouseButtonEventX e, S.mouseButtonEventY e)
  _                                               -> Nothing



----------------------------------------------------------------------------------------------------
--
-- Reads the current backend state, runs [f], writes the backend state back with modified GameState,
-- and then runs the continuation [cont] with the latest GameState
--
runOnGameState :: (a -> BackendState -> BackendState)
               -> IORef BackendState
               -> GameM' a
               -> (GameState -> IO ())
               -> IO ()
runOnGameState upd besRef gameM cont  = do
  bes     <- readIORef besRef
  (a, gs) <- runGameM' gameM (besGameState bes)
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
      renderer   = besSDLRenderer bes

  --
  -- I'm not 100% sure why yet but you need to create a new texture each time around
  -- in order to prevent very bad flickering.
  --
  format <- S.masksToPixelFormatEnum (fromIntegral bitsPerPixel)
               (fromBE32 0x0000ff00) (fromBE32 0x00ff0000)
               (fromBE32 0xff000000) (fromBE32 0x000000ff)
  texture <- S.createTexture renderer format S.textureAccessStreaming
                 (fromIntegral w) (fromIntegral h)

  alloca $ \pixelsptr -> alloca $ \pitchptr -> do
    S.lockTexture texture nullPtr pixelsptr pitchptr
    pixels <- peek pixelsptr
    pitch <- fromIntegral <$> peek pitchptr
    res <- C.withImageSurfaceForData (castPtr pixels) C.FormatARGB32 w h pitch $ \surface ->
      C.renderWith surface $ gsRender gs
    S.unlockTexture texture
  S.renderClear renderer
  S.renderCopy renderer texture nullPtr nullPtr
  S.destroyTexture texture
  S.renderPresent renderer
  writeIORef besRef $ bes { besFrames = besFrames bes + 1 }

----------------------------------------------------------------------------------------------------
--
-- [runInputEventHandler] tries to retrieve a game event. If it finds one then
-- [handleEvent] is called on this event and the FSMState in the BackEndState is updated.
--
runInputEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM' FSMState) -> IO ()
runInputEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  let gs       = besGameState bes
      fsmState = besFSMState bes
  mbEvent <- getEvent (besBackendToWorld bes) fsmState
  case mbEvent of
    Left ()         -> exitWith ExitSuccess
    Right Nothing   -> return () -- do nothing
    Right (Just ev) -> runOnGameState' besRef (handleEvent fsmState ev)
  where
    runOnGameState' b c = runOnGameState updFSMState b c (const $ return ())
    updFSMState fsmState bes = bes { besFSMState = fsmState }

----------------------------------------------------------------------------------------------------
runPhysicsEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM' FSMState) -> IO ()
runPhysicsEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  t <- getCurrentTime
  let gs = besGameState bes
      duration = toDouble $ diffUTCTime t (besLastTime bes)
      fsmState = besFSMState bes
  (fsmState', gs') <- runGameM' (handleEvent fsmState (Physics duration)) gs
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
getEvent :: BackendToWorld -> FSMState -> IO (Either () (Maybe Event))
getEvent b2w fsmState = do
  mbSDLEvent <- pollEvent
  case mbSDLEvent of
    Just sdlEvent -> do
      case checkForQuit sdlEvent of
        True -> return $ Left ()
        False -> (case sdlEvent of
                _ -> (case sdlEventToEvent b2w fsmState sdlEvent of
                        Nothing -> getEvent b2w fsmState -- keep polling
                        Just ev -> return $ Right $ Just ev))
    Nothing -> return $ Right Nothing
  where
    -- Checks for a Quit event (caused by closing the window) or whether the Q key is pressed
    checkForQuit e = case e of
      S.QuitEvent _ _           -> True
      _ | b <- isKeyDown e qKey -> b

qKey :: S.Keycode
qKey = 113

isKeyDown :: S.Event -> S.Keycode -> Bool
isKeyDown e code = case e of
  S.KeyboardEvent _ _ _ keyState _ (S.Keysym _ code' _) -> keyState == 1 && code == code'
  _ -> False


pollEvent = alloca $ \eventptr -> do
  status <- S.pollEvent eventptr
  if status == 1 then do
    event <- peek eventptr
    return $ Just event
  else
    return Nothing


----------------------------------------------------------------------------------------------------
mainLoop :: IORef BackendState
         -> (FSMState -> Event -> GameM' FSMState) -- event handler
         -> IO ()
mainLoop besRef handleEvent = loop $ do
  runFrameUpdate       besRef
  runInputEventHandler besRef handleEvent
  runPhysicsEventHandler besRef handleEvent
  logFrameRate besRef
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io

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

