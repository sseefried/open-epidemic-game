module Backend.SDL (
  initialize,
  mainLoop
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import qualified Graphics.UI.SDL.Mixer    as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import qualified Graphics.Rendering.Cairo as C

import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           System.Exit
import           Foreign.Ptr (castPtr)
import           Foreign.C.Types (CFloat)

-- friends
import Types
import Game
import GameM
import Graphics()
import Platform
import CUtil
import FrameRateBuffer

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
                                 , besFRBuf          :: FRBuf
                                 , besFSMState       :: FSMState
                                 -- must keep a handle on the window otherwise it gets
                                 -- garbage collected and hence disappears.
                                 , besWindow         :: S.Window
                                 , besLevelMusic     :: M.Music
                                 , besSquishSound    :: M.Chunk
                                 , besTexture        :: S.Texture
                                 }

data BackendToWorld = BackendToWorld { backendPtToWorldPt     :: (Int, Int) -> R2
                                     , backendNormPtToWorldPt :: (CFloat, CFloat) -> R2 }

backendToWorld ::  (Int, Int) -> BackendToWorld
backendToWorld (w,h) =
  BackendToWorld { backendPtToWorldPt = \(x,y) -> R2 ((fromIntegral x - w'/2)  * scale)
                                                      ((h'/2 - fromIntegral y) * scale)
                 , backendNormPtToWorldPt = \(fx,fy) -> R2 (frac (fx - 0.5) (w' * scale))
                                                           (frac (0.5 - fy) (h' * scale))
                 }
  where
    minor = min w' h'
    scale = worldMajor / minor
    w' = fromIntegral w
    h' = fromIntegral h
    frac f x = cFloatToDouble f * x

----------------------------------------------------------------------------------------------------
initialize :: String -> Int -> Int -> GameState -> IO (IORef BackendState)
initialize title screenWidth screenHeight gs = do
  S.init [S.InitVideo, S.InitAudio]
  window   <- S.createWindow title (S.Position 0 0) (S.Size w h) wflags
  renderer <- S.createRenderer window S.FirstSupported rflags


  (levelMusic, squishSound) <- case platform of
    Android -> return (error "levelMusic", error "squishSound")
    NoSound -> return (error "levelMusic", error "squishSound")
    _       -> do
     M.openAudio 44100 S.AudioS16Sys 1 1024
     M.allocateChannels 10
     levelMusic <- M.loadMUS "/Users/sseefried/code/games/epidemic-game/sounds/crystal-harmony.wav"
     rwOps <- S.fromFile "/Users/sseefried/code/games/epidemic-game/sounds/slime-splash.wav" "r"
     squishSound <- M.loadWAVRW rwOps False
     return (levelMusic, squishSound)
  t        <- getCurrentTime
  let dims = (screenWidth, screenHeight)
  texture <- S.createTexture renderer S.PixelFormatARGB8888 S.TextureAccessStreaming
                 (fromIntegral w) (fromIntegral h)
  frBuf <- initFRBuf
  newIORef $ BackendState t t renderer gs dims (backendToWorld dims) 0 frBuf (FSMLevel 1) window
                levelMusic squishSound texture

  where
    wflags = [S.WindowShown]
    -- Note: for debuggin purposes you can see the true framerate by commented out [PresentVSync]
    rflags = [S.Accelerated] -- , S.PresentVSync]
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
                      FSMPlayingLevel _ -> playingLevel sdlEvent
                      FSMGameOver       -> gameOver sdlEvent
                      _                 -> Nothing)
  where
    playingLevel e = case e of
      _ | Just pt <- isMouseOrTouchDown b2w e -> Just $ Tap pt
      _                                       -> Nothing
    ---------------------------------------
    gameOver e = case e of
      _ | Just _ <- isMouseOrTouchDown b2w e -> Just TapAnywhere
      _                                      -> Nothing

--
-- True if any mouse button is down.
--
isMouseOrTouchDown :: BackendToWorld -> S.Event -> Maybe R2
isMouseOrTouchDown b2w e = case S.eventData e of
  S.MouseButton { S.mouseButtonAt = p, S.mouseButtonState = S.Pressed } ->
    Just $ backendPtToWorldPt b2w (S.positionX p, S.positionY p)
  S.TouchFinger { S.touchX = fx, S.touchY = fy } ->
    Just $ backendNormPtToWorldPt b2w (fx, fy)
  _                                              -> Nothing

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
  (a, gs) <- runGameM (besGameState bes) gameM
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
      texture    = besTexture bes
  S.lockTexture texture Nothing $ \(pixels, pitch) -> do
                       res <- C.withImageSurfaceForData (castPtr pixels) C.FormatARGB32 w h pitch $ \surface ->
                         C.renderWith surface $ gsRender gs
                       S.unlockTexture texture
  S.renderClear renderer
  S.renderCopy renderer texture Nothing Nothing
  S.renderPresent renderer

----------------------------------------------------------------------------------------------------
--
-- [runInputEventHandler] tries to retrieve a game event. If it finds one then
-- [handleEvent] is called on this event and the FSMState in the BackEndState is updated.
--
runInputEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runInputEventHandler besRef handleEvent = do
  bes <- readIORef besRef
  let fsmState = besFSMState bes
  mbEvent <- getEvent (besBackendToWorld bes) fsmState
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
  (fsmState', gs') <- runGameM gs (handleEvent fsmState (Physics duration))
  case fsmState' of
    FSMPlayingLevel _ -> do
      -- If there are any queued sounds play them now
      playSoundQueue bes (gsSoundQueue gs')
      addTick (besFRBuf bes) duration
      writeIORef besRef $ bes { besGameState = gs' { gsSoundQueue = [] }
                              , besLastTime = t, besFSMState = fsmState'
                              , besFrames = besFrames bes + 1 }
    _ -> return ()

----------------------------------------------------------------------------------------------------
playSoundQueue :: BackendState -> [GameSound] -> IO ()
playSoundQueue bes = case platform of
  Android -> const $ return ()  -- don't play any sounds on android. FIXME: Change this
  NoSound -> const $ return ()  -- don't play any sounds with NoSound
  _       -> mapM_ playSound
  where
    playSound :: GameSound -> IO ()
    playSound s = case s of
      GameSoundLevelMusicStart -> M.playMusic (besLevelMusic  bes) 10000 -- loop a lot of times
      GameSoundLevelMusicStop  -> M.haltMusic
      GameSoundSquish     -> M.playChannelTimed (-1) (besSquishSound bes) 0 (-1) >> return ()

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
  mbSDLEvent <- S.pollEvent
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
    checkForQuit e = case S.eventData e of
      S.Quit                    -> True
      _ | b <- isKeyDown e qKey -> b




qKey :: SK.Keycode
qKey = SK.Q

isKeyDown :: S.Event -> SK.Keycode -> Bool
isKeyDown e code = case S.eventData e of
  S.Keyboard {  S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ code' _ } -> code == code'
  _ -> False


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

logFrameRate :: IORef BackendState -> IO ()
logFrameRate besRef = do
  bes <- readIORef besRef
  when (besFrames bes `mod` 30 == 0) $ do
    avTick <- averageTick (besFRBuf bes)
    printf "Framerate = %.2f frames/s\n" (1/avTick)




