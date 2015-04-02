{-# LANGUAGE ScopedTypeVariables #-}
module Backend.SDL (
  initialize,
  mainLoop
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Mixer    as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import           Graphics.Rendering.OpenGL.Raw
import           Data.Bits
import           Data.IORef
import           Data.Time
import           System.Directory (doesDirectoryExist)
import qualified Data.Map as M

-- friends
import GraphicsGL.GLSLPrograms.SeparateShaders as GLSL1
import Backend.Events
import Types
import Game.Types
import Game
import GameM
import GameEvent
import Platform
import CUtil
import Util
import FrameRateBuffer
import GraphicsGL
import Coordinate
import Foreign (CFloat)

----------------------------------------------------------------------------------------------------
data BackendState = BackendState { _besStartTime     :: UTCTime
                                 , besLastTime       :: UTCTime
                                 , _besDims          :: (Int, Int)
                                 , besGfxState       :: GfxState
                                 , _besGLContext     :: S.GLContext
                                 , besGameState      :: GameState
                                 , besPressHistory   :: IORef PressHistory
                                 , besBackendToWorld :: BackendToWorld
                                 , besFrames         :: Integer
                                 , besFRBuf          :: FRBuf
                                 , besFSMState       :: FSMState
                                 -- must keep a handle on the window otherwise it gets
                                 -- garbage collected and hence disappears.
                                 , besWindow         :: S.Window
                                 , besLevelMusic     :: Maybe M.Music
                                 , besSquishSound    :: Maybe M.Chunk
                                 }

----------------------------------------------------------------------------------------------------
initOpenGL :: S.Window -> IO S.GLContext
initOpenGL window = do
  --
  -- On iOS the you must set these attributes *before* the gl context is created.
  --
  let glAttrs = case True of
                _ | platform `elem` [Android, IOSPlatform] ->
                       [ (S.GLDepthSize,           24)
                       , (S.GLContextProfileMask,  S.glContextProfileES)
                       , (S.GLContextMajorVersion, 2) ]
                _ -> [ (S.GLDepthSize,           24) ]
  mapM_ (uncurry S.glSetAttribute) glAttrs
  context <- S.glCreateContext window
  when (platform == MacOSX) $ S.glSetSwapInterval S.ImmediateUpdates
  return context


----------------------------------------------------------------------------------------------------
initialize :: String -> Maybe String -> IO (IORef BackendState)
initialize title mbResourcePath = do
  setNoBuffering -- for android debugging
  S.init [S.InitVideo, S.InitAudio]
  dims@(w,h) <- case screenDimensions of
    Just (w',h') -> return (w', h')
    Nothing -> do
      mode <- S.getCurrentDisplayMode 0
      return (fromIntegral (S.displayModeWidth mode), fromIntegral (S.displayModeHeight mode))

  when (w < h) $ exitWithError $
    printf "Width of screen (%d) must be greater than or equal to height (%d)" w h
  window  <- S.createWindow title (S.Position 0 0) (S.Size w h) wflags
  resourcePath <- case platform of
    Android ->
      maybe (exitWithError "Resource path must be provided to haskell_main for Android")
            return mbResourcePath
    _ -> iOSResourcePath
  debugLog $ printf "Resource path is `%s'" resourcePath
  dirExists <- doesDirectoryExist resourcePath
  when (not dirExists ) $ exitWithError $
    printf "Resource path `%s' does not exist" resourcePath
  context <- initOpenGL window
  gfxState <- initGfxState (w,h) GLSL1.initShaders resourcePath
  (levelMusic, squishSound) <- case platform of
    NoSound -> return (Nothing, Nothing)
    _       -> do
      M.openAudio 44100 S.AudioS16Sys 2 1024
      M.allocateChannels 10
      levelMusic  <- M.loadMUS $ resourcePath ++ "/music.ogg"
      rwOps       <- S.fromFile (resourcePath ++ "/slime-splash.wav") "r"
      squishSound <- M.loadWAVRW rwOps False
      return (Just levelMusic, Just squishSound)
  t     <- getCurrentTime
  frBuf <- initFRBuf 100 -- FIXME: Turn framebuffer window size into constant
  gs    <- newGameState (w,h)
  pressHistory <- newIORef $ PressHistory Nothing M.empty
  newIORef $ BackendState { _besStartTime     = t
                          , besLastTime       = t
                          , _besDims          = dims
                          , besGfxState       = gfxState
                          , _besGLContext     = context
                          , besGameState      = gs
                          , besPressHistory   = pressHistory
                          , besBackendToWorld = backendToWorld dims
                          , besFrames         = 0
                          , besFRBuf          = frBuf
                          , besFSMState       = FSMLevel startLevelGerms
                          , besWindow         = window
                          , besLevelMusic     = levelMusic
                          , besSquishSound    = squishSound
                          }
  where
    -- WindowBorderLess is required for iOS so that status bar does not show on iOS 6 and below.
    wflags = [S.WindowShown] ++ (case platform of IOSPlatform -> [S.WindowBorderless]; _ -> [])

----------------------------------------------------------------------------------------------------
--
-- Reads the current backend state, runs [f], writes the backend state back with modified GameState,
-- and then runs the continuation [cont] with the latest GameState
--
_runOnGameState :: (a -> BackendState -> BackendState)
               -> IORef BackendState
               -> GameM a
               -> (GameState -> IO ())
               -> IO ()
_runOnGameState upd besRef gameM cont  = do
  bes     <- readIORef besRef
  (a, gs) <- runGameM (besGfxState bes) (besGameState bes) gameM
  writeIORef besRef $ upd a $ bes { besGameState = gs }
  cont gs

----------------------------------------------------------------------------------------------------
--
-- Executes an IO action, times it and then updates the BackendState with that duration.
_runAndTime :: IORef BackendState -> (Time -> BackendState -> BackendState) -> IO a -> IO a
_runAndTime besRef upd io = do
  bes <- readIORef besRef
  t <- getCurrentTime
  result <- io
  t' <- getCurrentTime
  writeIORef besRef $ upd (realToFrac $ diffUTCTime t' t) bes
  return result
  where


----------------------------------------------------------------------------------------------------
runFrameUpdate :: IORef BackendState -> IO ()
runFrameUpdate besRef = do
  let f2f :: Double -> CFloat
      f2f = realToFrac
  bes <- readIORef besRef
  let gs  = besGameState bes
      (Color r g b _) = backgroundColor -- FIXME: Shouldn't be transparent
      gfxs = besGfxState bes
      mainFBO = gfxMainFBO gfxs
  -- Only update if the render is dirty
  when (gsRenderDirty gs) $ do
    -- FIXME: Should this code really be here? What about in GraphicsGL?

    glBindFramebuffer gl_FRAMEBUFFER (fboFrameBuffer mainFBO)
    glClearColor (f2f r) (f2f g) (f2f b) 1 -- here it must be opaque
    glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)
    runGLMIO gfxs $ gsScreenRender gs
    mapM_ (runGLMIO gfxs . (uncurry drawLetterBox))
          (letterBoxes . worldGLSLOrthoBounds . glslData . gfxWorldGLSL $ gfxs)
    when debugSystem $ renderDebugInfo besRef
    ---
    modifyIORef besRef $ \bes -> bes { besGameState = gs { gsRenderDirty = False }}
    S.glSwapWindow (besWindow bes)

renderDebugInfo :: IORef BackendState -> IO ()
renderDebugInfo besRef = do
  bes <- readIORef besRef
  let glsls = besGfxState bes
  runGLMIO glsls $ drawTextLinesOfWidth_ (Color 0 0 0 1) (R2 0 0) fieldWidth
                     [show $ _besDims bes]

----------------------------------------------------------------------------------------------------
type LetterBox = ((Double, Double), (Double, Double))

letterBoxes :: OrthoBounds -> [LetterBox]
letterBoxes b = [ left, right, bottom, top]
  where
    screenWidth = orthoRight b - orthoLeft b
    screenHeight = orthoTop b - orthoBottom b

    left   = ((orthoLeft b, orthoBottom b), (worldLeft - orthoLeft b,   screenHeight))
    right  = ((worldRight, orthoBottom b),  (orthoRight b - worldRight, screenHeight))
    --
    bottom = ((orthoLeft b, orthoBottom b), (screenWidth, worldBottom - orthoBottom b))
    top    = ((orthoLeft b, worldTop),      (screenWidth,   orthoTop b - worldTop))

----------------------------------------------------------------------------------------------------
--
-- [runInputEventHandler] tries to retrieve a game event. If it finds one then
-- [handleEvent] is called on this event and the FSMState in the BackEndState is updated.
--
runInputEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runInputEventHandler besRef handleEvent = do
  mbEvent <- getEvents besRef
  case mbEvent of
    Quit        -> exitWith ExitSuccess
    Events []   -> return () -- do nothing
    Events evs  -> runUntilFSMStateChange evs
  where
    --
    -- Multiple events can be returned. It's possible that one of those events
    -- could cause a level to finish. When such a FSM state change occurs we must flush
    -- the rest of the events.
    --
    runUntilFSMStateChange :: [Event] -> IO ()
    runUntilFSMStateChange [] = return ()
    runUntilFSMStateChange (ev:evs) = do
        bes <- readIORef besRef
        let fsmState = besFSMState bes
            gs = besGameState bes
        (fsmState', gs') <- runGameM (besGfxState bes) gs (handleEvent fsmState ev)
        writeIORef besRef $ bes { besGameState = gs', besFSMState = fsmState' }
        if fsmState == fsmState' then runUntilFSMStateChange evs else return ()

----------------------------------------------------------------------------------------------------
--
-- Like [modifyIORef] but takes a monadic action.
--
withIORef :: IORef a -> (a -> IO a) -> IO ()
withIORef ioRef io = do { a <- readIORef ioRef; a' <- io a; writeIORef ioRef a' }

----------------------------------------------------------------------------------------------------
projIORef :: (s -> a) -> IORef s -> IO a
projIORef f ioRef = do { s <- readIORef ioRef; return $ f s }

----------------------------------------------------------------------------------------------------
runPhysicsEventHandler :: IORef BackendState -> (FSMState -> Event -> GameM FSMState) -> IO ()
runPhysicsEventHandler besRef handleEvent = do
  t <- getCurrentTime
  duration <- (realToFrac . diffUTCTime t) <$> projIORef besLastTime besRef
  -- Actions that must always be run
  withIORef besRef $ \bes -> do
    addTick (besFRBuf bes) duration
    let gs       = besGameState bes
        fsmState = besFSMState  bes
    (fsmState', gs') <- runGameM (besGfxState bes) gs (handleEvent fsmState (Physics duration))
    playSoundQueue bes (gsSoundQueue gs')
    -- update the fsmState and gameState.
    return $ bes { besFSMState  = fsmState'
                 , besLastTime  = t
                 , besGameState = gs' { gsSoundQueue = [] }
                 , besFrames    = besFrames bes + 1 }

  -- Actions that should be run in various game states.
  --withIORef besRef $ \bes -> do
  --  case besFSMState bes of
  --    _ -> return bes

----------------------------------------------------------------------------------------------------
playSoundQueue :: BackendState -> [GameSound] -> IO ()
playSoundQueue bes sounds = mapM_ playSound sounds
  where
    playSound :: GameSound -> IO ()
    playSound s = case s of
      GSLevelMusicStart -> do
        maybe (return ()) (\wav -> M.playMusic wav 10000) -- loop a lot of times
              (besLevelMusic  bes)
      GSLevelMusicStop  -> M.haltMusic
      GSLevelMusicPause -> M.pauseMusic >> M.pause (-1)
      GSLevelMusicResume -> M.resumeMusic >> M.resume (-1)
      GSSquish          -> do
         maybe (return ()) (\wav -> M.playChannelTimed (-1) wav 0 (-1) >> return ())
               (besSquishSound bes)

----------------------------------------------------------------------------------------------------
getEvents :: IORef BackendState -> IO (MaybeEvents)
getEvents besRef = do
  bes <- readIORef besRef
  case besFSMState bes of
    FSMPaused _ -> pauseEventHandler besRef
    _ -> eventHandler (besPressHistory bes) (besBackendToWorld bes)
  where
    pauseEventHandler besRef = do
     debugLog $ "AppDidEnterBackground. Waiting for foreground event"
     waitForForegroundEvent
     -- need to reset last time to now since we have been paused.
     -- Need to pretend it was one frame ago so that we don't pass a 0 duration to the physics
     -- engine which causes problems.
     t <- (addUTCTime $ -1/(realToFrac desiredFramerate)) <$> getCurrentTime
     --
     modifyIORef besRef $ \bes -> bes { besLastTime = t }
     return $ Events [Resume]

----------------------------------------------------------------------------------------------------
mainLoop :: IORef BackendState
         -> (FSMState -> Event -> GameM FSMState) -- event handler
         -> IO ()
mainLoop besRef handleEvent = loop $ do
  runFrameUpdate       besRef
  runInputEventHandler besRef handleEvent
  runPhysicsEventHandler besRef handleEvent
  when (not profiling) $ delayBasedOnAverageFramerate besRef
  logFrameRate besRef
  where
    loop :: IO () -> IO ()
    loop io = io >> loop io

----------------------------------------------------------------------------------------------------
delayBasedOnAverageFramerate :: IORef BackendState -> IO ()
delayBasedOnAverageFramerate besRef = do
  bes <- readIORef besRef
  avTick <- averageTick (besFRBuf bes)
  let t = ceiling $ (max (1/desiredFramerate - avTick) 0) * 1000.0
  S.delay t

----------------------------------------------------------------------------------------------------
logFrameRate :: IORef BackendState -> IO ()
logFrameRate besRef = do
  bes <- readIORef besRef
  let sz = fromIntegral 100 -- FIXME: window size constant
  when (besFrames bes `mod` sz == 0 && besFSMState bes == FSMPlayingLevel) $ do
    avTick <- averageTick (besFRBuf bes)
    debugLog $ printf "Framerate = %.2f frames/s\n" (1/avTick)

