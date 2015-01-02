{-# LANGUAGE ScopedTypeVariables #-}
module Backend.SDL (
  initialize,
  mainLoop
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Surface  as S
import qualified Graphics.UI.SDL.Keycode  as SK
import qualified Graphics.UI.SDL.Mixer    as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import           Graphics.Rendering.OpenGL.Raw
import           Data.Bits
import           Data.Maybe (maybe)


import           Data.List (intersperse)
import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           System.Exit
-- import           Foreign.C.Types (CUChar)
--import           Foreign.Marshal.Alloc (mallocBytes)
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.C.Types (CFloat)
import           Foreign.C.String (withCString, withCStringLen, peekCString)
import           Foreign.Marshal.Array (mallocArray, allocaArray, pokeArray)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Storable (peek)



-- friends
import Types
import Game
import GameM
import Graphics()
import Platform
import CUtil
import Util
import FrameRateBuffer

----------------------------------------------------------------------------------------------------
data BackendState = BackendState { besStartTime      :: UTCTime
                                 , besLastTime       :: UTCTime
                                 , besGLSLState      :: GLSLState
                                 , besGLContext      :: S.GLContext
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
                                 }

data BackendToWorld = BackendToWorld { backendPtToWorldPt     :: (Int, Int) -> R2
                                     , backendNormPtToWorldPt :: (CFloat, CFloat) -> R2 }

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------------------
initOpenGL :: S.Window -> (Int, Int) -> IO (GLSLState, S.GLContext)
initOpenGL window (w,h) = do
  context <- S.glCreateContext window
  mapM_ (uncurry S.glSetAttribute) [ {-(S.GLDoubleBuffer, 1),-} (S.GLDepthSize, 24) ]
  --S.glSetSwapInterval S.SynchronizedUpdates
  S.glSetSwapInterval S.ImmediateUpdates
  glEnable gl_TEXTURE_2D
  glEnable gl_BLEND
  glBlendFunc gl_ALPHA gl_ONE_MINUS_SRC_ALPHA
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LESS
  etVertexShader   <- loadShader vertexShaderSrc gl_VERTEX_SHADER
  vertexShader     <- exitOnLeft etVertexShader
  etFragmentShader <- loadShader fragmentShaderSrc gl_FRAGMENT_SHADER
  fragmentShader   <- exitOnLeft etFragmentShader
  programId        <- glCreateProgram
  when (programId == 0 ) $ exitWithError "Could not create GLSL program"

  glAttachShader programId vertexShader
  glAttachShader programId fragmentShader

  withCString "vPosition" $ \s -> glBindAttribLocation programId 0 s

  glLinkProgram programId
  linked <- alloca $ \ptrLinked -> do
    glGetProgramiv programId gl_LINK_STATUS ptrLinked
    peek ptrLinked
  when (linked == 0) $ do
     errorStr <- getGLError glGetProgramiv glGetProgramInfoLog programId
     exitWithError errorStr

  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glUseProgram programId
  --
  -- The co-ordinates are set to be the world co-ordinate system. This saves us
  -- converting for OpenGL calls
  --
  -- [ortho2D] must appear after glUseProgram programId
  ortho2D programId (-w2) w2 (-h2) h2
  positionIdx <- getAttributeIndex programId "position"
  texCoordIdx <- getAttributeIndex programId "texCoord"
  let glsls = GLSLState { glslProgramId = programId
                        , glslPosition  = positionIdx
                        , glslTexcoord  = texCoordIdx }
  return (glsls, context)
  where
    --
    -- Let aspect ratio be width/height. Let aspect ratio of the world be W and the aspect ratio of
    -- the canvas be C. If W > C then there will margins at the top and bottom of C that are not drawn
    -- to. If W < C then there will be margins on the left and right that will not be drawn to.
    --
    minor = min w h
    scale = realToFrac $ worldMajor / fromIntegral minor
    w2    = (fromIntegral w) * scale / 2
    h2    = (fromIntegral h) * scale / 2
    exitOnLeft et = case et of
                      Left error -> putStrLn error >> exitWith (ExitFailure 1)
                      Right val  -> return val

----------------------------------------------------------------------------------------------------
getAttributeIndex :: ProgramId -> String -> IO AttributeIndex
getAttributeIndex programId s = do
  idx <- withCString s $ \str -> glGetAttribLocation programId str
  when (idx < 0) $ exitWithError (printf "Attribute '%s' is not in vertex shader" s)
  return $ fromIntegral idx

----------------------------------------------------------------------------------------------------
--
-- Open GL functions 'glOrtho' and 'glOrtho2D' are not supported by GL ES 2.0 so we write
-- our own function to create the correct matrix.
--
ortho2D :: ProgramId -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
ortho2D programId left right bottom top = do
  modelView <- withCString "modelView" $ \cstr -> glGetUniformLocation programId cstr
  when (modelView < 0) $ exitWithError "'modelView' uniform doesn't exist"
  allocaArray 16 $ \ortho -> do
    pokeArray ortho [ a,   0,  0, 0
                    , 0,   b,  0, 0
                    , 0,   0,  c, 0
                    , tx, ty, tz, 1 ]
    glUniformMatrix4fv modelView 1 (fromIntegral gl_FALSE ) ortho
  where
    near = -1
    far = 1
    a  = 2 / (right - left)
    b  = 2 / (top - bottom)
    c  = -2.0 / (far - near)
    tx = - (right + left)/(right - left)
    ty = - (top + bottom)/(top - bottom)
    tz = - (far + near)/(far - near)

----------------------------------------------------------------------------------------------------
initialize :: String -> Int -> Int -> GameState -> IO (IORef BackendState)
initialize title screenWidth screenHeight gs = do
  setNoBuffering -- for android debugging
  S.init [S.InitVideo, S.InitAudio]
  window  <- S.createWindow title (S.Position 0 0) (S.Size w h) wflags
  (glslState, context) <- initOpenGL window (w,h)
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
  frBuf <- initFRBuf
  newIORef $ BackendState t t glslState context gs dims (backendToWorld dims) 0 frBuf (FSMLevel 1)
               window levelMusic squishSound
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


isMobile = platform `elem` [IOSPlatform, Android]

--
-- True if any mouse button is down.
--
isMouseOrTouchDown :: BackendToWorld -> S.Event -> Maybe R2
isMouseOrTouchDown b2w e = case S.eventData e of
  S.MouseButton { S.mouseButtonAt = p, S.mouseButtonState = S.Pressed } ->
    Just $ backendPtToWorldPt b2w (S.positionX p, S.positionY p)
  S.TouchFinger { S.touchX = fx, S.touchY = fy } | isMobile ->
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
  (a, gs) <- runGameM (besGLSLState bes) (besGameState bes) gameM
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


----------------------------------------------------------------------------------------------------
runFrameUpdate :: IORef BackendState -> IO ()
runFrameUpdate besRef = do
  bes <- readIORef besRef
  t <- getCurrentTime
  let (w,h)      = besDimensions bes
      gs         = besGameState bes
      sinceStart = toDouble $ diffUTCTime t (besStartTime bes)

  glClearColor 1 1 1 1
  glClear (gl_DEPTH_BUFFER_BIT  .|. gl_COLOR_BUFFER_BIT)
  runGLMIO (gsRender gs) (besGLSLState bes)
  glFlush
  S.glSwapWindow (besWindow bes)

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
  (fsmState', gs') <- runGameM (besGLSLState bes) gs (handleEvent fsmState (Physics duration))
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
    debugLog $ printf "Framerate = %.2f frames/s\n" (1/avTick)


----------------------------------------------------------------------------------------------------
getGLError :: (GLuint -> GLenum -> Ptr GLint -> IO ())
           -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
           -> GLuint -> IO String
getGLError getVal getInfoLog ident = do
        infoLen <- alloca $ \ptrInfoLen -> do
          getVal ident gl_INFO_LOG_LENGTH ptrInfoLen
          peek ptrInfoLen
        if (infoLen > 1)
                    then
                      allocaBytes (fromIntegral infoLen) $ \infoCStr -> do
                        getInfoLog ident infoLen nullPtr infoCStr
                        peekCString infoCStr
                    else return "Shader compiler failure. Couldn't get errorLog"
----------------------------------------------------------------------------------------------------
loadShader :: String -> ShaderType -> IO (Either String ShaderId)
loadShader src typ = do
  shaderId <- glCreateShader typ
  if (shaderId == 0)
   then return $ Left "Could not create shader"
   else do
     withCStringLen src $ \(cString, len) -> allocaArray 1 $ \ptrCString -> allocaArray 1 $ \ptrLength -> do
       pokeArray ptrCString [cString]
       pokeArray ptrLength [fromIntegral len]
       glShaderSource shaderId 1 ptrCString ptrLength
     glCompileShader shaderId
     compileStatus <- alloca $ \ptr -> do
       glGetShaderiv shaderId gl_COMPILE_STATUS ptr
       peek ptr
     if compileStatus == 0
      then do -- error
        errorStr <- getGLError glGetShaderiv glGetShaderInfoLog shaderId
        return $ Left errorStr
      else return $ Right shaderId

----------------------------------------------------------------------------------------------------
vertexShaderSrc :: String
vertexShaderSrc =
  concat $ intersperse "\n" [
      "#ifdef GL_ES"
    , "precision mediump float;        // set default precision for floats"
    , "#endif"
    , "attribute vec2 position;         // vertex position attribute"
    , "attribute vec2 texCoord;         // vertex texture coordinate attribute"
    , "uniform mat4 modelView;          // shader modelview matrix uniform"
    , "varying vec2 texCoordVar;        // vertex texture coordinate varying"
    , "void main()"
    , "{"
    , "  gl_Position = modelView * vec4(position,0,1); // transform vertex position with modelview matrix"
    , "  texCoordVar = texCoord;        // assign the texture coordinate attribute to its varying"
    , "}"
    ]

fragmentShaderSrc :: String
fragmentShaderSrc = concat $ intersperse "\n" [
      "#ifdef GL_ES"
    , "precision mediump float;        // set default precision for floats"
    , "#endif"
    , "uniform sampler2D texture;      // shader texture uniform"
    , " "
    , "varying vec2 texCoordVar;       // fragment texture coordinate varying"
    , " "
    , "void main()"
    , "{"
    , "    // sample the texture at the interpolated texture coordinate"
    , "    // and write it to gl_FragColor "
    , "    gl_FragColor = texture2D( texture, texCoordVar);"
--    , "    gl_FragColor = vec4(1,0,0,1);"
    , "}"
    ]