{-# LANGUAGE ScopedTypeVariables #-}
module Backend.SDL (
  initialize,
  mainLoop
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import qualified Graphics.UI.SDL.Mixer    as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import           Graphics.Rendering.OpenGL.Raw
import           Data.Bits
import           Data.List (intersperse)
import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           System.Exit
-- import           Foreign.C.Types (CUChar)
--import           Foreign.Marshal.Alloc (mallocBytes)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C.Types (CFloat)
import           Foreign.C.String (withCString, withCStringLen, peekCString)
import           Foreign.Marshal.Array (allocaArray, pokeArray)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Storable (peek)
import           Control.Applicative ((<$>))


-- friends
import Types
import Game
import GameM
import Graphics()
import Platform
import CUtil
import Util
import FrameRateBuffer
import GraphicsGL (drawLetterBox)

----------------------------------------------------------------------------------------------------
data BackendState = BackendState { _besStartTime     :: UTCTime
                                 , besLastTime       :: UTCTime
                                 , besGLSLState      :: GLSLState
                                 , _besGLContext     :: S.GLContext
                                 , besGameState      :: GameState
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
backendToWorld ::  (Int, Int) -> BackendToWorld
backendToWorld (w,h) =
  BackendToWorld { backendPtToWorldPt = \(x,y) -> R2 ((fromIntegral x)  * scale + left)
                                                      (top - (fromIntegral y) * scale)
                 , backendNormPtToWorldPt = \(fx,fy) -> R2 ((cf2d fx * w' * scale) + left)
                                                           (top - (cf2d fy * h' * scale))
                 }
  where
    bds      = orthoBounds (w,h)
    left     = orthoLeft bds
    top      = orthoTop bds

    scale    = 1/(screenScale bds)
    w'       = fromIntegral w
    h'       = fromIntegral h
    cf2d     = cFloatToDouble

----------------------------------------------------------------------------------------------------
--
-- Calculates the bounds of the visible screen in terms of world co-ordinates.
--
orthoBounds :: (Int, Int) -> OrthoBounds
orthoBounds (w,h) =
  OrthoBounds { orthoLeft   =  worldLeft   - (lbhm / scale)
              , orthoRight  =  worldRight  + (lbhm / scale)
              , orthoBottom =  worldBottom - (lbvm / scale)
              , orthoTop    =  worldTop    + (lbvm / scale)
              , screenScale =  scale
              }
  where
    aspectRatio :: Double
    aspectRatio = w'/h'
    w', h' :: Double
    w' = fromIntegral w
    h' = fromIntegral h
    scale = realToFrac $ lbsh / worldHeight
    letterBoxScreenHeight = if aspectRatio > worldAspectRatio
                             then h' else h' * (aspectRatio/worldAspectRatio)
    lbsh = letterBoxScreenHeight
    letterBoxVertMargin = (h' - lbsh)/2
    lbvm = letterBoxVertMargin

    letterBoxScreenWidth = if aspectRatio > worldAspectRatio
                            then w' * (worldAspectRatio/aspectRatio) else w'
    lbsw = letterBoxScreenWidth
    letterBoxHorizMargin = (w' - lbsw)/2
    lbhm = letterBoxHorizMargin

----------------------------------------------------------------------------------------------------
compileGLSLProgram :: GLSLProgram -> IO ProgramId
compileGLSLProgram p = do
  etVertexShader   <- loadShader (glslVertexShader p) gl_VERTEX_SHADER
  vertexShader     <- exitOnLeft etVertexShader
  etFragmentShader <- loadShader (glslFragmentShader p) gl_FRAGMENT_SHADER
  fragmentShader   <- exitOnLeft etFragmentShader
  programId        <- glCreateProgram
  when (programId == 0 ) $ exitWithError "Could not create GLSL program"
  glAttachShader programId vertexShader
  glAttachShader programId fragmentShader
  glLinkProgram programId
  linked <- alloca $ \ptrLinked -> do
    glGetProgramiv programId gl_LINK_STATUS ptrLinked
    peek ptrLinked
  when (linked == 0) $ do
     errorStr <- getGLError glGetProgramiv glGetProgramInfoLog programId
     exitWithError errorStr
  return programId
  where
    exitOnLeft et = case et of
                      Left error -> putStrLn error >> exitWith (ExitFailure 1)
                      Right val  -> return val

----------------------------------------------------------------------------------------------------

initOpenGL :: S.Window -> (Int, Int) -> IO (GLSLState, S.GLContext)
initOpenGL window (w,h) = do
  context <- S.glCreateContext window
  mapM_ (uncurry S.glSetAttribute) [ {-(S.GLDoubleBuffer, 1),-} (S.GLDepthSize, 24) ]
--  S.glSetSwapInterval S.SynchronizedUpdates
  S.glSetSwapInterval S.ImmediateUpdates
  --  glEnable gl_TEXTURE_2D is meaningless in GLSL
  glEnable gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LESS
  programId <- compileGLSLProgram textureProgram
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glUseProgram programId
  --
  -- The co-ordinates are set to be the world co-ordinate system. This saves us
  -- converting for OpenGL calls
  --
  -- [ortho2D] must appear after glUseProgram programId
  let bds = orthoBounds (w,h)
  ortho2D programId bds
  positionLoc    <- getAttributeLocation programId "position"
  texCoordLoc    <- getAttributeLocation programId "texCoord"
  drawTextureLoc <- getUniformLocation   programId "drawTexture"
  colorLoc       <- getUniformLocation   programId "color"
  let glsls = GLSLState { glslProgramId   = programId
                        , glslPosition    = positionLoc
                        , glslTexcoord    = texCoordLoc
                        , glslDrawTexture = drawTextureLoc
                        , glslColor       = colorLoc
                        , glslOrthoBounds = bds
                        }
  return (glsls, context)
  where
----------------------------------------------------------------------------------------------------
getShaderLocation :: (ProgramId -> Ptr GLchar -> IO GLint) -> String -> ProgramId -> String
                  -> IO VariableLocation
getShaderLocation getLoc variableSort programId s = do
  idx <- withCString s $ \str -> getLoc programId str
  when (idx < 0) $ exitWithError (printf "%s '%s' is not in shader program" variableSort s)
  return $ fromIntegral idx

----------------------------------------------------------------------------------------------------
getAttributeLocation :: ProgramId -> String -> IO AttributeLocation
getAttributeLocation = getShaderLocation glGetAttribLocation "Attribute"

----------------------------------------------------------------------------------------------------
getUniformLocation :: ProgramId -> String -> IO UniformLocation
getUniformLocation p s = fromIntegral <$> getShaderLocation glGetUniformLocation "Uniform" p s

----------------------------------------------------------------------------------------------------
--
-- Open GL functions 'glOrtho' and 'glOrtho2D' are not supported by GL ES 2.0 so we write
-- our own function to create the correct matrix.
--
-- See http://en.wikipedia.org/wiki/Orthographic_projection
--
-- One of the particularly confusing aspects of the orthographic projection is the
-- definitions of the terms "near" and "far". (See http://math.hws.edu/graphicsnotes/c3/s5.html)
-- 'near' is actually negative 'zMax' and 'far' is negative 'zMin'
--
--
ortho2D :: ProgramId -> OrthoBounds -> IO ()
ortho2D programId bds = do
  modelView <- withCString "modelView" $ \cstr -> glGetUniformLocation programId cstr
  when (modelView < 0) $ exitWithError "'modelView' uniform doesn't exist"
  allocaArray 16 $ \(ortho :: Ptr GLfloat) -> do
    pokeArray ortho [ a,   0,  0, 0
                    , 0,   b,  0, 0
                    , 0,   0,  c, 0
                    , tx, ty, tz, 1 ]
    glUniformMatrix4fv modelView 1 (fromIntegral gl_FALSE ) ortho
  where
    (left,right,bottom,top) = (orthoLeft bds, orthoRight bds, orthoBottom bds, orthoTop bds )
    near = realToFrac $ -zMax
    far  = realToFrac $ -zMin
    d2gl :: Double -> GLfloat
    d2gl = realToFrac
    a    = d2gl $ 2 / (right - left)
    b    = d2gl $  2 / (top - bottom)
    c    = d2gl $ -2 / (far - near)
    tx   = d2gl $ - (right + left)/(right - left)
    ty   = d2gl $ - (top + bottom)/(top - bottom)
    tz   = d2gl $   (far + near)/(far - near)

----------------------------------------------------------------------------------------------------
initialize :: String -> IO (IORef BackendState)
initialize title = do
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
  t     <- getCurrentTime
  frBuf <- initFRBuf
  gs    <- newGameState (w,h)
  newIORef $ BackendState t t glslState context gs (backendToWorld dims) 0 frBuf (FSMLevel 1)
               window levelMusic squishSound
  where
    wflags = [S.WindowShown]


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
  case fsmState of -- events that occur in specific FSM states
    FSMPlayingLevel   -> playingLevel sdlEvent
    FSMGameOver       -> tapAnywhere sdlEvent
    FSMLevelComplete  -> tapAnywhere sdlEvent
    _                 -> Nothing
  where
    playingLevel e = case e of
      _ | Just pt <- isMouseOrTouchDown b2w e -> Just $ Tap pt
      _                                       -> Nothing
    ---------------------------------------
    tapAnywhere e = case e of
      _ | Just _ <- isMouseOrTouchDown b2w e -> Just TapAnywhere
      _                                      -> Nothing


--
-- True if any mouse button is down.
--
isMouseOrTouchDown :: BackendToWorld -> S.Event -> Maybe R2
isMouseOrTouchDown b2w e = case S.eventData e of
  S.MouseButton { S.mouseButtonAt = p, S.mouseButtonState = S.Pressed } | not isMobile ->
    Just $ backendPtToWorldPt b2w (S.positionX p, S.positionY p)
  S.TouchFinger { S.touchFingerEvent = S.TouchFingerDown, S.touchX = fx, S.touchY = fy } | isMobile ->
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
_runAndTime :: IORef BackendState -> (Time -> BackendState -> BackendState) -> IO a -> IO a
_runAndTime besRef upd io = do
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
  let f2f :: Double -> CFloat
      f2f = realToFrac
  bes <- readIORef besRef
  let gs  = besGameState bes
      (Color r g b a) = backgroundColor
      glsls = besGLSLState bes
  glClearColor (f2f r) (f2f g) (f2f b) (f2f a)
  glClear (gl_DEPTH_BUFFER_BIT  .|. gl_COLOR_BUFFER_BIT)
  runGLMIO glsls (gsRender gs)
  mapM_ (runGLMIO glsls . (uncurry drawLetterBox)) $ letterBoxes (glslOrthoBounds glsls)
  glFlush
  S.glSwapWindow (besWindow bes)

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
  duration <- (toDouble . diffUTCTime t) <$> projIORef besLastTime besRef
  withIORef besRef $ \bes -> do
    let gs = besGameState bes
        fsmState = besFSMState bes
    (fsmState', gs') <- runGameM (besGLSLState bes) gs (handleEvent fsmState (Physics duration))
    playSoundQueue bes (gsSoundQueue gs')
    -- update the fsmState and gameState.
    return $ bes { besFSMState = fsmState'
                 , besGameState = gs' { gsSoundQueue = [] } }

  -- extra actions
  withIORef besRef $ \bes -> do
    case besFSMState bes of
      FSMPlayingLevel -> do
        -- If there are any queued sounds play them now
        addTick (besFRBuf bes) duration
        return $ bes { besLastTime = t
                     , besFrames = besFrames bes + 1 }
      _ -> return bes

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
  when (besFrames bes `mod` 30 == 0 && besFSMState bes == FSMPlayingLevel) $ do
    avTick <- averageTick (besFRBuf bes)
    debugLog $ printf "Framerate = %.2f frames/s" (1/avTick)


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
data GLSLProgram = GLSLProgram { glslVertexShader :: String
                               , glslFragmentShader :: String }

textureProgram :: GLSLProgram
textureProgram =
  GLSLProgram {
      glslVertexShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "uniform mat4 modelView;"
          , "varying vec2 texCoordVar;"
          , ""
          , "void main()"
          , "{"
          , "  gl_Position = modelView * vec4(position,1);"
          , "  texCoordVar = texCoord;"
          , "}"
          ]
    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "uniform bool drawTexture;"
          , "uniform vec4 color;"
          , " "
          , "varying vec2 texCoordVar;"
          , " "
          , "void main()"
          , "{"
          , "  if (drawTexture) {"
          , "    // sample the texture at the interpolated texture coordinate"
          , "    // and write it to gl_FragColor "
          , "    gl_FragColor = texture2D(texture, texCoordVar);"
          , "  } else {"
          , "    gl_FragColor = color;"
          , "  }"
          , "}"
          ]
  }