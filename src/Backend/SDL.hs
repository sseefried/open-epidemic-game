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
import           Data.List (intersperse)
import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           System.Exit
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.C.Types (CFloat)
import           Foreign.C.String (withCString, withCStringLen, peekCString)
import           Foreign.Marshal.Array (allocaArray, pokeArray)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Storable (peek)
import           Control.Applicative ((<$>))
import           System.Directory (doesDirectoryExist)
import qualified Data.Map as M

-- friends
import Backend.Events
import Types
import GLM
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
import FreeType

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
                      Left error -> debugLog error >> exitWith (ExitFailure 1)
                      Right val  -> return val

----------------------------------------------------------------------------------------------------

initOpenGL :: S.Window -> (Int, Int) -> String -> IO (GfxState, S.GLContext)
initOpenGL window (w,h) resourcePath = do
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
  --  glEnable gl_TEXTURE_2D is meaningless in GLSL
  glEnable gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LESS
  glViewport 0 0 (fromIntegral w) (fromIntegral h)

  mainFBO   <- genFBO (w,h)
  worldGLSL <- initWorldGLSL (w,h)
  blurGLSL  <- initBlurGLSL (w,h)
  fontFace  <- loadFontFace $ resourcePath ++ "/font.ttf"

  --
  let gfxs = GfxState { gfxWorldGLSL   = worldGLSL
                      , gfxBlurGLSL    = blurGLSL
                      , gfxFontFace    = fontFace
                      , gfxMainFBO     = mainFBO
                      }
  return (gfxs, context)

initWorldGLSL :: (Int, Int) -> IO WorldGLSL
initWorldGLSL (w,h) = do
  programId <- compileGLSLProgram worldGLSLProgram
  --
  -- The co-ordinates are set to be the world co-ordinate system. This saves us
  -- converting for OpenGL calls
  --
  let bds = orthoBounds (w,h)
  ortho2D programId bds
  [positionLoc, texCoordLoc] <- mapM (getAttributeLocation programId) ["position", "texCoord"]
  [drawTextureLoc, colorLoc] <- mapM (getUniformLocation  programId) ["drawTexture", "color"]
  return $ WorldGLSL { worldGLSLPosition    = positionLoc
                     , worldGLSLTexcoord    = texCoordLoc
                     , worldGLSLDrawTexture = drawTextureLoc
                     , worldGLSLColor       = colorLoc
                     , worldGLSLOrthoBounds = bds
                     , worldGLSLProgramId   = programId
                     }

initBlurGLSL :: (Int, Int) -> IO BlurGLSL
initBlurGLSL (w, h) = do
  programId <- compileGLSLProgram blurGLSLProgram
  glUseProgram programId
  modelView <- withCString "modelView" $ \cstr -> glGetUniformLocation programId cstr
  when (modelView < 0) $ exitWithError "'modelView' uniform doesn't exist"
  allocaArray 16 $ \(ortho :: Ptr GLfloat) -> do
    pokeArray ortho $
      [ 1, 0, 0, 0
      , 0, 1, 0, 0
      , 0, 0, 1, 0
      , 0, 0, 0, 1 ]
    glUniformMatrix4fv modelView 1 (fromIntegral gl_FALSE ) ortho

  let blurFactorNames = map (printf "blurFactor%d") [0..4 :: Int]
  [positionLoc, texCoordLoc] <- mapM (getAttributeLocation programId) ["position", "texCoord"]
  [axis,radius, bf0, bf1, bf2, bf3, bf4] <- mapM (getUniformLocation programId)
                                                 ("axis":"radius":blurFactorNames)
  fbo <- genFBO (w,h)
  glUniform1f radius (fromIntegral $ min w h)
  return $ BlurGLSL { blurGLSLPosition  = positionLoc
                    , blurGLSLTexcoord  = texCoordLoc
                    , blurGLSLFactor0   = bf0
                    , blurGLSLFactor1   = bf1
                    , blurGLSLFactor2   = bf2
                    , blurGLSLFactor3   = bf3
                    , blurGLSLFactor4   = bf4
                    , blurGLSLAxis      = axis
                    , blurGLSLPhase1FBO = fbo
                    , blurGLSLProgramId = programId
                    }


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
-- 'near = -zMax' and 'far = -zMin'
--
--
ortho2D :: ProgramId -> OrthoBounds -> IO ()
ortho2D texGLSLProgramId bds = do
  glUseProgram texGLSLProgramId
  modelView <- withCString "modelView" $ \cstr -> glGetUniformLocation texGLSLProgramId cstr
  when (modelView < 0) $ exitWithError "'modelView' uniform doesn't exist"
  allocaArray 16 $ \(ortho :: Ptr GLfloat) -> do
    pokeArray ortho $ map d2gl
      [ a,   0,  0, 0
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
    a    = 2 / (right - left)
    b    =  2 / (top - bottom)
    c    = -2 / (far - near)
    tx   = - (right + left)/(right - left)
    ty   = - (top + bottom)/(top - bottom)
    tz   =   (far + near)/(far - near)

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
  (gfxState, context) <- initOpenGL window (w,h) resourcePath
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
  frBuf <- initFRBuf
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
    glBindFramebuffer gl_FRAMEBUFFER (fboFrameBuffer mainFBO)
    glClearColor (f2f r) (f2f g) (f2f b) 1 -- here it must be opaque
    glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)
    runGLMIO gfxs $ gsScreenRender gs
    mapM_ (runGLMIO gfxs . (uncurry drawLetterBox))
          (letterBoxes . worldGLSLOrthoBounds . gfxWorldGLSL $ gfxs)
    when debugSystem $ renderDebugInfo besRef
    glFlush
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
  delayBasedOnAverageFramerate besRef
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
  let sz = fromIntegral frBufWindowSize
  when (besFrames bes `mod` sz == 0 && besFSMState bes == FSMPlayingLevel) $ do
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
data GLSLProgram = GLSLProgram { glslVertexShader :: String
                               , glslFragmentShader :: String }

glslVertexShaderDefault :: String
glslVertexShaderDefault =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "uniform mat4 modelView;"
          , "varying vec2 vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  gl_Position = modelView * vec4(position,1);"
          , "  vTexCoord = texCoord;"
          , "}"
          ]

worldGLSLProgram :: GLSLProgram
worldGLSLProgram =
  GLSLProgram {
      glslVertexShader = glslVertexShaderDefault
    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "uniform bool drawTexture;"
          , "uniform vec4 color;"
          , " "
          , "varying vec2 vTexCoord;"
          , " "
          , "void main()"
          , "{"
          , "  if (drawTexture) {"
          , "    // sample the texture at the interpolated texture coordinate"
          , "    // and write it to gl_FragColor "
          , "    gl_FragColor = texture2D(texture, vTexCoord);"
          , "  } else {"
          , "    gl_FragColor = color;"
          , "  }"
          , "}"
          ]
  }

blurGLSLProgram :: GLSLProgram
blurGLSLProgram =
  GLSLProgram {
      glslVertexShader = glslVertexShaderDefault
    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "varying vec2 vTexCoord;"
          , ""
          , "uniform float blurFactor0;"
          , "uniform float blurFactor1;"
          , "uniform float blurFactor2;"
          , "uniform float blurFactor3;"
          , "uniform float blurFactor4;"
          , "uniform float radius;"
          , ""
          , "uniform bool axis;"
          , ""
          , "float scale = 1.0/radius;"
          , "vec4 blurComponent(float dist, float blurFactor) {"
          , "  if (axis) { // y-axis"
          , "   return texture2D(texture, vec2(vTexCoord.x, vTexCoord.y + dist*scale))*blurFactor;"
          , "  } else {"
          , "   return texture2D(texture, vec2(vTexCoord.x + dist*scale, vTexCoord.y))*blurFactor;"
          , "  }"
          , "}"
          , ""
          , "void main() {"
          , "  vec4 sum = vec4(0.0);"
          , "  sum += blurComponent( 0.0, blurFactor0);"
          , "  sum += blurComponent( 1.0, blurFactor1);"
          , "  sum += blurComponent(-1.0, blurFactor1);"
          , "  sum += blurComponent( 2.0, blurFactor2);"
          , "  sum += blurComponent(-2.0, blurFactor2);"
          , "  sum += blurComponent( 3.0, blurFactor3);"
          , "  sum += blurComponent(-3.0, blurFactor3);"
          , "  sum += blurComponent( 4.0, blurFactor4);"
          , "  sum += blurComponent(-4.0, blurFactor4);"
          , ""
          , "  gl_FragColor = vec4(sum.xyz, 1.0);"
          , "}"
        ]
  }

--
-- A GLSL program that just passes a texture straight through.
--
_idGLSLProgram :: GLSLProgram
_idGLSLProgram =
  GLSLProgram {
      glslVertexShader   =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "varying vec2 vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  gl_Position = vec4(position,1);"
          , "  vTexCoord = texCoord;"
          , "}"
          ]
    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "varying vec2 vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  gl_FragColor = texture2D(texture, vTexCoord);"
          , "}"
          ]
  }
