{-# LANGUAGE ScopedTypeVariables #-}
module GraphicsGL.Util where

import           Graphics.Rendering.OpenGL.Raw
import           GHC.Word

-- friends
import Types
import Util
import GraphicsGL.GLM
import Platform
import Foreign

----------------------------------------------------------------------------------------------------
bytesPerWord32 :: Int
bytesPerWord32 = 4

----------------------------------------------------------------------------------------------------
floatSize :: Int
floatSize = sizeOf (undefined :: GLfloat)

----------------------------------------------------------------------------------------------------
withBoundTexture :: TextureId -> IO a -> IO a
withBoundTexture textureId io = withBinder (glBindTexture gl_TEXTURE_2D) textureId io

----------------------------------------------------------------------------------------------------
withVBO :: BufferId -> IO a -> IO a
withVBO bufferId io = withBinder (glBindBuffer gl_ARRAY_BUFFER) bufferId io

----------------------------------------------------------------------------------------------------
withBinder :: Num b => (b -> IO ()) -> b -> IO a -> IO a
withBinder f i io = do { f i; res <- io; f 0; return res }

----------------------------------------------------------------------------------------------------
rgbFormat :: GLint
rgbFormat = fromIntegral $ case platform of
  Android     -> gl_BGRA
  IOSPlatform -> gl_RGBA
  _           -> gl_RGBA

----------------------------------------------------------------------------------------------------
-- Helper function
genGen :: Storable a => (GLint -> Ptr a -> IO ()) -> IO a
genGen gen = alloca $ \(ptr :: Ptr a) -> do { gen 1 ptr; peek ptr }

----------------------------------------------------------------------------------------------------
genTexture :: IO TextureId
genTexture = genGen glGenTextures

----------------------------------------------------------------------------------------------------
genFrameBuffer :: IO FrameBufferId
genFrameBuffer = genGen glGenFramebuffers

----------------------------------------------------------------------------------------------------
genBuffer :: IO BufferId
genBuffer = genGen glGenBuffers

----------------------------------------------------------------------------------------------------
getScreenFrameBufferId :: IO FrameBufferId
getScreenFrameBufferId = do
  alloca $ \(ptr :: Ptr GLint) ->
    do glGetIntegerv gl_FRAMEBUFFER_BINDING ptr
       fbId <- peek ptr
       return $ fromIntegral fbId
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
-- Frees the [textureId] for reuse and deletes any bound textures.
--
poolTexture :: GfxState -> TextureId -> IO ()
poolTexture gfxs textureId = modifyIORef (gfxTexturePool gfxs) (textureId:)

----------------------------------------------------------------------------------------------------
--
-- Gets new textureId from the pool.
--
getTextureIdFromPool :: GfxState -> IO TextureId
getTextureIdFromPool gfxs = do
  let ref = gfxTexturePool gfxs
  textures  <- readIORef ref
  textureId <- case textures of
                 []                     -> genTexture
                 (textureId:textureIds) -> writeIORef ref textureIds >> return textureId
  return textureId

----------------------------------------------------------------------------------------------------
--
-- Generates an FBO, which is a framebuffer plus its associated color texture. (We are not
-- dealing with depth textures in this game).
--

genFBO :: (Int, Int) -> IO FBO
genFBO (w,h) = do
  fb <- genFrameBuffer
  glBindFramebuffer gl_FRAMEBUFFER fb

  texture <- genTexture
  withBoundTexture texture $ do
    glTexImage2D gl_TEXTURE_2D 0 rgbFormat glW glH 0 gl_BGRA gl_UNSIGNED_BYTE nullPtr
    -- Required for non-power-of-two textures in GL ES 2.0
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S     (fromIntegral gl_CLAMP_TO_EDGE)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T     (fromIntegral gl_CLAMP_TO_EDGE)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)
    glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D texture 0
    status <- glCheckFramebufferStatus gl_FRAMEBUFFER
    when (status /= gl_FRAMEBUFFER_COMPLETE) $ do
      exitWithError "Framebuffer error"
    return $ FBO { fboFrameBuffer = fb, fboTexture = texture }
  where
    glW = fromIntegral w
    glH = fromIntegral h

----------------------------------------------------------------------------------------------------
{-# INLINE clearBuffer #-}
--
-- Clears a buffer of n 32-bit words to a particular color.
--
clearBuffer :: Color -> Int -> Ptr a -> IO ()
clearBuffer (Color r g b a) n ptr = do
  let for = forN n
  for 0 $ \i -> do
    let pk off x = pokeByteOff ptr (i*bytesPerWord32+off) (toColorWord x)
    pk 0 r
    pk 1 g
    pk 2 b
    pk 3 a
  where
    toColorWord :: Double -> Word8
    toColorWord x = floor (x*255.0)

----------------------------------------------------------------------------------------------------
--
-- Generates a new texture, does something with it, frees it.
--
withPooledTexture :: GfxState -> (TextureId -> IO a) -> IO a
withPooledTexture gfxs f = do
  textureId <- getTextureIdFromPool gfxs
  res       <- f textureId
  poolTexture gfxs textureId
  return res

----------------------------------------------------------------------------------------------------
f2gl :: Double -> GLfloat
f2gl = realToFrac

----------------------------------------------------------------------------------------------------
compileGLSLSource :: GLSLSource -> IO ProgramId
compileGLSLSource p = do
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
glslProgramUseAndInit :: GLSLProgram a -> IO ()
glslProgramUseAndInit p = do
  glUseProgram (glslProgramId p)
  glslInit p


----------------------------------------------------------------------------------------------------
screenSizeVBO :: (GLfloat, GLfloat, GLfloat, GLfloat) -> IO BufferId
screenSizeVBO (left, right, bottom, top) = do
  vertexBuf <- genBuffer
  let bufSize = (4*4*floatSize)
  -- FIXME: sseefried: Needs a depth
  allocaArray bufSize $ \(vs :: Ptr GLfloat) -> do
    pokeArray vs [ left,  bottom, 0, 0
                 , right, bottom, 1, 0
                 , left,  top,    0, 1
                 , right, top,    1, 1
                 ]
    glBindBuffer gl_ARRAY_BUFFER vertexBuf
    glBufferData gl_ARRAY_BUFFER (fromIntegral bufSize) vs gl_STATIC_DRAW -- P1
    glBindBuffer gl_ARRAY_BUFFER 0
  return vertexBuf
