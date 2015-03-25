--
--
-- Here's the beauty of the GLM monad.
--
--  You can only run GLM monads inside the GameM monad (not IO)
--
-- The downside at present is that it's very easy to just use [liftGLM] or [glm]
-- to inject arbitrary IO actions into the GameM monad.
--
-- The right, but time consuming, thing to do would be to wrap every single GL call
-- in the GLM monad and only expose those.
--
-- For now, try to enforce the discipline of only creating GLM things from the building blocks
-- in module [GraphicsGL]
--
module GraphicsGL.GLM (
  -- Types
  GLM, -- opaque
  GfxState(..),
  WorldGLSL(..),
  BlurGLSL(..),
  Screen(..),
  FBO(..),
  GLSLSource(..),
  GLSLProgram(..),
  ShadersGenerator,
  MipMapIndex,
  ProgramId,
  ShaderId,
  ShaderType,
  AttributeLocation,
  UniformLocation,
  VariableLocation,
  TextureId,
  FrameBufferId,
 -- constants
  zMax,
  zMin,
  -- functions

  glm, -- smart constructor
  liftGLM,
  getGfxState,
  runGLMIO,
--  unsafeSequenceGLM
) where

import Graphics.Rendering.Cairo (FontFace)
import Graphics.Rendering.OpenGL.Raw

-- friends
import Types.Basic
import Util

--
-- Magnitude of near and far planes in orthographic project
--
zMin, zMax :: GLfloat
zMin = -10000
zMax = 10000

type MipMapIndex       = GLint
type ProgramId         = GLuint
type ShaderId          = GLuint
type ShaderType        = GLenum
type AttributeLocation = GLuint
type UniformLocation   = GLint
type VariableLocation  = GLuint
type TextureId         = GLuint
type FrameBufferId     = GLuint


data GLSLSource = GLSLSource { glslVertexShader   :: String
                             , glslFragmentShader :: String
                             }

data GLSLProgram a = GLSLProgram { glslProgramId :: ProgramId
                                 , glslData      :: a
                                 , glslInit      :: IO ()
                                 }

data GfxState = GfxState { gfxBlurGLSL    :: GLSLProgram BlurGLSL
                         , gfxWorldGLSL   :: GLSLProgram WorldGLSL
                         , gfxFontFace    :: FontFace
                         , gfxMainFBO     :: FBO
                         -- on iOS this is not 0!
                         , gfxScreenFBId  :: FrameBufferId
                         }

-- FIXME: Probably want to change the names of these data structures
data WorldGLSL = WorldGLSL {
                     worldGLSLPosition    :: AttributeLocation
                   , worldGLSLTexcoord    :: AttributeLocation
                   , worldGLSLDrawTexture :: UniformLocation
                   , worldGLSLColor       :: UniformLocation
                   , worldGLSLOrthoBounds :: OrthoBounds
                   }

data BlurGLSL = BlurGLSL {
                  blurGLSLPosition  :: AttributeLocation
                , blurGLSLTexcoord  :: AttributeLocation
                , blurGLSLFactor0   :: UniformLocation
                , blurGLSLFactor1   :: UniformLocation
                , blurGLSLFactor2   :: UniformLocation
                , blurGLSLFactor3   :: UniformLocation
                , blurGLSLFactor4   :: UniformLocation
                , blurGLSLAxis      :: UniformLocation
                , blurGLSLPhase1FBO :: FBO
                }

type ShadersGenerator = (Int,Int) -> IO (GLSLProgram WorldGLSL, GLSLProgram BlurGLSL)

data Screen = Screen

data FBO = FBO { fboFrameBuffer :: FrameBufferId, fboTexture :: TextureId }

data GLM a = GLM { unGLM :: GfxState -> IO a }

instance Functor GLM where
  -- (a -> b) -> (GLM a -> GLM b)
  fmap f (GLM g) = GLM $ fmap f . g

instance Monad GLM where
  return = GLM . const . return
  (GLM f) >>= k = GLM $ \as -> f as >>= \a -> unGLM (k a) as

instance Applicative GLM where
  pure = return
  (GLM f) <*> (GLM f') = GLM $ liftA2 (<*>) f f'

--
-- Lifts an [IO] in to the [GLM] monad.
--
liftGLM :: IO a -> GLM a
liftGLM io = GLM $ const io

-- Same as the GLM constructor. The difference is that [GLM] is opaque and can't be
-- pattern matched against
--
glm :: (GfxState -> IO a) -> GLM a
glm = GLM

getGfxState :: GLM GfxState
getGfxState = GLM return

runGLMIO :: GfxState -> GLM a -> IO a
runGLMIO glsls (GLM f) = f glsls

--unsafeSequenceGLM :: GLM p1 a -> GLM p2 b -> GLM p2 b
--unsafeSequenceGLM (GLM f) glm' = (GLM f) >> glm'