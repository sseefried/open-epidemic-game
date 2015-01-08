{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Types where

import           Graphics.Rendering.Cairo (Render)
import           Graphics.Rendering.OpenGL.Raw (GLint, GLuint, GLenum, GLfloat)
-- import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Physics.Hipmunk as H
import           Data.Map (Map)
import           Control.Applicative


--
-- World co-ordinates vs. canvas co-ordinates
--
-- People may play this game at a variety of different resolutions. The co-ordinate system
-- this is in is known as the canvas co-ordinates. For the purposes of the physics the
-- co-ordindate system will remain fixed.
--
-- exported
data R2 = R2 !Double !Double deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
--
-- Co-ordinate systems
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Internally we use type [CairoPoint]. This uses Cairo's co-ordinate system which has the origin
-- in the top-left corner, x axis goes left to right and y axis goes to bottom.
--
type Frac = Float

type Time = Double

type Anim = Time -> Render ()

data Color = Color !Double !Double !Double !Double deriving Show

type CairoPoint = (Double, Double)



white, blue, green, black :: Color
white = Color 1 1 1 1
blue  = Color 0 0 1 1
green = Color 0 1 0 1
black = Color 0 0 0 1



type GermGradient = (Color, Color)

-- polar point (r,a) that lies inside a unit circle. Invariant: r < 1, 0 <= a < 1
data PolarPoint = P2 Frac Frac -- radius and angle

data GermGfx =
  GermGfx { germGfxBodyGrad    :: !GermGradient
          , germGfxNucleusGrad :: !GermGradient
          , germGfxBody        :: [MovingPoint]
          , germGfxNucleus     :: [MovingPoint]
          , germGfxSpikes      :: !Int
          }

----------------------------------------------------------------------------------------------------
--
-- Represents a periodic function as data.
-- \t -> pfAmp * sinU ((t + pfPhase)/pfPeriod)
--
type PeriodicFun = (Frac, Frac, Frac)
----------------------------------------------------------------------------------------------------
--
-- A [MovingPoint] is a polar point but each component 'r' and 'a' has been
-- annotated with a list of [PeriodicFun]s.
-- The final point at a time 't' is determined by summing the value of
-- the periodic functions at 't' and adding that to the component.
--
type MovingPoint = ((Frac, PeriodicFun),(Frac, PeriodicFun))

--
--
-- The [germCumulativeTime] field is used in animating the germs. This is the value
-- that is passed to the [drawGerm] function in the Graphics module.
-- This value grows inversely proportional to the size of the germ because I've found
-- that small germs need to animate quicker to look like they are moving at all. See function
-- [growGerm].
--
-- [germGL] is a closure which takes the germ position, its size, its [germGfx] and returns OpenGL
-- commands for drawing it.
--
data Germ = Germ { germMultiplyAt     :: Time
                 , germSizeFun        :: Time -> Double
                 , germHipCirc        :: HipCirc
                 , germPos            :: R2 -- cached pos
                 , germGfx            :: GermGfx
                 , germGL             :: GermGL
                 , germCumulativeTime :: Time
                 , germAnimTime       :: Time
                 }
----------------------------------------------------------------------------------------------------
--
-- In a perfect world I would wrap OpenGL in a free monad (just like I did for Hipmunk)
-- to hide the IO monad. I still might do that, but for now
--
--
data GLM a = GLM { unGLM :: GLSLState -> IO a }

data GLSLState = GLSLState { glslPosition  :: AttributeIndex
                           , glslTexcoord  :: AttributeIndex
                           , glslProgramId :: ProgramId
                           }

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
-- Magnitude of near and far planes in orthographic project
--
zMin, zMax :: GLfloat
zMin = -10000
zMax = 10000

type MipMapIndex     = GLint
type ProgramId       = GLuint
type ShaderId        = GLuint
type ShaderType      = GLenum
type AttributeIndex  = GLuint
type TextureId       = GLuint
data GermGL = GermGL { germGLFun :: Int    -- z index
                                 -> R2     -- position
                                 -> Time   -- cumulative animation time
                                 -> Double -- radius
                                 -> GLM ()
                     , germGLFinaliser :: GLM () }

----------------------------------------------------------------------------------------------------
data HipCirc  = HipCirc  { _hipCircShape  :: !H.Shape }
--
-- The canvas might not have the same aspect ratio as the world, in which case
-- we ensure there will be some portions of the canvas that won't be drawn to.
--

data WorldToCanvas = WorldToCanvas { worldPtToCanvasPt :: R2 -> CairoPoint
                                   , worldLenToCanvasLen :: Double -> Double }

type HipSpace = H.Space
----------------------------------------------------------------------------------------------------
type GermId = Int

data GameState = GameState { gsRender        :: GLM () -- GL commands
                           , gsBounds        :: !(Int, Int)
                           , gsGerms         :: !(Map GermId Germ)
                           , gsWorldToCanvas :: !WorldToCanvas
                           , gsNextGermId    :: !GermId
                           , gsHipState      :: HipSpace
                           , gsSoundQueue    :: ![GameSound]
                           , gsCurrentLevel  :: Int
                           }

data GameSound = GameSoundLevelMusicStart -- start level music
               | GameSoundLevelMusicStop  -- stop level music
               | GameSoundSquish
----------------------------------------------------------------------------------------------------

