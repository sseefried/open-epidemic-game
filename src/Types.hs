{-# LANGUAGE TypeFamilies #-}
module Types where

import           Graphics.Rendering.Cairo (Render, FontFace)
import           Graphics.Rendering.OpenGL.Raw (GLint, GLuint, GLenum, GLfloat)
-- import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Physics.Hipmunk as H
import           Data.Map (Map)
import qualified Data.Map as M
import           Control.Applicative

-- friends
import Platform

-- -------------------
-- CO-ORDINATE SYSTEMS
-- -------------------
--
-- For the game we're going to a 4:3 aspect ratio (regardless of the screen).
-- Preferably we get the full height of the screen but for actual screens with aspect ratio
-- smaller than 4:3 we letter box.
--
--
-- There are 4 co-ordinate systems. Because we are using OpenGL and Cairo we
-- can use floating point values in all 3.
--
-- "left-handed" means x-axis goes left to right and y-axis top to bottom.
-- "right-handed" means x-axis goes left to right and y-axis bottom to top.
--
--
-- 1. Screen.
--    Type:   integral
--    System: left-handed
--    Origin: top-left
--    Width:  screen width
--    Height: screen height
--
--    SDL Mouse input comes in this co-ordinate system.
--
-- 2. Normalized
--    Type: fractional
--    System: left-handed
--    Origin: top-left
--    Width: 1.0
--    Height: 1.0
--
--    SDL Touch input comes in this system. This one is interesting because the
--    aspect ratio is 1, not the aspect ratio of the screen. If the screen is wider than high
--    then the the x-axis is "stretched". The distance from 0 to 1 on the x-axis is longer than
--    the same distance on the y-axis, with respect to the screen.
--
-- 3. Canvas
--    Type: fractional
--    System: right-handed
--    Origin: centre.
--    Width:  (see below)
--    Height: (see below)
--
--    The canvas co-ordinate system is for Cairo graphics. When rendering Cairo graphics to
--    image files the co-ordinate system is normally a left-handed one. However, because
--    we are rendering to OpenGL textures which use the World co-ordinate system it becomes
--    a right-handed system.
--
--    The width and height of the canvas are somewhat meaningless as we are rendering
--    to textures of arbitrary size. However, be careful. If you are not using mipmapping
--    the texture will not look good unless the *screen* width/height of the texture.
--    Since the OpenGL context is set to World co-ordinates you will need to scale from
--    World co-ordinates to Screen co-ordinates.
--
-- 4. World (fractional)
--
--    The *world* is split into a *side bar* and a *game field*. The terms have a precise
--    definition. The *world* is the *side bar* plus the *game field*.
--    The game field has an aspect ratio of [fieldAspectRatio]. The remainder is for
--    the side bar. The side bar is to the left of the game field.
--
--    If it is possible to map the full height of the world to the full height of the screen this
--    is done. This is possible when [worldAspectRatio] < screen aspect ratio. Otherwise it is
--    letter boxed.
--
--    The OpenGL context is set up to match the world co-ordinate system. The origin is
--    the centre of the *game field* not the centre of the *game area*.
--
--    # Game Field
--
--    The game field will always have bounds
--      (fieldLeft,       fieldRight,    fieldBottom,      fieldTop)
--    = (-fieldWidth/2, fieldWidth/2, -fieldHeight/2, fieldHeight/2)
--
--    However, because of letter boxing the bounds for the OpenGL othographic bounds may be
--    different.
--
--    # Side bar
--
--    The side bar will have bounds (sideBarLeft, fieldLeft, fieldBottom, fieldTop)
--    where
--      sideBarWidth = worldWidth - fieldWidth
--      sideBarLeft = sideBarLeft - sideBarWidth
--


----------------------------------------------------------------------------------------------------
-- Constants (feel free to change)
--
levelCompleteGrad, gameOverGrad, scoreGrad :: Gradient
levelCompleteGrad = (Color 0.09 0.37 0.16 1, Color 0.09 0.80 0.16 1)
gameOverGrad      = (Color 0.53 0.18 0.18 1, Color 0.73 0.18 0.18 1)
scoreGrad         = (Color 0.0 0.0 0.5 1,    Color 0.0 0.0 1.0 1)
continueGrad      = (Color 0 0 0 1,          Color 0.8  0.8  0.8   1)

--
-- Game constants
--
fieldWidth, fieldHeight :: Double
fieldWidth  = 100
fieldHeight = 100

-- Aspect ratio of entire game area.
worldAspectRatio :: Double
worldAspectRatio = 4/3 -- must be greater than [fieldAspectRatio]. Rest is for side bar

initialGermSize :: Double
initialGermSize = worldHeight / 30

initialGermSizeVariance :: Double
initialGermSizeVariance = worldHeight / 150

doublingPeriod :: Double
doublingPeriod = 3

doublingPeriodVariance :: Double
doublingPeriodVariance = 0.5

resistanceIncrease :: Double
resistanceIncrease = 1.1

-- Maximum number of germs before you are "infected"
maxGerms :: Int
maxGerms = 50

startingAntibioticEffectiveness :: Double
startingAntibioticEffectiveness = 0.9 -- percentage

-- Amount we multiply effectiveness by each time antibiotic is used
effectivenessDilutionFactor :: Double
effectivenessDilutionFactor = 0.97

-- the maximum amount that a component of a Color (R,G,B) can
-- mutate by (either up or down)
gradientColorMutationMax :: Double
gradientColorMutationMax = 0.15

--
-- When a level is finished or an antibiotics is unlocked we need to "mute" the events
-- for a certain amount of time so that a stray click or finger tap doesn't immediately
-- send the player to the next level before they've even notice they finished the level.
--
eventMuteTime :: Double
eventMuteTime = 1.5 -- seconds

unlockAntibioticsMap :: Map Int Antibiotic
unlockAntibioticsMap = M.fromList $ case debugGame of
  True  -> [(5, Penicillin), (10, Cyprofloxacin)]
  False -> [(25, Penicillin), (100, Cyprofloxacin)]

----------------------------------------------------------------------------------------------------
-- Derived constants. (Do not change)

worldWidth, worldHeight :: Double
worldHeight = fieldWidth
worldWidth  = worldAspectRatio * worldHeight

sideBarWidth :: Double
sideBarWidth = worldWidth - fieldWidth

fieldAspectRatio :: Double
fieldAspectRatio = worldWidth/worldHeight

fieldLeft, fieldRight, fieldBottom, fieldTop :: Double
fieldLeft   = -fieldWidth/2
fieldRight  =  fieldWidth/2
fieldBottom = -fieldHeight/2
fieldTop    =  fieldHeight/2

sideBarLeft, sideBarRight, sideBarBottom, sideBarTop :: Double
sideBarLeft   = fieldLeft - sideBarWidth
sideBarRight  = fieldLeft
sideBarBottom = fieldBottom
sideBarTop    = fieldTop

worldLeft, worldRight, worldBottom, worldTop :: Double
worldLeft   = sideBarLeft
worldRight  = fieldRight
worldBottom = fieldBottom
worldTop    = fieldTop

antibioticWidth = sideBarWidth * 0.8

----------------------------------------------------------------------------------------------------
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
type Frac = Double

type Time = Double

type Anim = Time -> Render ()

data Color = Color !Double !Double !Double !Double deriving Show

type CairoPoint = (Double, Double)

white, blue, green, black, whiteT :: Color
white           = Color 1 1 1 1
blue            = Color 0 0 1 1
green           = Color 0 1 0 1
black           = Color 0 0 0 1
whiteT          = Color 1 1 1 0 -- T means transparent
backgroundColor = whiteT


type Gradient = (Color, Color)

uniformGrad :: Color -> Gradient
uniformGrad c = (c,c)

-- polar point (r,a) that lies inside a unit circle. Invariant: r < 1, 0 <= a < 1
data PolarPoint = P2 Frac Frac -- radius and angle

data GermGfx =
  GermGfx { germGfxBodyGrad    :: !Gradient
          , germGfxNucleusGrad :: !Gradient
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
                 , germGfx            :: GermGfx
                 , germGL             :: GermGL
                 , germCumulativeTime :: Time
                 , germAnimTime       :: Time
                 , germSelected       :: Bool
                 , germResistances    :: [Antibiotic]
                 }
----------------------------------------------------------------------------------------------------
--
-- In a perfect world I would wrap OpenGL in a free monad (just like I did for Hipmunk)
-- to hide the IO monad. I still might do that, but for now
--
--
data OrthoBounds =
  OrthoBounds { orthoLeft    :: Double
              , orthoRight   :: Double
              , orthoBottom  :: Double
              , orthoTop     :: Double
              , screenScale  :: Double -- convert from world distance to screen distance
              } deriving (Show, Eq)

data GLM a = GLM { unGLM :: GLSLState -> IO a }

data GLSLState = GLSLState { glslPosition    :: AttributeLocation
                           , glslTexcoord    :: AttributeLocation
                           , glslDrawTexture :: UniformLocation
                           , glslColor       :: UniformLocation
                           , glslProgramId   :: ProgramId
                           , glslOrthoBounds :: OrthoBounds
                           -- FIXME: not really GLSL... Maybe rename this state?
                           , glslFontFace    :: FontFace
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
-- Lifts an [IO] in to the [GLM] monad.
--
liftGLM :: IO a -> GLM a
liftGLM io = GLM . const $ io

getGLSLState :: GLM GLSLState
getGLSLState = GLM return

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
data GermGL = GermGL { germGLFun :: Int    -- z index
                                 -> R2     -- position
                                 -> Time   -- cumulative animation time
                                 -> Double -- radius
                                 -> Double -- amplitude scale
                                 -> GLM ()
                     , germGLFinaliser :: GLM () }

----------------------------------------------------------------------------------------------------
data HipCirc  = HipCirc  { _hipCircShape  :: !H.Shape }
type HipSpace = H.Space
----------------------------------------------------------------------------------------------------
type GermId = Int

data GameState = GameState { gsRender        :: GLM () -- GL commands
                           , gsBounds        :: !(Int, Int)
                           , gsGerms         :: !(Map GermId Germ)
                           , gsNextGermId    :: !GermId
                           , gsHipState      :: HipSpace
                           , gsSoundQueue    :: ![GameSound]
                           , gsCurrentLevel  :: !Int
                           , gsAntibiotics   :: Map Antibiotic AntibioticData
                           , gsScore         :: Int
                           }
data AntibioticData = AntibioticData { abEffectiveness :: Double
                                     , abEnabled       :: Bool
                                     , abInitPos       :: R2     -- init position on screen
                                     , abPos           :: R2     -- actual position on screen
                                     , abSelected      :: Bool
                                     }


data GameSound = GameSoundLevelMusicStart -- start level music
               | GameSoundLevelMusicStop  -- stop level music
               | GameSoundSquish

----------------------------------------------------------------------------------------------------
data Antibiotic = Penicillin
                | Cyprofloxacin
                deriving (Eq, Show, Ord, Enum)

allAntibiotics :: [Antibiotic]
allAntibiotics = let p = Penicillin in [p..]

numAntibiotics :: Int
numAntibiotics = length allAntibiotics

