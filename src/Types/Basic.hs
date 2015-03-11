{-# LANGUAGE TypeFamilies #-}
module Types.Basic where

import           Graphics.Rendering.Cairo (Render)
import qualified Physics.Hipmunk as H

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

----------------------------------------------------------------------------------------------------
data HipCirc  = HipCirc  { _hipCircShape  :: !H.Shape }
type HipSpace = H.Space
----------------------------------------------------------------------------------------------------
type GermId = Int


data AntibioticData = AntibioticData { abEffectiveness :: Double
                                     , abEnabled       :: Bool
                                     , abInitPos       :: R2     -- init position on screen
                                     , abPos           :: R2     -- actual position on screen
                                     , abSelected      :: Bool
                                     }

data GameSound = GSLevelMusicStart -- start level music
               | GSLevelMusicPause -- pause level music
               | GSLevelMusicResume
               | GSLevelMusicStop  -- stop level music
               | GSSquish

----------------------------------------------------------------------------------------------------
data Antibiotic = Penicillin
                | Ciprofloxacin
                deriving (Eq, Show, Ord, Enum)

allAntibiotics :: [Antibiotic]
allAntibiotics = let p = Penicillin in [p..]

numAntibiotics :: Int
numAntibiotics = length allAntibiotics

