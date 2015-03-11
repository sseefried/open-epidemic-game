module Game.Types (
  module Game.Types,
  GLM -- re-export this
) where


import           Data.Map (Map)
import           Control.Applicative

-- friends
import Types.Basic
import Types.GLM


data GermGL = GermGL { germGLFun :: Int    -- z index
                                 -> R2     -- position
                                 -> Time   -- cumulative animation time
                                 -> Double -- radius
                                 -> Double -- amplitude scale
                                 -> GLM ()
                     , germGLFinaliser :: GLM ()
                     }

data GameState = GameState { gsRender        :: GLM () -- GL commands
                           , gsRenderDirty   :: Bool
                           , gsBounds        :: !(Int, Int)
                           , gsGerms         :: !(Map GermId Germ)
                           , gsNextGermId    :: !GermId
                           , gsHipState      :: HipSpace
                           , gsSoundQueue    :: ![GameSound]
                           , gsCurrentLevel  :: !Int
                           , gsAntibiotics   :: Map Antibiotic AntibioticData
                           , gsScore         :: Int
                           }

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
                 , germGeneration     :: Int -- how many parents did this germ have?
                 , germCumulativeTime :: Time
                 , germAnimTime       :: Time
                 , germSelected       :: Bool
                 , germResistances    :: [Antibiotic]
                 }
