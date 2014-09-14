{-# LANGUAGE GADTs, LiberalTypeSynonyms #-}
module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--

-- system imports
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import           Control.Applicative
import           Text.Printf
import           Data.Map (Map)
import qualified Data.Map as M

-- friends
import Graphics

----------------------------------------------------------------------------------------------------
--
-- Game constants
--
worldWidth, worldHeight, worldMajor :: Double
worldWidth  = 100
worldHeight = 100
worldMajor = max worldWidth worldHeight

initialGermSize :: Double
initialGermSize = worldMajor / 50

doublingPeriod :: Double
doublingPeriod = 3

doublingPeriodVariance :: Double
doublingPeriodVariance = 0.5

resistanceIncrease :: Double
resistanceIncrease = 1.1



----------------------------------------------------------------------------------------------------
--
-- World co-ordinates vs. canvas co-ordinates
--
-- People may play this game at a variety of different resolutions. The co-ordinate system
-- this is in is known as the canvas co-ordinates. For the purposes of the physics the
-- co-ordindate system will remain fixed.
--
-- exported
data R2 = R2 Double Double

--
-- The canvas might not have the same aspect ratio as the world, in which case
-- we ensure there will be some portions of the canvas that won't be drawn to.
--

data WorldToCanvas = WorldToCanvas { worldPtToCanvasPt :: R2 -> CairoPoint
                                   , worldLenToCanvasLen :: Double -> Double}

--
-- Let aspect ratio be width/height. Let aspect ration of the world be W and the aspect ratio of
-- the  canvas be C. If W > C then there will margins at the top and bottom of C that are not drawn
-- to.  If W < C then there will be margins on the left and right that will not be drawn to.
--
worldToCanvas :: (Int, Int) -> WorldToCanvas
worldToCanvas (w,h) =
  WorldToCanvas { worldPtToCanvasPt   = \(R2 x y) -> (w'/2 + scale*x, h'/2 - scale*y)
                , worldLenToCanvasLen = \len -> scale * len  }
  where
    w' = fromIntegral w
    h' = fromIntegral h
    minor = min w' h'
    scale = minor / worldMajor

----------------------------------------------------------------------------------------------------
--
--
-- The [germCumulativeTime] field is used in animating the germs. This is the value
-- that is passed to the [drawGerm] function in the Graphics module.
-- This value grows inversely proportional to the size of the germ because I've found
-- that small germs need to animate quicker to look like they are moving at all. See function
-- [growGerm].
--

data Germ = Germ { germMultiplyAt     :: Time
                 , germGrowthRate     :: Double
                 , germSize           :: Double
                 , germPos            :: R2
                 , germGfx            :: GermGfx
                 , germCumulativeTime :: Time
                 }


growthRateForSteps :: Int -> Double
growthRateForSteps n = 2**(1/fromIntegral n)

-- TODO: Remove magic numbers
createGerm :: RandomGen g => R2 -> Rand g Germ
createGerm pos = do
  gfx <- randomGermGfx
  return $ Germ { germMultiplyAt        = 3
                , germGrowthRate        = growthRateForSteps (3 * 30)
                , germSize              = initialGermSize
                , germPos               = pos
                , germGfx               = gfx
                , germCumulativeTime    = 0
                }


----------------------------------------------------------------------------------------------------
--
-- Events
-- ~~~~~~
--
-- This game may be implemented on a variety of backends, some which have mice/touchpads,
-- some which have touch screens, some which have keyboards and some that don't. One approach
-- would be for the game logic to understand the basic concepts of 'keypress', 'mouse click',
-- 'touch event', etc, but a more abstract way to do things is simply to have a notion of
-- "game events" such as "germ squash", "quit", "continue to next level", etc.
-- It is then the backend's job to correctly translate its events to these game events.
--
-- This actually saves me a lot of work. For instance, were we to have a notion of 'keypress'
-- in the game logic then you would have to account for every different kind of key that could
-- be pressed, which is not only a large number of keys but would likely be different between
-- the different backends. (Some would leave out certain keys not common on all keyboards, some
-- would have them. I'd have to choose a subset that worked for all of them, etc.)
--


----------------------------------------------------------------------------------------------------
-- The game monad
type GameM a = StateT GameState (Rand StdGen) a

----------------------------------------------------------------------------------------------------
--
-- Finite State Machine states for this game.
--
data FSMState = FSMLevel Int -- level number
              | FSMAntibioticUnlocked
              | FSMLevelComplete
              | FSMGameOver
              deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
data GameState = GameState { gsRender        :: Render ()
                           , gsBounds        :: (Int, Int)
                           , gsGerms         :: [Germ]
                           , gsWorldToCanvas :: WorldToCanvas
                           }

----------------------------------------------------------------------------------------------------
{-
TODO: I would really like it if there was some (fairly easy) way of associating a particular
data type with each constructor of the FSM. As it stands I could easily make a mistake in my
code and have backend code that returned an event that wasn't handled by a particular FSM
state. At this point these errors can only be caught at run-time.
-}
data Event = Tap (Double, Double) -- location at which tap occurred.
           | TapAnywhere          -- tap occurred but anywhere.
           | NextFrame
           | Reset
           | Physics Time -- how much time the last frame took
           deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
--
-- The sorts of events that can occur are dependent on the state of the FSM.
--
newGameState :: (Int, Int) -> IO GameState
newGameState bounds = do
  g <- evalRandIO $ createGerm (R2 0 0)
  return $ initGameState bounds [g]

resetGameState :: GameM ()
resetGameState = do
  gs <- get
  g  <- lift $ createGerm (R2 0 0)
  put $ initGameState (gsBounds gs) [g]

initGameState :: (Int,Int) -> [Germ] -> GameState
initGameState bounds germs = GameState (return ()) bounds germs (worldToCanvas bounds)

----------------------------------------------------------------------------------------------------
--
-- The game as a Finite State Machine
--
handleEvent :: FSMState -> Event -> GameM FSMState
handleEvent fsmState ev = do
  -- events that can occur in any FSM State
  gs <- get
  case ev of
    Reset   -> resetGameState >> (return $ FSMLevel 1)
    _  -> (case fsmState of -- events that depend on current FSM State
            FSMLevel i            -> fsmLevel i
            FSMAntibioticUnlocked -> fsmAntibioticUnlocked
            FSMLevelComplete      -> fsmLevelComplete
            FSMGameOver           -> fsmGameOver
          )
  where
    fsmLevel i = case ev of
      Tap (x,y)        -> error "This is where you kill a germ"
      Physics duration -> physics duration >> return fsmState
      _ -> error $ printf "Event '%s' not handled by fsmLevel" (show ev)
    fsmAntibioticUnlocked = error "fsmAntibioticUnlocked not implemented"
    fsmLevelComplete      = error "fsmLevelComplete not implemented"
    fsmGameOver           = error "fsmGameOver not implemented"

----------------------------------------------------------------------------------------------------
--
-- As mentioned above, the [germCumulativeTime] grows inversely proportional to
-- the size of the germ. I found that visually it works better if it grows as (1 / sqrt size)
-- but I have yet to determine why this looks so natural.
--

growGerm :: Time -> Germ -> Germ
growGerm duration g =
  let size = germSize g
      t    = germCumulativeTime g
  in g { germSize = size * germGrowthRate g
       , germCumulativeTime = (sqrt (worldMajor / size) * duration) + t}

----------------------------------------------------------------------------------------------------
--
-- Physics is reponsible for updating the [gsRender] field of the GameState.
--
physics :: Time -> GameM ()
physics duration = do
  gs <- get
  let bounds = gsBounds gs
      w2c = gsWorldToCanvas gs
  let germs = map (growGerm duration) (gsGerms gs)
  let drawOneGerm :: Germ -> Render ()
      drawOneGerm g = drawGerm
                         (germGfx g)
                         bounds
                         (worldPtToCanvasPt w2c $ germPos g)
                         (worldLenToCanvasLen w2c $ germSize g)
                         (germFooTime g)
  let render = mapM_ drawOneGerm germs
  put $ gs { gsRender = render, gsGerms = germs}
  where


----------------------------------------------------------------------------------------------------
runGameM :: GameM a -> GameState  -> IO (a, GameState)
runGameM gameM gs = evalRandIO $ runStateT gameM gs
