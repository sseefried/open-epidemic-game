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
-- Events
-- ~~~~~~
--
-- This game may be implemented on a variety of backends, some which have mice/touchpads,
-- some which have touch screens, some which have keyboards and some that don't. One approach
-- would be for the game logic to understand the basic concepts of 'keypress', 'mouse click',
-- 'touch event', etc, but a more abstract way to do things is simply to have a notion of
-- "game events" such as "germ squash", "quit", "continue to next level", etc.
-- It is then the backend's job to correctly translate to these events.
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
data GameState = GameState { gsRender    :: Anim
                           , gsBounds    :: (Int, Int)
                           , gsGerms     :: [GermGfx]
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
data Germ = Germ { germPos  :: (Double, Double)
                 , germSize :: Double }

----------------------------------------------------------------------------------------------------
--
-- The sorts of events that can occur are dependent on the state of the FSM.
--
newGameState :: (Int, Int) -> IO GameState
newGameState bounds = do
  g <- evalRandIO randomGermGfx
  return $ GameState (const $ return ()) bounds [g]

resetGameState :: GameM ()
resetGameState = do
  gs <- get
  g <- lift $ randomGermGfx
  put $ GameState (const $ return ()) (gsBounds gs) [g]

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
      Physics duration -> physics  >> return fsmState
      _ -> error $ printf "Event '%s' not handled by fsmLevel" (show ev)
    fsmAntibioticUnlocked = error "fsmAntibioticUnlocked not implemented"
    fsmLevelComplete      = error "fsmLevelComplete not implemented"
    fsmGameOver           = error "fsmGameOver not implemented"

----------------------------------------------------------------------------------------------------
--
-- Physics is reponsible for updating the [gsRender] field of the GameState.
--
physics :: GameM ()
physics = do
  gs <- get
  let bounds = gsBounds gs
  let render = \t -> mapM_ (\g -> drawGerm g bounds (R2 50 100) 100 t) (gsGerms gs)
  put $ gs { gsRender = render}

----------------------------------------------------------------------------------------------------
runGameM :: GameM a -> GameState  -> IO (a, GameState)
runGameM gameM gs = evalRandIO $ runStateT gameM gs
