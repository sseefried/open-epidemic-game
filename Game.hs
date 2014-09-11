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
import FSM

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

type GameFSM = FSM (StateT GameState (Rand StdGen)) Event FSMState
-- ^ I want to write:
--     type GameFSM = FSM GameM Event FSMState
-- but I need to fully expand the type synonym for GameM Otherwise we can an annoying GHC error:
--     Type synonym ‘GameM’ should have 1 argument, but has been given none
--       In the type declaration for ‘GameFSM’



----------------------------------------------------------------------------------------------------
--
-- Finite State Machine states for this game.
--
data FSMState = FSMLevel Int -- level number
              | FSMAntibioticUnlocked
              | FSMLevelComplete
              | FSMGameOver
              | FSMQuit -- going into this state causes immediate close of program in backend
              deriving (Show, Eq, Ord)
----------------------------------------------------------------------------------------------------
data GameState = GameState { gsRender    :: Anim
                           , gsBounds    :: (Int, Int)
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
           deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
--
-- The sorts of events that can occur are dependent on the state of the FSM.
--
newGameState :: (Int,Int) -> IO GameState
newGameState bounds = do
  germAnim <- evalRandIO $ newSingleGermAnim bounds
  return $ germAnimToNewGameState bounds germAnim

germAnimToNewGameState :: (Int, Int) -> Anim -> GameState
germAnimToNewGameState bounds germAnim = GameState germAnim bounds


resetGameState :: GameM ()
resetGameState = do
  gs <- get
  germAnim <- lift $ newSingleGermAnim (gsBounds gs)
  put $ germAnimToNewGameState (gsBounds gs) germAnim

----------------------------------------------------------------------------------------------------
----
---- Advance the FSM by one step.
----
--fsm :: Event -> GameM ()
--fsm e = do
--  -- events that can occur in any FSM State
--  gs <- get
--  case e of
--    Reset -> resetGameState
--    _  -> (case gsFSMState gs of -- events that depend on current FSM State
--            FSMLevel i            -> fsmLevel i
--            FSMAntibioticUnlocked -> fsmAntibioticUnlocked
--            FSMLevelComplete      -> fsmLevelComplete
--            FSMGameOver           -> fsmGameOver
--            FSMQuit               -> return () -- do nothing
--          )
--  where
--    fsmLevel i = case e of
--      Tap (x,y) -> error "This is where you kill a germ"
--      _ -> error $ printf "Event '%s' not handled by fsmLevel" (show e)
--    fsmAntibioticUnlocked = error "fsmAntibioticUnlocked not implemented"
--    fsmLevelComplete      = error "fsmLevelComplete not implemented"
--    fsmGameOver           = error "fsmGameOver not implemented"


----------------------------------------------------------------------------------------------------
gameFSM :: GameFSM
gameFSM = FSM {
    fsmAnyStateTransitions = M.fromList [(Reset, FSMTransitions { fsmUnconditionals = []
                                                               , fsmConditionals = [condTransReset] })]
  , fsmDependentTransitions = M.fromList []

  }

condTransReset = FSMCondTrans { fsmCondTrans    = transitionReset
                               , fsmNextState    = FSMLevel 1 }
transitionReset :: GameM Bool
transitionReset = resetGameState >> return True


----------------------------------------------------------------------------------------------------
frameUpdate :: Time -> Time -> GameM ()
frameUpdate duration sinceStart = return ()

----------------------------------------------------------------------------------------------------
--
-- The game as a Finite State Machine
--
handleEvent :: FSMState -> Event -> GameM FSMState
handleEvent fsmState event = runFSM gameFSM event fsmState

----------------------------------------------------------------------------------------------------
runGameM :: GameM a -> GameState  -> IO (a, GameState)
runGameM gameM gs = evalRandIO $ runStateT gameM gs
