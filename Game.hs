module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--
import Control.Monad.Random

-- friends
import Graphics

data GameInput = GameInput { giDuration   :: Time
                           , giSinceStart :: Time
                           , giEvents     :: [Event] }

data FSMState = FSMQuit
              | FSMPlay

data GameState = GameState { gsFSMState  :: FSMState
                           , gsRender    :: Anim
                           , gsBounds    :: (Int, Int)
                           }


type KeyCode = Int

data Event = KeyDown KeyCode
           | KeyUp   KeyCode
           | Quit deriving (Show,Eq)

newGameState :: (Int,Int) -> IO GameState
newGameState (w, h) = do
  germAnim <- evalRandIO $ newSingleGermAnim (w, h)
  return $ GameState FSMPlay germAnim (w, h)

--
-- The game as a Finite State Machine
--
-- FIXME: Is the return type going to have to be IO GameState?
-- we will need to generate new animations.
--
game :: GameInput -> GameState -> Rand StdGen GameState
game gi gs = if Quit `elem` es
             then return $ gs { gsFSMState = FSMQuit }
             else if not . null . filter isKeyDown $ es
                  then do
                    g <- newSingleGermAnim (gsBounds gs)
                    return $ gs { gsRender = g}
                  else return gs
  where
    es = giEvents gi
    isKeyDown (KeyDown _) = True
    isKeyDown _           = False