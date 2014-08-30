module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--
import Control.Monad.Random

-- friends
import Graphics

-- The game monad
type GameM a = Rand StdGen a

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
  germAnim <- runGameM $ newSingleGermAnim (w, h)
  return $ GameState FSMPlay germAnim (w, h)

--
-- The game as a Finite State Machine
--
--
game :: GameInput -> GameState -> GameM GameState
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

runGameM :: GameM a -> IO a
runGameM = evalRandIO