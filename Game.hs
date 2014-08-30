module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--

-- friends
import Graphics

data GameInput = GameInput { giDuration   :: Time
                           , giSinceStart :: Time
                           , giEvents     :: [Event] }

data FSMState = FSMQuit
              | FSMPlay

data GameState = GameState { gsFSMState  :: FSMState
                           , gsRender    :: Anim }


type KeyCode = Int

data Event = KeyDown KeyCode
           | KeyUp   KeyCode
           | Quit deriving (Show,Eq)

newGameState :: Int -> Int -> IO GameState
newGameState screenHeight screenWidth = do
  germAnim <- newGermAnim screenHeight screenWidth
  return $ GameState FSMPlay germAnim

--
-- The game as a Finite State Machine
--
game :: GameInput -> GameState -> GameState
game gi gs = if Quit `elem` giEvents gi then gs { gsFSMState = FSMQuit } else gs
