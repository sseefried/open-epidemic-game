module Game where

-- friends
import Germ

data GameInput = GameInput { giDuration   :: Time
                           , giSinceStart :: Time
                           , giEvents     :: [Event] }

data FSMState = FSMQuit
              | FSMPlay


data GameState = GameState { gsFSMState  :: FSMState
                           , gsRender    :: Anim }

game :: GameInput -> GameState -> GameState
game gi gs = if Quit `elem` giEvents gi then gs { gsFSMState = FSMQuit } else gs

type KeyCode = Int

data Event = KeyDown KeyCode
           | KeyUp   KeyCode
           | Quit deriving (Show,Eq)

newGameState :: Int -> Int -> IO GameState
newGameState screenHeight screenWidth = do
  germAnim <- newGermAnim screenHeight screenWidth
  return $ GameState FSMPlay germAnim
