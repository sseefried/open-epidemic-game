{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module MainSDL where


-- friends
import Game
import Backend.SDL as B

#ifdef SDL_BACKEND
import qualified Graphics.UI.SDL as S

foreign export ccall "haskell_mainSDL" mainSDL :: IO ()
#endif

screenWidth, screenHeight :: Int
screenWidth = 512
screenHeight = 512

w, h :: Double
w = fromIntegral screenWidth
h = fromIntegral screenHeight


#ifdef SDL_BACKEND
--
-- You need to do a few things to write a Haskell SDL application
-- 1. Add the LANGUAGE ForeignFunctionInterface pragma
-- 2. foreign export ccall "haskell_mainSDL" <your_main> :: IO ()
-- 3. Compile with GHC with "-main-is" option.
-- 4. Link against mainc.o
--

mainSDL :: IO ()
mainSDL = S.withInit [S.InitVideo] $ main
#endif

main :: IO ()
main = do
  gameState <- runGameM $ newGameState (screenWidth, screenHeight)
  besRef   <- B.initialize "Epidemic" screenWidth screenHeight gameState
  B.mainLoop besRef handleEvent frameUpdate