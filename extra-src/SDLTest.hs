
import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import           Control.Monad
import           Text.Printf
import           System.Exit

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  S.init [S.InitVideo, S.InitAudio]
  window  <- S.createWindow "SDL Test" (S.Position 0 0) (S.Size 800 600) wflags
  mainLoop window
  where
    wflags = [S.WindowShown]

----------------------------------------------------------------------------------------------------
mainLoop :: S.Window -> IO ()
mainLoop w = do
  S.delay 16
  eventHandler
  mainLoop w

----------------------------------------------------------------------------------------------------
eventHandler :: IO ()
eventHandler = do
  mbSDLEvent <- S.pollEvent
  case mbSDLEvent of
    Just e -> do
      when (checkForQuit e) $ exitWith ExitSuccess
      printEvent e
    Nothing -> return ()
----------------------------------------------------------------------------------------------------
printEvent e = do
  putStrLn $ show e


----------------------------------------------------------------------------------------------------
checkForQuit e = case S.eventData e of
      S.Quit                    -> True
      _ | b <- isKeyDown e SK.Q -> b

----------------------------------------------------------------------------------------------------
isKeyDown :: S.Event -> SK.Keycode -> Bool
isKeyDown e code = case S.eventData e of
  S.Keyboard {  S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ code' _ } -> code == code'
  _ -> False