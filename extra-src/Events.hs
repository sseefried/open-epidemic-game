import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
import           Control.Monad
import           Text.Printf
import           System.Exit
import           Data.Map (Map)
import qualified Data.Map as M
import           GHC.Word (Word32)
import           Foreign.C.Types (CFloat)
import           Data.IORef
import           Data.Maybe

data MouseDown = MouseDown Word32 (Int, Int)
data TouchDown = TouchDown Word32 (CFloat, CFloat)

type FingerId = Integer

data PressHistory = PressHistory { phMouseDown     :: Maybe MouseDown
                                 , phTouchDowns    :: Map FingerId TouchDown
                                 }

isMobile, isDesktop :: Bool
isMobile = False
isDesktop = not isMobile

--
-- A simplified event system
--
data Event = Tap
           | Select
           | Unselect
           | Drag deriving (Show, Eq)

----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  S.init [S.InitVideo, S.InitAudio]
  window <- S.createWindow "SDL Test" (S.Position 0 0) (S.Size 300 300) wflags
  phRef  <- newIORef $ PressHistory Nothing M.empty
  mainLoop window phRef
  where
    wflags = [S.WindowShown]

----------------------------------------------------------------------------------------------------
whenJust :: Maybe a -> b -> (a -> IO b) -> IO b
whenJust mb b io = do
  case mb of
    Just a  -> io a
    Nothing -> return b

----------------------------------------------------------------------------------------------------
whenJust_ :: Maybe a -> (a -> IO ()) -> IO ()
whenJust_ mb = whenJust mb ()

----------------------------------------------------------------------------------------------------
whenLookup :: Ord k => k -> Map k a -> b -> (a -> IO b) -> IO b
whenLookup k m b io = do
  case M.lookup k m of
    Just a -> io a
    Nothing -> return b


----------------------------------------------------------------------------------------------------
mainLoop :: S.Window -> IORef PressHistory -> IO ()
mainLoop w phRef = do
  evs <- eventHandler phRef
  case evs of
    [] -> return ()
    _  -> putStrLn $ show evs
  mainLoop w phRef

----------------------------------------------------------------------------------------------------
eventHandler :: IORef PressHistory -> IO [Event]
eventHandler phRef = do
  evs <- selectEvents phRef
  mbSDLEvent <- S.pollEvent
  evs' <- case mbSDLEvent of
    Just e -> do
      when (checkForQuit e) $ exitWith ExitSuccess
      mbEv <- decodeEvent phRef e
      es'  <- eventHandler phRef -- loop until no more
      return $ mbEv `consMaybe` es'
    Nothing -> return []
  return $ evs ++ evs'

consMaybe :: Maybe a -> [a] -> [a]
mbX `consMaybe` xs = maybe xs (:xs) mbX

----------------------------------------------------------------------------------------------------
--
-- Like [modifyIORef] but takes a monadic action.
--
withIORef :: IORef a -> (a -> IO a) -> IO ()
withIORef ioRef io = do { a <- readIORef ioRef; a' <- io (seq a a); writeIORef ioRef a' }

----------------------------------------------------------------------------------------------------
maxTapDuration :: Word32
maxTapDuration = 200

----------------------------------------------------------------------------------------------------
decodeEvent :: IORef PressHistory -> S.Event -> IO (Maybe Event)
decodeEvent phRef e = do
  let t = S.eventTimestamp e
  case S.eventData e of
    -- mouse down
    S.MouseButton { S.mouseButtonState = S.Pressed
                  , S.mouseButton      = S.LeftButton
                  , S.mouseButtonAt    = S.Position x y }
                  | isDesktop -> do
      modifyIORef' phRef $ \ph -> ph { phMouseDown = Just (MouseDown t (x,y)) }
      return Nothing
    -- mouse up
    S.MouseButton { S.mouseButtonState = S.Released
                  , S.mouseButton      = S.LeftButton
                  , S.mouseButtonAt    = S.Position x y }
                  | isDesktop -> do
      ph <- readIORef phRef
      whenJust (phMouseDown ph) (Just Unselect) $ \(MouseDown t' _) -> do
        writeIORef phRef $ ph { phMouseDown = Nothing }
        return $ Just Tap
    -- mouse move
    S.MouseMotion { S.mouseMotionState = btns } | isDesktop -> do
      ph <- readIORef phRef
      let drag = Just Drag
      if S.LeftButton `elem` btns
       then do
         whenJust (phMouseDown ph) drag $ \(MouseDown t' _) -> do
           if (t - t' > maxTapDuration)
            then do
              modifyIORef phRef $ \ph -> ph { phMouseDown = Nothing }
              return drag
            else return Nothing
       else return Nothing
    -- touch down
    S.TouchFinger { S.touchFingerEvent = S.TouchFingerDown
                  , S.touchFingerID    = fingerIdL
                  , S.touchX           = x'
                  , S.touchY           = y' }
                  |  isMobile -> do
      let fingerId = fromIntegral fingerIdL
          td = TouchDown t (x',y')
      modifyIORef' phRef $ \ph -> ph { phTouchDowns = M.insert fingerId td (phTouchDowns ph) }
      return Nothing
    -- touch up
    S.TouchFinger { S.touchFingerEvent = S.TouchFingerUp
                  , S.touchFingerID    = fingerIdL
                  , S.touchX           = x'
                  , S.touchY           = y' }
                  | isMobile -> do
      ph <- readIORef phRef
      let fingerId = fromIntegral fingerIdL
      let tds = phTouchDowns ph
      whenLookup fingerId tds (Just Unselect) $ \(TouchDown t' _) -> do
        writeIORef phRef $ ph { phTouchDowns = M.delete fingerId tds }
        return $ Just Tap
    -- touch move
    S.TouchFinger { S.touchFingerEvent = S.TouchFingerMotion
                  , S.touchFingerID    = fingerIdL
                  , S.touchX           = x'
                  , S.touchY           = y'}
                  | isMobile -> do
      ph <- readIORef phRef
      let drag     = Just Drag
          fingerId = fromIntegral fingerIdL
          tds      = phTouchDowns ph
      whenLookup fingerId tds drag $ \(TouchDown t' _) -> do
           if (t - t' > maxTapDuration)
            then do
              modifyIORef phRef $ \ph -> ph { phTouchDowns = M.delete fingerId tds }
              return drag
            else return Nothing
    _ -> return Nothing
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
--
-- Checks if a certain amount of time has passed for presses and returns a "select" if this
-- time has been exceeded.
--
selectEvents :: IORef PressHistory -> IO [Event]
selectEvents phRef = do
  t  <- S.getTicks
  mbEv <- readIORef phRef >>= \ph -> do
    whenJust (phMouseDown ph) Nothing $ \(MouseDown t' (x,y)) -> do
      if ((t - t') > maxTapDuration)
       then do
         writeIORef phRef $ ph { phMouseDown = Nothing }
         return $ Just Select
       else do
         return Nothing
  evs <- readIORef phRef >>= \ph -> do
    let (selects, nonSelects) = M.mapEither tooLong (phTouchDowns ph)
        tooLong td@(TouchDown t' _ ) = if (t - t') > maxTapDuration then Left td else Right td
    writeIORef phRef $ ph { phTouchDowns = nonSelects }
    return $ map (const Select) $ M.elems selects
  return $ mbEv `consMaybe` evs

----------------------------------------------------------------------------------------------------
printEvent e = do
  let p = putStrLn $ show e
  case S.eventData e of
    S.MouseMotion { S.mouseMotionState = btns } | not (null btns) ->
      p
    _ -> return ()

----------------------------------------------------------------------------------------------------
checkForQuit e = case S.eventData e of
      S.Quit                    -> True
      _ | b <- isKeyDown e SK.Q -> b

----------------------------------------------------------------------------------------------------
isKeyDown :: S.Event -> SK.Keycode -> Bool
isKeyDown e code = case S.eventData e of
  S.Keyboard {  S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ code' _ } -> code == code'
  _ -> False