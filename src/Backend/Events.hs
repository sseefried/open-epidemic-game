module Backend.Events (
  -- data types
  PressHistory(..)
  -- functions
  , eventHandler
) where

import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Keycode  as SK
-- import           Control.Monad
-- import           Text.Printf
-- import           System.Exit
import           Data.Map (Map)
import qualified Data.Map as M
import           GHC.Word (Word32)
import           Foreign.C.Types (CFloat)
import           Data.IORef

-- friends
import GameEvent
import Platform
import Coordinate

data MouseDown = MouseDown Word32 (Int, Int)
data TouchDown = TouchDown Word32 (CFloat, CFloat)

type FingerId = Integer

data PressHistory = PressHistory { phMouseDown      :: Maybe MouseDown
                                 , phTouchDowns     :: Map FingerId TouchDown
                                 }


----------------------------------------------------------------------------------------------------
whenJust :: Maybe a -> b -> (a -> IO b) -> IO b
whenJust mb b io = do
  case mb of
    Just a  -> io a
    Nothing -> return b

----------------------------------------------------------------------------------------------------
_whenJust_ :: Maybe a -> (a -> IO ()) -> IO ()
_whenJust_ mb = whenJust mb ()

----------------------------------------------------------------------------------------------------
whenLookup :: Ord k => k -> Map k a -> b -> (a -> IO b) -> IO b
whenLookup k m b io = do
  case M.lookup k m of
    Just a -> io a
    Nothing -> return b


----------------------------------------------------------------------------------------------------
--mainLoop :: S.Window -> IORef PressHistory -> IO ()
--mainLoop w phRef = do
--  evs <- eventHandler phRef
--  case evs of
--    [] -> return ()
--    _  -> putStrLn $ show evs
--  mainLoop w phRef

----------------------------------------------------------------------------------------------------
-- Nothing = quit game
-- Just _  = events (maybe empty)

eventHandler :: IORef PressHistory -> BackendToWorld -> IO (Maybe [Event])
eventHandler phRef b2w = do
  evs <- selectEvents phRef b2w
  mbSDLEvent <- S.pollEvent
  mbEvs <- case mbSDLEvent of
    Just e -> do
      case checkForQuit e of
        True -> return Nothing
        False -> do
          mbEv   <- decodeEvent phRef b2w e
          mbEvs' <- eventHandler phRef b2w -- loop until no more
          return $ maybe Nothing (Just . (mbEv `consMaybe`)) mbEvs'
    Nothing -> return $ Just []
  return $ maybe Nothing (Just . (evs++)) mbEvs

consMaybe :: Maybe a -> [a] -> [a]
mbX `consMaybe` xs = maybe xs (:xs) mbX

----------------------------------------------------------------------------------------------------
maxTapDuration :: Word32
maxTapDuration = 200

----------------------------------------------------------------------------------------------------
decodeEvent :: IORef PressHistory -> BackendToWorld -> S.Event -> IO (Maybe Event)
decodeEvent phRef b2w e = do
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
      let unselect = Just $ Unselect $ backendPtToWorldPt b2w (x,y)
      whenJust (phMouseDown ph) unselect $ \(MouseDown _ pt) -> do
        writeIORef phRef $ ph { phMouseDown = Nothing }
        return $ Just $ Tap $ backendPtToWorldPt b2w pt
    -- mouse move
    S.MouseMotion { S.mouseMotionState = btns
                  , S.mouseMotionPosition = S.Position x y
                  , S.mouseMotionXRelMotion = dx
                  , S.mouseMotionYRelMotion = dy }
                  | isDesktop -> do
      ph <- readIORef phRef
      let (dx', dy') = (fromIntegral dx, fromIntegral dy)
          drag = Just $ Drag (backendPtToWorldPt b2w (x,y)) (backendPtToWorldPt b2w (x+dx', y+dy'))
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
                  , S.touchX           = x
                  , S.touchY           = y }

                  | isMobile -> do
      ph <- readIORef phRef
      let fingerId = fromIntegral fingerIdL
          tds = phTouchDowns ph
          unselect = Just $ Unselect $ backendNormPtToWorldPt b2w (x,y)
      whenLookup fingerId tds unselect $ \(TouchDown _ pt) -> do
        writeIORef phRef $ ph { phTouchDowns = M.delete fingerId tds }
        return $ Just $ Tap (backendNormPtToWorldPt b2w pt)
    -- touch move
    S.TouchFinger { S.touchFingerEvent = S.TouchFingerMotion
                  , S.touchFingerID    = fingerIdL
                  , S.touchX           = x
                  , S.touchY           = y
                  , S.touchDx          = dx
                  , S.touchDy          = dy }
                  | isMobile -> do
      ph <- readIORef phRef
      let drag     = Just $ Drag (backendNormPtToWorldPt b2w (x,y))
                                 (backendNormPtToWorldPt b2w (x+dx,y+dy))
          fingerId = fromIntegral fingerIdL
          tds      = phTouchDowns ph
      whenLookup fingerId tds drag $ \(TouchDown t' _) -> do
           if t - t' > maxTapDuration
            then do
              modifyIORef phRef $ \ph -> ph { phTouchDowns = M.delete fingerId tds }
              return drag
            else return Nothing
    _ -> return Nothing
----------------------------------------------------------------------------------------------------
--
-- Checks if a certain amount of time has passed for presses and returns a "select" if this
-- time has been exceeded.
--
selectEvents :: IORef PressHistory -> BackendToWorld -> IO [Event]
selectEvents phRef b2w = do
  t  <- S.getTicks
  mbEv <- readIORef phRef >>= \ph -> do
    whenJust (phMouseDown ph) Nothing $ \(MouseDown t' pt) -> do
      if ((t - t') > maxTapDuration)
       then do
         writeIORef phRef $ ph { phMouseDown = Nothing }
         return . Just . Select $ backendPtToWorldPt b2w pt
       else do
         return Nothing
  evs <- readIORef phRef >>= \ph -> do
    let (selectPts, nonSelects) = M.mapEither tooLong (phTouchDowns ph)
        tooLong td@(TouchDown t' pt) =
            if (t - t') > maxTapDuration
              then Left $ backendNormPtToWorldPt b2w pt
              else Right td
    writeIORef phRef $ ph { phTouchDowns = nonSelects }
    return $ map Select $ M.elems selectPts
  return $ mbEv `consMaybe` evs

----------------------------------------------------------------------------------------------------
_printEvent e = do
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