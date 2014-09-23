{-# LANGUAGE GADTs, RankNTypes #-}
module GameM (
  -- opaque types
  GameM, HipM,
  -- functions
  -- GameM smart constructors
  getRandom,
  evalRand,
  get,
  modify,
  put,
  newHipSpace,
  runHipM,
  -- HipM smart constructors
  hipStep,
  addHipCirc,
  getHipCircPos,
  setHipCircRadius,
  removeHipCirc,
  -- to run the GameM monad
  runGameM

) where

--
-- Define a game monad as a free monad.
--

import           Control.Monad.Random (Random, Rand, StdGen)
import qualified Control.Monad.Random as R
import           Control.Monad.Free

import qualified Physics.Hipmunk as H
import qualified Physics.Hipmunk.Unsafe as H
import qualified Data.StateVar as H
import           Data.IORef

-- friends
import Types


data GameScript next =
    forall a. Random a => GetRandom    (a,a) (a -> next)
  | forall a.             EvalRand     (Rand StdGen a) (a -> next)
  |                       Get          (GameState -> next)
  |                       Modify       (GameState -> GameState) next
  |                       Put          GameState next
  |                       NewHipSpace  (H.Space -> next)
  | forall a.             RunHipM      H.Space (HipM a) (a -> next)

data HipScript next =
    HipStep          Double next
  | AddHipCirc       Double R2 (HipCirc -> next)
  | GetHipCircPos    HipCirc (R2 -> next)
  | SetHipCircRadius HipCirc Double next
  | RemoveHipCirc    HipCirc next

type GameM = Free GameScript
type HipM = Free HipScript


----------------------------------------------------------------------------------------------------
-- Smart constructors for GameM

getRandom :: Random a => (a,a) -> GameM a
getRandom bds = Impure (GetRandom bds Pure)

evalRand :: Rand StdGen a -> GameM a
evalRand rand = Impure (EvalRand rand Pure)

get :: GameM GameState
get = Impure (Get Pure)

modify :: (GameState -> GameState) -> GameM ()
modify f = Impure (Modify f (Pure ()))

put :: GameState -> GameM ()
put gs = Impure (Put gs (Pure ()))

newHipSpace :: GameM H.Space
newHipSpace = Impure (NewHipSpace Pure)

runHipM :: H.Space -> HipM a -> GameM a
runHipM space hipM = Impure (RunHipM space hipM Pure)

----------------------------------------------------------------------------------------------------
-- Smart constructors for HipM
hipStep :: Double -> HipM ()
hipStep dt = Impure (HipStep dt (Pure ()))

addHipCirc :: Double -> R2 -> HipM HipCirc
addHipCirc r pos = Impure (AddHipCirc r pos Pure)

getHipCircPos :: HipCirc -> HipM R2
getHipCircPos c = Impure (GetHipCircPos c Pure)

setHipCircRadius :: HipCirc -> Double -> HipM ()
setHipCircRadius c r = Impure (SetHipCircRadius c r (Pure ()))

removeHipCirc :: HipCirc -> HipM ()
removeHipCirc c = Impure (RemoveHipCirc c (Pure ()))

----------------------------------------------------------------------------------------------------



runGameM :: GameState -> GameM a -> IO (a, GameState)
runGameM = runGameMGen runHipMIO

runGameMGen ::(forall a. H.Space -> HipM a -> IO a) -> GameState -> GameM a -> IO (a, GameState)
runGameMGen hipI gs gameM = do
  gsRef <- newIORef gs
  a <- go gsRef gameM
  gs' <- readIORef gsRef
  return (a, gs')

  where
    go :: IORef GameState -> GameM a -> IO a
    go gsRef p = do
      let go' = go gsRef
      case p of
        (Impure (GetRandom bds f))      -> R.randomRIO bds >>= go' . f
        (Impure (EvalRand rand f))      -> R.evalRandIO rand >>= go' . f
        (Impure (Get f))                -> readIORef gsRef >>= go' . f
        (Impure (Modify f p'))          -> modifyIORef gsRef f >> go' p'
        (Impure (Put gs p'))            -> writeIORef gsRef gs >> go' p'
        (Impure (NewHipSpace f))        -> H.newSpace >>= go' . f
        (Impure (RunHipM space hipM f)) -> hipI space hipM >>= go' . f
        (Pure x)                        -> return x

runHipMIO :: H.Space -> HipM a -> IO a
runHipMIO space = go
  where
    go :: HipM a -> IO a
    go p = case p of
      (Impure (HipStep dt p'))                 -> H.step space dt >> go p
      (Impure (AddHipCirc r pos f))            -> runAddHipCirc r pos >>= go . f
      (Impure (GetHipCircPos hipCirc f))       -> runGetHipCircPos hipCirc >>= go . f
      (Impure (SetHipCircRadius hipCirc r p')) -> runSetHipCircRadius hipCirc r >> go p'
      (Impure (RemoveHipCirc hipCirc p'))      -> runRemoveHipCirc hipCirc >> go p'
      Pure x                                   -> return x

    runAddHipCirc :: Double -> R2 -> IO HipCirc
    runAddHipCirc r (R2 x y) = do
      let pos = H.Vector x y
      b <- H.newBody 1 H.infinity
      H.spaceAdd space b
      H.position b H.$= pos
      s <- H.newShape b (H.Circle r) (H.Vector 0 0) -- (0,0) offset
      H.spaceAdd space s
      return $ HipCirc s

    runGetHipCircPos :: HipCirc -> IO R2
    runGetHipCircPos (HipCirc s) = do
      H.Vector x y <- H.get $ H.position $ H.body s
      return $ R2 x y

    runSetHipCircRadius :: HipCirc -> Double -> IO ()
    runSetHipCircRadius (HipCirc s) r = do
      H.unsafeShapeRedefine s (H.Circle r) (H.Vector 0 0) -- (0,0) offset

    runRemoveHipCirc :: HipCirc -> IO ()
    runRemoveHipCirc (HipCirc s) = do
      H.spaceRemove space (H.body s)
      H.spaceRemove space s
