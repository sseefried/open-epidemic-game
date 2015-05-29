{-# LANGUAGE GADTs, RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module GameM (
  -- opaque types
  GameM, UTCTime,
  -- functions
  -- GameM smart constructors
  getRandom,
  evalRand,
  get,
  modify,
  put,
  printStr,
  printStrLn,
  getTime,
  timeSince,
  newHipSpace,
  runHipM,
  -- helpers
  runOnHipState,
  germPos,

  -- to run the GL monad
  runGLM,
  -- to run the GameM monad
  runGameM,
  --
  runGLMIO -- required in SDL/OpenGL backend

) where

--
-- Define a game monad as a free monad.
--

import           Control.Monad.Random (Random, Rand, StdGen)
import qualified Control.Monad.Random as R
import           Control.Monad.Free
import qualified Physics.Hipmunk as H
import           Data.IORef
import           Data.Time (getCurrentTime, diffUTCTime, UTCTime)

-- friends
import Types
import Game.Types
import GraphicsGL.GLM
import Platform
import HipM

data GameScript next =
    forall a. Random a => GetRandom    (a,a) (a -> next)
  | forall a.             EvalRand     (Rand StdGen a) (a -> next)
  |                       Get          (GameState -> next)
  |                       Modify       (GameState -> GameState) next
  |                       Put          !GameState next
  |                       PrintStr     !String next
  |                       GetTime      (UTCTime -> next)
  |                       TimeSince    UTCTime (Double -> next)
  | forall a.             RunGLM       (GLM a) (a -> next)
  |                       NewHipSpace  (HipSpace -> next)
  | forall a.             RunHipM      !HipSpace (HipM a) (a -> next)



type GameM = Free GameScript
----------------------------------------------------------------------------------------------------
-- Functor instances. I wish I didn't have to write these manually but the
-- existential types get in the way.

instance Functor GameScript where
  fmap f _gs = case _gs of
    GetRandom a g  -> GetRandom a (f . g)
    EvalRand a g   -> EvalRand a (f . g)
    Get g          -> Get (f . g)
    Modify a gs'   -> Modify a (f gs')
    Put a gs'      -> Put a (f gs')
    PrintStr a gs' -> PrintStr a (f gs')
    GetTime g      -> GetTime (f . g)
    TimeSince a g  -> TimeSince a (f . g)
    RunGLM a g     -> RunGLM a (f . g)
    NewHipSpace g  -> NewHipSpace (f . g)
    RunHipM a b g  -> RunHipM a b (f . g)



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

printStr :: String -> GameM ()
printStr s = Impure (PrintStr s (Pure ()))

printStrLn :: String -> GameM ()
printStrLn s = Impure (PrintStr (s++"\n") (Pure ()))

getTime :: GameM UTCTime
getTime = Impure (GetTime Pure)

timeSince :: UTCTime -> GameM Double
timeSince t = Impure (TimeSince t Pure)

newHipSpace :: GameM HipSpace
newHipSpace = Impure (NewHipSpace Pure)

runGLM :: GLM a -> GameM a
runGLM glm' = Impure (RunGLM glm' Pure)

runHipM :: HipSpace -> HipM a -> GameM a
runHipM space hipM = Impure (RunHipM space hipM Pure)


----------------------------------------------------------------------------------------------------
-- Helper functions
--
-- Runs in the HipM monad. Gets the new state. Sets it.
--
runOnHipState :: HipM a -> GameM a
runOnHipState hipM = do
  gs <- get
  runHipM (gsHipState gs) hipM

germPos :: Germ -> GameM R2
germPos g = runOnHipState $ getHipCircPos (germHipCirc g)


----------------------------------------------------------------------------------------------------

runGameM :: GfxState -> GameState -> GameM b -> IO (b, GameState)
runGameM glsls gs gameM = do
  gsRef <- newIORef gs
  a <- go gsRef gameM
  gs' <- readIORef gsRef
  return (a, gs')
  where
    go :: IORef GameState -> GameM a -> IO a
    go gsRef _p = do
      let go' = go gsRef
      case _p of
        (Impure (GetRandom bds f))      -> R.randomRIO bds >>= go' . f
        (Impure (EvalRand rand f))      -> R.evalRandIO rand >>= go' . f
        (Impure (Get f))                -> readIORef gsRef >>= go' . f
        (Impure (Modify f p))           -> modifyIORef gsRef f >> go' p
        (Impure (Put gs' p))            -> writeIORef gsRef gs' >> go' p
        (Impure (PrintStr s p))         -> debugLog s >> go' p
        (Impure (GetTime f))            -> getCurrentTime >>= go' . f
        (Impure (TimeSince t f))        -> timeSince' t >>= go' . f
        (Impure (NewHipSpace f))        -> H.initChipmunk >> H.newSpace >>= go' . f
        (Impure (RunHipM space hipM f)) -> runHipMIO space hipM >>= go' . f
        (Impure (RunGLM glm' f))        -> runGLMIO glsls glm' >>= go' . f
        (Pure x)                        -> return x
    timeSince' :: UTCTime -> IO Double
    timeSince' t = do
      t' <- getCurrentTime
      return . realToFrac $ diffUTCTime t' t

----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
