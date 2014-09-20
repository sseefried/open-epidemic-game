{-# LANGUAGE GADTs, LiberalTypeSynonyms #-}
module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--

-- system imports
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import           Control.Applicative
import           Text.Printf
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Physics.Hipmunk as H

-- for pure Hipmunk. TODO factor out
import qualified Physics.Hipmunk as H
import qualified Physics.Hipmunk.Unsafe as H
import qualified Data.StateVar as H
import           Data.Functor.Identity
import           System.IO.Unsafe -- for pure Hipmunk... see below

-- friends
import Graphics

----------------------------------------------------------------------------------------------------
--
-- Game constants
--
worldWidth, worldHeight, worldMajor :: Double
worldWidth  = 100
worldHeight = 100
worldMajor = max worldWidth worldHeight

initialGermSize :: Double
initialGermSize = worldMajor / 30

initialGermSizeVariance :: Double
initialGermSizeVariance = worldMajor / 150

doublingPeriod :: Double
doublingPeriod = 3

doublingPeriodVariance :: Double
doublingPeriodVariance = 0.5

resistanceIncrease :: Double
resistanceIncrease = 1.1
----------------------------------------------------------------------------------------------------
--
-- World co-ordinates vs. canvas co-ordinates
--
-- People may play this game at a variety of different resolutions. The co-ordinate system
-- this is in is known as the canvas co-ordinates. For the purposes of the physics the
-- co-ordindate system will remain fixed.
--
-- exported
data R2 = R2 Double Double deriving (Show, Eq, Ord)

--
-- The canvas might not have the same aspect ratio as the world, in which case
-- we ensure there will be some portions of the canvas that won't be drawn to.
--

data WorldToCanvas = WorldToCanvas { worldPtToCanvasPt :: R2 -> CairoPoint
                                   , worldLenToCanvasLen :: Double -> Double }




--
-- Let aspect ratio be width/height. Let aspect ration of the world be W and the aspect ratio of
-- the  canvas be C. If W > C then there will margins at the top and bottom of C that are not drawn
-- to.  If W < C then there will be margins on the left and right that will not be drawn to.
--
worldToCanvas :: (Int, Int) -> WorldToCanvas
worldToCanvas (w,h) =
  WorldToCanvas { worldPtToCanvasPt   = \(R2 x y) -> (w'/2 + scale*x, h'/2 - scale*y)
                , worldLenToCanvasLen = \len -> scale * len  }
  where
    w' = fromIntegral w
    h' = fromIntegral h
    minor = min w' h'
    scale = minor / worldMajor



----------------------------------------------------------------------------------------------------
--
--
-- The [germCumulativeTime] field is used in animating the germs. This is the value
-- that is passed to the [drawGerm] function in the Graphics module.
-- This value grows inversely proportional to the size of the germ because I've found
-- that small germs need to animate quicker to look like they are moving at all. See function
-- [growGerm].
--
data Germ = Germ { germMultiplyAt     :: Time
                 , germSizeFun        :: Time -> Double
                 , germHipCirc        :: HipCirc
                 , germPos            :: R2 -- cached pos
                 , germGfx            :: GermGfx
                 , germCumulativeTime :: Time
                 , germAnimTime       :: Time
                 }


--
-- Given an initial size [initSize] and a time that the germ should multiply at [multiplyAt]
-- (now being twice its original size) returns a function that given a time [t] returns
-- the current germ size.
--
--
germSizeFunForParams :: Double -> Double -> (Time -> Double)
germSizeFunForParams initSize multiplyAt t = initSize * (2**(t/multiplyAt))

-- TODO: Remove magic numbers
-- precondition: position of HipCirc must be the same as [pos]
createGerm :: RandomGen g => Double -> R2 -> HipCirc -> Rand g Germ
createGerm initSize pos hipCirc = do
  gfx        <- randomGermGfx
  multiplyAt <- randomValWithVariance doublingPeriod  doublingPeriodVariance
  return $ Germ { germMultiplyAt        = multiplyAt
                , germSizeFun           = germSizeFunForParams initSize multiplyAt
                , germHipCirc           = hipCirc
                , germPos               = pos
                , germGfx               = gfx
                , germCumulativeTime    = 0
                , germAnimTime          = 0
                }

randomValWithVariance :: RandomGen g => Double -> Double -> Rand g Double
randomValWithVariance val variance = (val+) <$> getRandomR (-variance, variance)

----------------------------------------------------------------------------------------------------
--
-- Events
-- ~~~~~~
--
-- This game may be implemented on a variety of backends, some which have mice/touchpads,
-- some which have touch screens, some which have keyboards and some that don't. One approach
-- would be for the game logic to understand the basic concepts of 'keypress', 'mouse click',
-- 'touch event', etc, but a more abstract way to do things is simply to have a notion of
-- "game events" such as "germ squash", "quit", "continue to next level", etc.
-- It is then the backend's job to correctly translate its events to these game events.
--
-- This actually saves me a lot of work. For instance, were we to have a notion of 'keypress'
-- in the game logic then you would have to account for every different kind of key that could
-- be pressed, which is not only a large number of keys but would likely be different between
-- the different backends. (Some would leave out certain keys not common on all keyboards, some
-- would have them. I'd have to choose a subset that worked for all of them, etc.)
--


----------------------------------------------------------------------------------------------------
-- The game monad
type GameM a = StateT GameState (Rand StdGen) a

----------------------------------------------------------------------------------------------------
--
-- Finite State Machine states for this game.
--
data FSMState = FSMLevel Int -- level number
              | FSMPlayingLevel
              | FSMAntibioticUnlocked
              | FSMLevelComplete
              | FSMGameOver
              deriving (Show, Eq, Ord)

type GermId = Int

----------------------------------------------------------------------------------------------------
data GameState = GameState { gsRender        :: Render ()
                           , gsBounds        :: (Int, Int)
                           , gsGerms         :: Map GermId Germ
                           , gsWorldToCanvas :: WorldToCanvas
                           , gsNextGermId    :: GermId
                           , gsHipState      :: HipState
                           }

----------------------------------------------------------------------------------------------------
{-
TODO: I would really like it if there was some (fairly easy) way of associating a particular
data type with each constructor of the FSM. As it stands I could easily make a mistake in my
code and have backend code that returned an event that wasn't handled by a particular FSM
state. At this point these errors can only be caught at run-time.
-}
data Event = Tap R2 -- location at which tap occurred.
           | TapAnywhere          -- tap occurred but anywhere.
           | NextFrame
           | Reset
           | Physics Time -- how much time the last frame took
           deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
--
-- The sorts of events that can occur are dependent on the state of the FSM.
--
newGameState :: (Int, Int) -> IO GameState
newGameState bounds = do
  return $ initGameState bounds []

resetGameState :: GameM ()
resetGameState = do
  gs <- get
  put $ initGameState (gsBounds gs) []

initGameState :: (Int,Int) -> [Germ] -> GameState
initGameState bounds germs =
  GameState
    (return ())
    bounds
    germMapList
    (worldToCanvas bounds)
    (length germs)
    (newHipState ())
  where
    germMapList = M.fromList $ zip [0..] germs
----------------------------------------------------------------------------------------------------
--
-- The game as a Finite State Machine
--
handleEvent :: FSMState -> Event -> GameM FSMState
handleEvent fsmState ev = do
  -- events that can occur in any FSM State
  gs <- get
  case ev of
    Reset   -> resetGameState >> (return $ FSMLevel 1)
    _  -> (case fsmState of -- events that depend on current FSM State
            FSMLevel i            -> fsmLevel i
            FSMPlayingLevel       -> fsmPlayingLevel
            FSMAntibioticUnlocked -> fsmAntibioticUnlocked
            FSMLevelComplete      -> fsmLevelComplete
            FSMGameOver           -> fsmGameOver
          )
  where
    fsmLevel i = do
      gs <- get
      -- create n germs randomly
      let bounds = gsBounds gs
      germs <- replicateM i $ do
                 x <- lift $ getRandomR (-worldWidth/8, worldWidth/8)
                 y <- lift $ getRandomR (-worldHeight/8, worldHeight/8)
                 initSize <- lift $ randomValWithVariance initialGermSize initialGermSizeVariance
                 hc <- runOnHipState $ addHipCirc initSize (x,y)
                 lift $ createGerm initSize (R2 x y) hc
      put $ gs { gsGerms = M.fromList (zip [0..] germs), gsNextGermId = length germs }
      return $ FSMPlayingLevel

    fsmPlayingLevel = case ev of
      Tap (R2 x y)        -> error $ "This is where you kill a germ" ++ show (x,y)
      Physics duration -> physics duration >> return fsmState
      _ -> error $ printf "Event '%s' not handled by fsmLevel" (show ev)
    fsmAntibioticUnlocked = error "fsmAntibioticUnlocked not implemented"
    fsmLevelComplete      = error "fsmLevelComplete not implemented"
    fsmGameOver           = error "fsmGameOver not implemented"


--
-- Runs in the HipM monad. Gets the new state. Sets it.
--
runOnHipState :: HipM a -> GameM a
runOnHipState hipM = do
  gs <- get
  let (a, hs) = runState hipM (gsHipState gs)
  put gs { gsHipState = hs}
  return a


----------------------------------------------------------------------------------------------------
--
-- As mentioned above, the [germCumulativeTime] grows inversely proportional to
-- the size of the germ. I found that visually it works better if it grows as (1 / sqrt size)
-- but I have yet to determine why this looks so natural.
--
growGerm :: Time -> GermId -> GameM ()
growGerm duration germId = do
  whenGerm germId $ \gs g -> do
    let animT  = germAnimTime g
        t      = germCumulativeTime g
        hc     = germHipCirc g
        sz     = germSizeFun g t
    -- New pos of germ from physics engine
    (x,y) <- runOnHipState $ getHipCircPos hc
    if (t > germMultiplyAt g)
    then do
      -- TODO mutate the germ
      dx <- lift $ getRandomR (0,sz)
      dy <- lift $ getRandomR (0,sz)
      let i       = gsNextGermId gs
          hc      = germHipCirc g
          (x',y') = (x + dx, y + dy)
      hc' <- runOnHipState $ do
        setHipCircRadius hc (sz/2)
        addHipCirc (sz/2) (x',y')
      ng <- lift $ createGerm (sz/2) (R2 x' y') hc'
      insertGerm i ng -- insert new germ
      -- update first germ
      insertGerm germId $ g { germCumulativeTime = 0, germPos = R2 x y }
      modify $ \gs -> gs { gsNextGermId = i + 1 }
    else do
      runOnHipState $ setHipCircRadius hc sz -- update the size in the physics
      let g' = g { germCumulativeTime = duration + t
                 , germPos            = R2 x y
                 , germAnimTime       = (sqrt (worldMajor / sz) * duration) + animT }
      insertGerm germId g'

----------------------------------------------------------------------------------------------------
whenGerm :: GermId -> (GameState -> Germ -> GameM ()) -> GameM ()
whenGerm germId f = do
  gs <- get
  case M.lookup germId (gsGerms gs) of
    Just germ -> f gs germ
    Nothing   -> return ()

insertGerm :: GermId -> Germ -> GameM ()
insertGerm germId germ = modify $ \gs -> gs { gsGerms = M.insert germId germ (gsGerms gs) }

----------------------------------------------------------------------------------------------------
--
-- Physics is reponsible for updating the [gsRender] field of the GameState.
--
physics :: Time -> GameM ()
physics duration = do
  gs <- get
  let bounds = gsBounds gs
      w2c = gsWorldToCanvas gs
  mapM_ (growGerm duration) (M.keys $ gsGerms gs)
  runOnHipState $ replicateM 10 (hipStep (duration/10))
  let drawOneGerm :: Germ -> Render ()
      drawOneGerm g = drawGerm
                         (germGfx g)
                         bounds
                         (worldPtToCanvasPt w2c $ germPos g)
                         (worldLenToCanvasLen w2c $ germSizeFun g (germCumulativeTime g))
                         (germAnimTime g)
  modify $ \gs -> let render = drawBackground (gsBounds gs) >> mapM_ drawOneGerm (M.elems $ gsGerms gs)
                  in  gs { gsRender = render }

----------------------------------------------------------------------------------------------------
runGameM :: GameM a -> GameState  -> IO (a, GameState)
runGameM gameM gs = evalRandIO $ runStateT gameM gs


----------------------------------------------------------------------------------------------------
-- TODO: Factor out

-- A small _pure_ wrapper for Hipmunk

data HipCirc  = HipCirc  { _hipCircShape  :: H.Shape }
data HipState = HipState H.Space
type HipM a   = State HipState a

toHipM :: (H.Space -> IO a) -> HipM a
toHipM f = StateT $ \hs@(HipState space) ->
                  let v = unsafePerformIO $ f space
                  in  v `seq` (Identity (v,hs)) -- use of 'seq' here very important

hipStep :: Double -> HipM ()
hipStep dt = toHipM $ \space -> H.step space dt

newHipState :: () -> HipState
newHipState () = HipState (unsafePerformIO $ H.initChipmunk >> H.newSpace)

addHipCirc :: Double -> (Double, Double) -> HipM HipCirc
addHipCirc r (x,y) = toHipM $ \space -> do
  let pos = H.Vector x y
  b <- H.newBody 1 H.infinity
  H.spaceAdd space b
  H.position b H.$= pos
  s <- H.newShape b (H.Circle r) (H.Vector 0 0) -- (0,0) offset
  H.spaceAdd space s
  return $ HipCirc s

getHipCircPos :: HipCirc -> HipM (Double, Double)
getHipCircPos (HipCirc s) = toHipM $ \space -> do
  H.Vector x y <- H.get $ H.position $ H.body s
  return (x,y)

setHipCircRadius :: HipCirc -> Double -> HipM ()
setHipCircRadius (HipCirc s) r = toHipM $ \_ -> do
  H.unsafeShapeRedefine s (H.Circle r) (H.Vector 0 0) -- (0,0) offset

removeHipCirc :: HipCirc -> HipM ()
removeHipCirc (HipCirc s) = toHipM $ \space -> do
  H.spaceRemove space (H.body s)
  H.spaceRemove space s


