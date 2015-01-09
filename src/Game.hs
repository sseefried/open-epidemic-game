{-# LANGUAGE GADTs, LiberalTypeSynonyms #-}
module Game where

--
-- This module will contain all the game mechanics but will not concern itself
-- with issues of rendering.
--

-- system imports
import           Control.Monad.Random hiding (getRandom, evalRand)
import           Control.Monad (replicateM)
import           Control.Applicative
import           Text.Printf
import qualified Data.Map as M

-- friends
import Types
import GameM
import Graphics   -- vector graphics
import GraphicsGL -- GL graphics
----------------------------------------------------------------------------------------------------
--
-- Game constants
--
worldWidth, worldHeight, worldMajor :: Double
worldWidth  = 100
worldHeight = 100
worldMajor = max worldWidth worldHeight
worldMinor = min worldWidth worldHeight

worldWidthI, worldHeightI :: Int
worldWidthI = floor worldWidth
worldHeightI = floor worldHeight

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

-- Maximum number of germs before you are "infected"
maxGerms :: Int
maxGerms = 50

----------------------------------------------------------------------------------------------------
--
-- Let aspect ratio be width/height. Let aspect ratio of the world be W and the aspect ratio of
-- the canvas be C. If W > C then there will margins at the top and bottom of C that are not drawn
-- to. If W < C then there will be margins on the left and right that will not be drawn to.
--
worldToCanvas :: (Int, Int) -> WorldToCanvas
worldToCanvas (w,h) =
  WorldToCanvas { worldPtToCanvasPt   = \(R2 x y) -> (w'/2 + scale*x, h'/2 - scale*y)
                , worldLenToCanvasLen = (scale*)  }
  where
    w' = fromIntegral w
    h' = fromIntegral h
    minor = min w' h'
    scale = minor / worldMajor

----------------------------------------------------------------------------------------------------
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
createGerm :: Double -> R2 -> HipCirc -> GameM Germ
createGerm initSize pos hipCirc = do
  gfx        <- evalRand $ randomGermGfx
  multiplyAt <- evalRand $ randomValWithVariance doublingPeriod  doublingPeriodVariance
  germGL     <- runGLM $ germGfxToGLFun gfx
  return $ Germ { germMultiplyAt     = multiplyAt
                , germSizeFun        = germSizeFunForParams initSize multiplyAt
                , germHipCirc        = hipCirc
                , germPos            = pos
                , germGfx            = gfx
                , germGL             = germGL
                , germCumulativeTime = 0
                , germAnimTime       = 0
                }

randomValWithVariance :: RandomGen g => Double -> Double -> Rand g Double
randomValWithVariance val variance = (val+) <$> getRandomR (-variance, variance)


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

{-
TODO: I would really like it if there was some (fairly easy) way of associating a particular
data type with each constructor of the FSM. As it stands I could easily make a mistake in my
code and have backend code that returned an event that wasn't handled by a particular FSM
state. At this point these errors can only be caught at run-time.
-}
data Event = Tap R2       -- location at which tap occurred.
           | TapAnywhere  -- tap occurred but anywhere.
           | Reset
           | Physics Time -- how much time the last frame took
           deriving (Show, Eq, Ord)

----------------------------------------------------------------------------------------------------
--
-- The sorts of events that can occur are dependent on the state of the FSM.
--
-- FIXME: Remove the need for this IO function.
--
newGameState :: (Int, Int) -> IO GameState
newGameState bounds = do
  return $ initGameState bounds (error "Hipmunk space not defined yet") []

resetGameState :: GameM ()
resetGameState = do
  gs <- get
  hipSpace <- newHipSpace
  addBoundsToHipSpace hipSpace
  put $ initGameState (gsBounds gs) hipSpace []

addBoundsToHipSpace :: HipSpace -> GameM ()
addBoundsToHipSpace hipSpace = runHipM hipSpace $ do
  -- bottom
  addHipStaticPoly [R2 (-w) (-h), R2 w (-h), R2 w (-h - d), R2 (-w) (-h - d)]
  -- top
  addHipStaticPoly [R2 (-w) (h+d), R2 w (h+d), R2 w h, R2 (-w) h]
  -- left
  addHipStaticPoly [R2 (-w-d) (-h), R2 (-w-d) h, R2 (-w) h, R2 (-w) (-h)]
  -- right
  addHipStaticPoly [R2 (w) (-h), R2 (w) h, R2 (w+d) h, R2 (w+d) (-h)]
  where
    w = worldWidth/2
    h = worldHeight/2
    d  = 0.2 * w

initGameState :: (Int,Int) -> HipSpace -> [Germ] -> GameState
initGameState bounds hipSpace germs =
  GameState
    (return ())
    bounds
    germMapList
    (worldToCanvas bounds)
    (length germs)
    hipSpace
    []
    1 -- current level
  where
    germMapList = M.fromList $ zip [0..] germs
----------------------------------------------------------------------------------------------------
--
-- The game as a Finite State Machine
--
handleEvent :: FSMState -> Event -> GameM FSMState
handleEvent fsmState ev = do
  -- events that can occur in any FSM State
  case ev of
    Reset   -> resetGameState >> (return $ FSMLevel 1)
    _  -> (case fsmState of -- events that depend on current FSM State
             FSMLevel i            -> fsmLevel i
             FSMPlayingLevel       -> fsmPlayingLevel
             FSMAntibioticUnlocked -> fsmAntibioticUnlocked
             FSMLevelComplete      -> fsmLevelComplete
             FSMGameOver           -> fsmGameOver)
  where
    fsmLevel i = do
      resetGameState
      -- create [n] germs randomly
      germs <- replicateM i $ do
                 x <- getRandom (-worldWidth/8, worldWidth/8)
                 y <- getRandom (-worldHeight/8, worldHeight/8)
                 initSize <- evalRand $ randomValWithVariance initialGermSize initialGermSizeVariance
                 hc <- runOnHipState $ addHipCirc initSize (R2 x y)
                 createGerm initSize (R2 x y) hc
      modify $ \gs -> gs { gsGerms        = M.fromList (zip [0..] germs)
                         , gsNextGermId   = length germs
                         , gsSoundQueue   = [GameSoundLevelMusicStart]
                         , gsCurrentLevel = i
                         }
      return $ FSMPlayingLevel
    --------------------------------------
    fsmPlayingLevel = do
      gs <- get
      if M.size (gsGerms gs) > maxGerms
       then return FSMGameOver
       else do
        case ev of
          Tap p            -> playingLevelTap p
          Physics duration -> do
            physics duration
            return fsmState
          _ -> error $ printf "Event '%s' not handled by fsmLevel" (show ev)
    --------------------------------------
    fsmAntibioticUnlocked = error "fsmAntibioticUnlocked not implemented"
    --------------------------------------
    fsmLevelComplete      = do
      gs <- get
      case ev of
        TapAnywhere -> return $ FSMLevel (gsCurrentLevel gs + 1)
        _ -> do
          let w2c = gsWorldToCanvas gs
              render = drawText levelCompleteColor (R2 0 0) (worldWidth,worldHeight/2) w2c
                         "Epidemic averted!"
          modify $ \gs -> gs { gsRender = render }
          return $ FSMLevelComplete
    --------------------------------------
    fsmGameOver           = do
      case ev of
        TapAnywhere -> return $ FSMLevel 1
        _ -> do
          modify $ \gs ->
           gs { gsRender = do
                  gsRender gs -- draw what we had before
                  let w2c = gsWorldToCanvas gs
                  drawText gameOverColor (R2 0 0) (worldWidth,worldHeight/2) w2c
                    "Infected!"
              , gsSoundQueue = [GameSoundLevelMusicStop]
              }
          return FSMGameOver

----------------------------------------------------------------------------------------------------
playingLevelTap ::  R2 -> GameM FSMState
playingLevelTap p = do
  killGerm p
  gs <- get
  return $ case M.size (gsGerms gs) of
    0 -> FSMLevelComplete
    _ -> FSMPlayingLevel

----------------------------------------------------------------------------------------------------
--
-- FIXME: Make this more efficient. Brute force searches through germs to kill them.
--
killGerm :: R2 -> GameM ()
killGerm p = do
  gs <- get
  let germsToKill = M.toList $ M.filter (tapCollides p) (gsGerms gs)
  let kkk (germId, germ) = do
        runOnHipState $ removeHipCirc (germHipCirc germ)
        modify $ \gs -> gs { gsGerms = M.delete germId (gsGerms gs)
                           , gsSoundQueue = GameSoundSquish:gsSoundQueue gs }
        runGLM . germGLFinaliser . germGL $ germ
  mapM_ kkk germsToKill
  where
    tapCollides :: R2 -> Germ -> Bool
    tapCollides (R2 x y) g = let sz       = germSizeFun g (germCumulativeTime g)
                                 R2 x' y' = germPos g
                             in (x' - x)**2 + (y' - y)**2 < sz*sz


--
-- Runs in the HipM monad. Gets the new state. Sets it.
--
runOnHipState :: HipM a -> GameM a
runOnHipState hipM = do
  gs <- get
  runHipM (gsHipState gs) hipM


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
    (R2 x y) <- runOnHipState $ getHipCircPos hc
    if (t > germMultiplyAt g)
    then do
      -- TODO mutate the germ
      dx <- getRandom (0,sz)
      dy <- getRandom (0,sz)
      let i       = gsNextGermId gs
          hc      = germHipCirc g
          (x',y') = (x + dx, y + dy)
      hc' <- runOnHipState $ do
        setHipCircRadius hc (sz/2)
        addHipCirc (sz/2) (R2 x' y')
      ng <- createGerm (sz/2) (R2 x' y') hc'
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
--
-- [whenGerm] applies [f] if germ with [germId] exists in the [GameState]
--
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
  mapM_ (growGerm duration) (M.keys $ gsGerms gs)
  runOnHipState $ hipStep duration -- replicateM 10 (hipStep (duration/10))
  let drawOneGerm :: (Int, Germ) -> GLM ()
      drawOneGerm (i,g) = do
        (germGLFun . germGL $ g) i (germPos g) (germAnimTime g) (germSizeFun g (germCumulativeTime g))
  modify $ \gs -> let render = mapM_ drawOneGerm (zip [50..] $ M.elems $ gsGerms gs)
                  in  gs { gsRender = render }

----------------------------------------------------------------------------------------------------

--drawText :: WorldToCanvas -> Color -> R2 -> Double -> String -> Render ()
--drawText w2c c pos w s = do
--  let wl2cl = worldLenToCanvasLen w2c
--      wp2cp = worldPtToCanvasPt w2c
--  text "Helvetica" c (wp2cp pos) (wl2cl w) s