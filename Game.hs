module Game where

import Graphics.Rendering.Cairo

import Control.Monad.Random

sinU, cosU :: Floating a => a -> a
sinU = sin . (2*pi*)
cosU = cos . (2*pi*)
tanU a = sinU a / cosU a

data Color = Color Double Double Double Double deriving Show

type Point = (Double, Double)

white = Color 1 1 1 1
blue  = Color 0 0 1 1
green = Color 0 1 0 1
black = Color 0 0 0 1

type GermGradient = (Color, Color)

data GermKind = Wobble Int -- number of wobbles
              | Spiky  Int -- number of spikes
              deriving Show

-- point which lies inside a unit circle. Invariant: x*x + y*y < 1
newtype NormalisedPoint = NormalisedPoint Point deriving Show

data Germ = Germ { germKind         :: GermKind
                 , germRadius       :: Double
                 , germBodyGrad     :: GermGradient
                 , germNucleusGrad  :: GermGradient
                 , germNucleusPts   :: [NormalisedPoint]
                 } deriving Show

normalisedPtToPt :: Double -> NormalisedPoint -> Point
normalisedPtToPt scale (NormalisedPoint (x,y)) = (scale*x, scale*y)

drawGerm :: Germ -> Render ()
drawGerm g = do
  let r = germRadius g
  withGermGradient (germBodyGrad g) r    $ drawBody (germKind g) r
  withGermGradient (germNucleusGrad g) r $ blob (map (normalisedPtToPt (r/2)) $ germNucleusPts g)
  where
    drawBody :: GermKind -> Double -> Render ()
    drawBody k r = case k of
      Wobble n -> wobble n r
      Spiky  n -> spiky n (r/2) r

withGermGradient :: GermGradient -> Double -> Render () -> Render ()
withGermGradient (Color r g b a, Color r' g' b' a') radius render = do
  withRadialPattern 0 0 0 0 0 radius $ \p -> do
    patternAddColorStopRGBA p 0 r  g  b  a
    patternAddColorStopRGBA p 1 r' g' b' a'
    setSource p
    render
    fill

-- TODO: Draw some pictures of how you derived radCircle and lenPolySide
wobble :: Int -> Double -> Render ()
wobble bumpiness radius = do
  let radial s a = (s*cosU a, s*sinU a)
      l          = lenPolySide bumpiness radius
      r          = radius - l/4
      evenPts    = map (radial (radius - l/4))     [0,2/bumps..1]
      oddPts     = map (radial (radInnerCircle bumpiness r)) [1/bumps,3/bumps..1]
      smallCircle pt = circle pt (l/4) >> fill
  mapM_ smallCircle evenPts
  polygon evenPts >> fill
  withColor black $ mapM_ smallCircle oddPts
    where
      bumps = fromIntegral (2*bumpiness)

--
-- Take a circle of radius [r], transcribe a regular polygon with [n] sides
-- inside it. Each vertex touches the circle at regular intervals. Now transcribe
-- a circle within that polygon. The circle will touch the mid-point of each side
-- of the polygon.
--
-- This function returns radius of that inner circle.
--
radInnerCircle n r =   r*sinU (alpha n/2)

--
--
--
lenPolySide    n r = 2*r*c/(1+1/2*c) where c = cosU (alpha n/2)

--
-- Takes two lists, not necessarily of the same length.
-- Returns a list that alternates between elements of the
-- first list and the second list up to the point where
-- the shorter list runs out of values. The remaning elements
-- are from the longer list.
--
-- Examples:
--
-- alternate [1,2]     [10,20]     == [1,10,2,20]
-- alternate [1,2]     [10,20,30]  == [1,10,2,20,30]
-- alternate [1,2,3,4] [10,20]     == [1,10,2,20,3,4]
--
alternate :: [a] -> [a] -> [a]
alternate [] ys = ys
alternate xs [] = xs
alternate (x:xs) (y:ys) = x:y:alternate xs ys

data PolarPoint = P2 Double Double -- radius and angle

polarPtToPt :: PolarPoint -> Point
polarPtToPt (P2 r ang) = (r*cosU ang, r*sinU ang)

--
-- Given [n] the number of points in the star, and two value specifying
-- the inner and out radii, returns the points defining a star.
-- The first "point" of the star points right.
--
starPolyPoints :: Int -> Double -> Double -> [Point]
starPolyPoints n ri ro = map polarPtToPt pairs
  where
    n' = fromIntegral n
    angInc = 1/n' -- angle between points on star

    outerPolarPt a = P2 ro a
    innerPolarPt a = P2 ri (a + angInc/2)
    pairs =  [f a | a <- angles, f <-  [outerPolarPt, innerPolarPt]]
    angles = [0,angInc..1-angInc]



blob :: [Point] -> Render ()
blob ps = do
  uncurry moveTo start
  mapM_ (uncurry quadraticCurveTo) rest
  fill
  where
    ((_,start):rest) = take (length ps + 1) . foo . cycle $ ps
    foo :: [Point] -> [(Point, Point)]
    foo []        = []
    foo [_]       = []
    foo (x:x':xs) = (x, midPt x x'):foo (x':xs)

spiky :: Int -> Double -> Double -> Render ()
spiky n ri ro = blob $ starPolyPoints n ri ro

midPt :: Point -> Point -> Point
midPt (x,y) (x',y') = ((x+x')/2, (y+y')/2)

--
-- alpha returns the internal angle of a regular n-gon in turns
--
alpha :: (Integral a, Fractional f) => a -> f
alpha n = (n'-2)/(2*n') where n' = fromIntegral n

----------------
-- Random helpers

circle :: Point -> Double -> Render ()
circle (x,y) r = arc x y r 0 (2*pi)

polygon :: [Point] -> Render ()
polygon []  = return ()
polygon [_] = return ()
polygon (s:x:xs) = moveTo' s >> go (x:xs)
  where
    go []     = lineTo' s
    go (x:xs) = lineTo' x >> go xs


moveTo' = uncurry moveTo
lineTo' = uncurry lineTo

setColor :: Color -> Render ()
setColor (Color r g b a) = setSourceRGBA r g b a

inContext :: Render () -> Render ()
inContext r = save >> r >> restore

withColor :: Color -> Render () -> Render ()
withColor color d = inContext $ setColor color >> d

quadraticCurveTo :: Point -> Point -> Render ()
quadraticCurveTo (cx,cy) (ex,ey) = do
   (x,y) <- getCurrentPoint
   curveTo (f x cx) (f y cy) (f ex cx) (f ey cy) ex ey
   where
     f a b = 1.0/3.0 * a + 2.0/3.0 * b

---------------------------------------
-- Randomness

randomColor :: RandomGen g => Rand g Color
randomColor = do
  (r:g:b:_)  <- getRandomRs (0, 1)
  return $ Color r g b 1

--
-- Given [n] and number of points and bounds [(lo, hi)] produces a series
-- of points. Angularly they are equal spaced around the origin. Their
-- radii differ though.
--
randomRadialPoints :: RandomGen g => Int -> Rand g [NormalisedPoint]
randomRadialPoints n = do
  let n' = fromIntegral n
  rs <- getRandomRs (0.3,1)
  let as = [0,1/n'..1-1/n']
  return $ map (NormalisedPoint . polarPtToPt) $ zipWith P2 rs as

randomGermKind :: RandomGen g => Rand g GermKind
randomGermKind = do
  k <- getRandomR (0::Int,1)
  n <- getRandomR (5,13)
  return $ case k of
    0 -> Wobble n
    1 -> Spiky n

randomGradient :: RandomGen g => Rand g GermGradient
randomGradient = do
  c  <- randomColor
  c' <- randomColor
  return (c,c')

randomGerm :: RandomGen g => Double -> Rand g Germ
randomGerm radius = do
  k   <- randomGermKind
  g   <- randomGradient
  g'  <- randomGradient
  pts <- randomRadialPoints 10
  return $ Germ k radius g g' pts

--------------------------

renderCenter :: Double -> Double -> Render () -> Render ()
renderCenter w h render = do
  translate w h
  render


