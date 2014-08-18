module Game where

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Rasterific.Linear
import Codec.Picture
import Control.Monad.Random

sinU, cosU :: Floating a => a -> a
sinU = sin . (2*pi*)
cosU = cos . (2*pi*)
tanU a = sinU a / cosU a

white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
blue  = PixelRGBA8 0 0 0xFF 0xFF
green = PixelRGBA8 0x00 0xFF 0x00 0xFF

type GermGradient = (PixelRGBA8, PixelRGBA8)

data GermKind = Wobble Int -- number of wobbles
              | Spiky Int  -- number of spikes
              deriving Show

-- point which lies inside a unit circle. Invariant: x*x + y*y < 1
newtype NormalisedPoint = NormalisedPoint Point deriving Show

data Germ = Germ { germKind         :: GermKind
                 , germRadius       :: Float
                 , germBodyGrad     :: GermGradient
                 , germNucleusGrad  :: GermGradient
                 , germNucleusPts   :: [NormalisedPoint]
                 } deriving Show

normalisedPtToPt :: Float -> NormalisedPoint -> Point
normalisedPtToPt scale (NormalisedPoint p) = p ^* scale

drawGerm :: Germ -> Drawing PixelRGBA8 ()
drawGerm g = do
  let r = germRadius g
  withTexture (toGrad (germBodyGrad g) r)    $ drawBody (germKind g) r
  withTexture (toGrad (germNucleusGrad g) r) $ blob (map (normalisedPtToPt (r/2)) $ germNucleusPts g)
  where
    toGrad :: GermGradient -> Float -> Texture PixelRGBA8
    toGrad (c,c') r = radialGradientTexture [(0.0, c), (1.0, c')] (V2 0 0) r
    drawBody :: GermKind -> Float -> Drawing PixelRGBA8 ()
    drawBody k r = case k of
      Wobble n -> wobble n r
      Spiky  n -> spiky n (r/2) r

-- TODO: Draw some pictures of how you derived radCircle and lenPolySide
wobble :: Int -> Float -> Drawing PixelRGBA8 ()
wobble bumpiness radius = do
  let radial s a = V2 (cosU a) (sinU a) ^* s
      l          = lenPolySide bumpiness radius
      r          = radius - l/4
      evenPts    = map (radial (radius - l/4))     [0,2/bumps..1]
      oddPts     = map (radial (radInnerCircle bumpiness r)) [1/bumps,3/bumps..1]
      smallCircle pt = fill . circle pt $ l/4
  mapM_ smallCircle evenPts
  fill $ polygon evenPts
  withColor white $ mapM_ smallCircle oddPts
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

data PolarPoint = P2 Float Float -- radius and angle

polarPtToPt :: PolarPoint -> Point
polarPtToPt (P2 r ang) = V2 (cosU ang) (sinU ang) ^* r

--
-- Given [n] the number of points in the star, and two value specifying
-- the inner and out radii, returns the points defining a star.
-- The first "point" of the star points right.
--
starPolyPoints :: Int -> Float -> Float -> [Point]
starPolyPoints n ri ro = map polarPtToPt pairs
  where
    n' = fromIntegral n
    angInc = 1/n' -- angle between points on star

    outerPolarPt a = P2 ro a
    innerPolarPt a = P2 ri (a + angInc/2)
    pairs =  [f a | a <- angles, f <-  [outerPolarPt, innerPolarPt]]
    angles = [0,angInc..1-angInc]



blob :: [Point] -> Drawing PixelRGBA8 ()
blob ps = fill $ pathToPrimitives $ Path start False $
                 map (uncurry PathQuadraticBezierCurveTo) rest
  where
    ((_,start):rest) = take (length ps + 1) . foo . cycle $ ps
    foo :: [Point] -> [(Point, Point)]
    foo []        = []
    foo [_]       = []
    foo (x:x':xs) = (x, midPt x x'):foo (x':xs)

spiky :: Int -> Float -> Float -> Drawing PixelRGBA8 ()
spiky n ri ro = blob $ starPolyPoints n ri ro

midPt :: Point -> Point -> Point
midPt (V2 x y) (V2 x' y') = V2 ((x+x')/2) ((y+y')/2)

--
-- alpha returns the internal angle of a regular n-gon in turns
--
alpha :: (Integral a, Fractional f) => a -> f
alpha n = (n'-2)/(2*n') where n' = fromIntegral n

withColor :: {-forall px . RenderablePixel px =>-} px -> Drawing px () -> Drawing px ()
withColor color d = withTexture (uniformTexture color) d

---------------------------------------
-- Randomness

randomColor :: RandomGen g => Rand g PixelRGBA8
randomColor = do
  (r:g:b:_)  <- getRandomRs (0, 0xFF)
  return $ PixelRGBA8 r g b 0xFF

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

randomGerm :: RandomGen g => Float -> Rand g Germ
randomGerm radius = do
  k   <- randomGermKind
  g   <- randomGradient
  g'  <- randomGradient
  pts <- randomRadialPoints 10
  return $ Germ k radius g g' pts

-----------------------------------------
