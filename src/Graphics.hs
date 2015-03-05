module Graphics (
  -- types
  GermGfx(..), Time, Color, Gradient, CairoPoint, Render, TextConstraint(..),
  -- functions
  randomGermGfx, germGfxRenderNucleus, germGfxRenderBody, germGfxRenderGerm,
  textOfWidth, textOfHeight, textConstrainedBy, textOfWidth_, textOfHeight_,
  textLinesOfWidth, movingPtToPt, movingPtToStaticPt, mutateGermGfx,
  runWithoutRender, flask, fontInfoForWidth,
  --
  circle


) where

import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as M
import           Control.Monad.Random
import           Foreign.Marshal.Alloc (allocaBytes)
import           Data.List (maximumBy)
import           Control.Monad
import           Text.Printf

-- friends
import Types
import Util
import Platform

{-

TODO: There is a question of whether everything is normalised or not.
Especially where we are adding periodic functions onto the radius of the germ it's sometimes
going to go out of bounds. Is this okay?

-}

-------------------------------------------
-- Constants

jigglePeriodBounds :: (Frac, Frac)
jigglePeriodBounds = (7,15)

jiggleRadiusAmplitudeBounds :: (Frac, Frac)
jiggleRadiusAmplitudeBounds = (0.05, 0.1)

jiggleAngleAmplitudeBounds :: (Frac, Frac)
jiggleAngleAmplitudeBounds = (-0.02, 0.02)

jigglePhaseBounds :: (Frac, Frac)
jigglePhaseBounds = (0,1)

germSpikeRange :: (Int, Int)
germSpikeRange = (5,13)

-- A value between 0 and 1 that represents how transparent the nucleus is. 0 is transparent.
-- 1 is opaque.
nucleusAlpha :: Double
nucleusAlpha = 0.5

numNucleusPoints :: Int
numNucleusPoints = 10

nucleusRadiusRange :: (Frac, Frac)
nucleusRadiusRange = (0.2,0.5)

-- Between 0 and 1
spikyInnerRadius, spikyOuterRadius :: Frac
spikyInnerRadius = 0.5
spikyOuterRadius = 1

----------------------------------------------------------------------------------------------------
--
-- [sinU], [cosU], [tanU] are like sin, cos and tan except that they work in units of
-- "turns" (as opposed to radians or degrees). 1 turn is equal to 2*pi radians or 360 degrees.
--
-- Writing functions in terms of turns almost completely eliminates all mention of pi from
-- your functions and makes them easier to understand.
--
sinU, cosU :: Floating a => a -> a
sinU   = sin . (2*pi*)
cosU   = cos . (2*pi*)

----------------------------------------------------------------------------------------------------
{-# INLINE movingPtToPt #-}
movingPtToPt :: Time ->  Double -> MovingPoint -> (Double, Double)
movingPtToPt t scale ((r, pf),(a, pf')) =
  polarPtToPt (P2 r' a')
  where
    r' = r + scale * periodicValue t pf
    a' = a + scale * periodicValue t pf'
----------------------------------------------------------------------------------------------------
--
-- [movingPtToStaticPt] throws away the perturbations.
--
{-# INLINE movingPtToStaticPt #-}
movingPtToStaticPt :: MovingPoint -> (Double, Double)
movingPtToStaticPt ((r, _),(a, _)) = polarPtToPt (P2 r a)

----------------------------------------------------------------------------------------------------
ptToCairoPt :: (Frac, Frac) -> CairoPoint
ptToCairoPt = id

----------------------------------------------------------------------------------------------------
periodicValue :: Time -> PeriodicFun -> Frac
periodicValue t (amp, period, phase) = amp * sinU ((t + phase)/period)

----------------------------------------------------------------------------------------------------
--
-- Draws a germ of radius [r] centred at [(x,y)]
--
--
-- The animation speed is dependent on the size of the germ. Smaller germs don't look
-- like they're moving at all unless their animation speed is increased.
--
germGfxRenderNucleus :: GermGfx -> Double -> Render ()
germGfxRenderNucleus gg r = do
   asGroup $ do
     scale r r
     translate 1 1
     withGradient (pmap (changeAlpha nucleusAlpha) $ germGfxNucleusGrad gg) 1 $ do
       blob . map (ptToCairoPt . movingPtToStaticPt) . germGfxNucleus $ gg
     -- scale to radius [r]

----------------------------------------------------------------------------------------------------
germGfxRenderBody :: GermGfx -> Double -> Render ()
germGfxRenderBody gg r = do
   asGroup $ do
     scale r r
     translate 1 1
     withGradient (germGfxBodyGrad gg) 1 $ do
       blob . map (ptToCairoPt . movingPtToStaticPt) . germGfxBody $ gg
     -- scale to radius [r]

----------------------------------------------------------------------------------------------------
germGfxRenderGerm :: GermGfx -> Double -> Render ()
germGfxRenderGerm gg r = germGfxRenderBody gg r >> germGfxRenderNucleus gg r

----------------------------------------------------------------------------------------------------
withGradient :: Gradient -> Double -> Render () -> Render ()
withGradient (Color r g b a, Color r' g' b' a') radius drawing = do
  withRadialPattern 0 0 0 0 0 radius $ \p -> do
    patternAddColorStopRGBA p 0 r  g  b  a
    patternAddColorStopRGBA p 1 r' g' b' a'
    setSource p
    drawing
    fill

----------------------------------------------------------------------------------------------------
{-# INLINE polarPtToPt #-}
polarPtToPt :: PolarPoint -> (Frac, Frac)
polarPtToPt (P2 r ang) = (r*cosU ang, r*sinU ang)

----------------------------------------------------------------------------------------------------
--
-- Given [n] the number of points in the star, and two values specifying
-- the inner and out radii, returns the points defining a star.
-- The first "point" of the star points right.
--
starPolyPoints :: Int -> Frac -> Frac -> [PolarPoint]
starPolyPoints n ri ro = polarPoints
  where
    n' = fromIntegral n
    angInc = 1/n' -- angle between points on star
    outerPolarPt a = P2 ro a
    innerPolarPt a = P2 ri (a + angInc/2)
    polarPoints =  [f a | a <- angles, f <-  [outerPolarPt, innerPolarPt]]
    angles = [0,angInc..1-angInc]

----------------------------------------------------------------------------------------------------
--
-- Smooths out the rough edges on a polygon
--
blob :: [CairoPoint] -> Render ()
blob ps = do
  moveTo' start
  mapM_ (uncurry quadraticCurveTo) rest
  fill
  where
    ((_,start):rest) = take (length ps + 1) . foo . cycle $ ps
    foo :: [CairoPoint] -> [(CairoPoint, CairoPoint)]
    foo []        = []
    foo [_]       = []
    foo (x:x':xs) = (x, midPt x x'):foo (x':xs)

----------------------------------------------------------------------------------------------------
midPt :: CairoPoint -> CairoPoint -> CairoPoint
midPt (x,y) (x',y') = ((x+x')/2, (y+y')/2)

----------------------------------------------------------------------------------------------------
--
-- alpha returns the internal angle of a regular n-gon in turns
--
_alpha :: (Integral a, Fractional f) => a -> f
_alpha n = (n'-2)/(2*n') where n' = fromIntegral n

-------------------------------------------------
-- Random helpers to make Cairo more declarative

_clear :: Render () -> Render ()
_clear r = inContext $ do
  setOperator OperatorClear
  r

----------------------------------------------------------------------------------------------------
circle :: CairoPoint -> Double -> Render ()
circle (x,y) r = arc x y r 0 (2*pi)

----------------------------------------------------------------------------------------------------
_polygon :: [CairoPoint] -> Render ()
_polygon []  = return ()
_polygon [_] = return ()
_polygon (s:x:xs) = moveTo' s >> go (x:xs)
  where
    go []     = _lineTo' s
    go (x:xs) = _lineTo' x >> go xs


----------------------------------------------------------------------------------------------------
moveTo', _lineTo' :: (Double, Double) -> Render ()
moveTo' = uncurry moveTo
_lineTo' = uncurry lineTo

----------------------------------------------------------------------------------------------------
setColor :: Color -> Render ()
setColor (Color r g b a) = setSourceRGBA r g b a

----------------------------------------------------------------------------------------------------
inContext :: Render a -> Render a
inContext r = do { save; res <- r ; restore; return res }

----------------------------------------------------------------------------------------------------
_withColor :: Color -> Render () -> Render ()
_withColor color d = inContext $ setColor color >> d

----------------------------------------------------------------------------------------------------
quadraticCurveTo :: CairoPoint -> CairoPoint -> Render ()
quadraticCurveTo (cx,cy) (ex,ey) = do
   (x,y) <- getCurrentPoint
   curveTo (f x cx) (f y cy) (f ex cx) (f ey cy) ex ey
   where
     f a b = 1.0/3.0 * a + 2.0/3.0 * b

----------------------------------------------------------------------------------------------------
_drawBackground :: (Int, Int) -> Render ()
_drawBackground (w, h)  = do
  setAntialias AntialiasSubpixel
  setColor white
  rectangle 0 0 (fromIntegral w) (fromIntegral h)
  fill
----------------------------------------------------------------------------------------------------
-- Randomness

randomColor :: RandomGen g => Rand g Color
randomColor = do
  (r:g:b:_)  <- getRandomRs (0, 1)
  return $ Color r g b 1

----------------------------------------------------------------------------------------------------
--
-- Renders a flask. This is the icon representing an antibiotic.
--
--
--                -0 +     + 0                                               |
--                   |     |                                                 |
--                   |     |                                                 + 1a
--                -1 +     + 1             Zoom in on neck:                  |
--                  /       \                                                + 1
--                 /         \l (for liquid)                                  \
--                /           \                                                \
--               /             \                                                \ 1b
--         -2a  +               + 2a
--             /                 \
--         -2 +--+------------+--+ 2
--               -2b        2b
--
--
-- Instead of sharp corners, we draw a quadratic bezier curve with control points (2', 2, 2'')
--
--
flask :: Color -> Render ()
flask liquidColor = do
  let neckWidth = 0.2
      bodyWidth = 1.0
      neckHeight = totalHeight - bodyHeight
      bodyHeight = 0.7*totalHeight
      totalHeight = 0.97
      roundFactor = 0.25 -- percent
      neckRoundFactor = 0.03
      spoutRoundFactorX = 0.04
      spoutRoundFactorY = 0.05
      liquidFactor = 0.75 -- percentage full (not by volume, by height)
  setLineWidth 0.02
  setLineCap LineCapRound
  ----
  let ndy = neckRoundFactor*totalHeight
      dy  = roundFactor*totalHeight
      sdy = spoutRoundFactorY*totalHeight
      sdx = spoutRoundFactorX*totalHeight
      slope = (y2 - y1) / (x2 - x1)
      c     = y2 - x2*slope

      (x0a,y0a) = (x0 + sdx, totalHeight/2)
      (x0,y0)   = (neckWidth/2, y0a - sdy)
      (x0b,y0b) = (x0, y0 - sdy)

      (lx, ly)  = ((ly - c)/slope, liquidFactor * bodyHeight - totalHeight/2)

      (x1a,y1a) = (x1, y1 + ndy)
      (x1,y1)   = (neckWidth/2, totalHeight/2 - neckHeight)
      (x1b,y1b) = ((y1b - c)/slope  , y1 - ndy)

      (x2a,y2a) = ((y2a - c)/slope, y2 + dy)
      (x2,y2)   = (bodyWidth/2, -totalHeight/2)
      (x2b,y2b) = (x2 - dy, y2)
  ----
  let drawBody x' y' = do
        moveTo x' y'
        lineTo x2a y2a
        quadraticCurveTo (x2, y2) (x2b, y2b)
        lineTo (-x2b) (y2b)
        quadraticCurveTo (-x2, y2) (-x2a, y2a)
        lineTo (-x') y'

  setColor liquidColor
  drawBody lx ly
  fill

  setColor $ Color 0.5 0.5 0.5 1
  moveTo x0a y0a
  quadraticCurveTo (x0, y0) (x0b, y0b)
  lineTo x1a y1a
  quadraticCurveTo (x1, y1) (x1b, y1b)

  drawBody x1b y1b

  quadraticCurveTo (-x1, y1) (-x1a, y1a)
  lineTo (-x0b) y0b
  quadraticCurveTo (-x0, y0) (-x0a, y0a)
  stroke

----------------------------------------------------------------------------------------------------
--
-- We want the two colours to be a minimum distance apart
--
--
randomGradient :: RandomGen g => Rand g Gradient
randomGradient = do
  c@(Color r g b _) <- randomColor
  (dr:dg:db:_)   <- getRandomRs (0.1,0.5)
  return (c, Color (f r dr) (f g dg) (f b db) 1)
  where
    f x dx = if x < 0.5 then x + dx else x - dx

----------------------------------------------------------------------------------------------------
randomGermGfx :: RandomGen g => Rand g GermGfx
randomGermGfx = do
  n <- getRandomR germSpikeRange
  g <- randomGradient
  g' <- randomGradient
  let bodyPts = starPolyPoints n spikyInnerRadius spikyOuterRadius
  bodyPts'    <- mapM polarPtToMovingPt bodyPts
  nucleusPts  <- randomRadialPoints2 numNucleusPoints
  nucleusPts' <- mapM polarPtToMovingPt nucleusPts
  return $ GermGfx
    { germGfxBodyGrad    = g
    , germGfxNucleusGrad = g'
    , germGfxBody        = bodyPts'
    , germGfxNucleus     = nucleusPts'
    , germGfxSpikes      = n }
  where
----------------------------------------------------------------------------------------------------
mutateGermGfx :: RandomGen g => GermGfx  -> Rand g GermGfx
mutateGermGfx gfx = do
  bodyGrad <- mutateGradient $ germGfxBodyGrad gfx
  nucleusGrad <- mutateGradient $ germGfxNucleusGrad gfx
  dn <- getRandomR (-1,1) -- -1,0,1
  let n'      = uncurry clamp germSpikeRange (dn + germGfxSpikes gfx)
      bodyPts = starPolyPoints n' spikyInnerRadius spikyOuterRadius
  bodyPts' <- mapM polarPtToMovingPt bodyPts
  nucleusPts  <- randomRadialPoints2 numNucleusPoints
  nucleusPts' <- mapM polarPtToMovingPt nucleusPts
  return $ GermGfx
    { germGfxBodyGrad    = bodyGrad
    , germGfxNucleusGrad = nucleusGrad
    , germGfxBody        = bodyPts'
    , germGfxNucleus     = nucleusPts'
    , germGfxSpikes      = n'
    }
  where

    mutateGradient :: RandomGen g => Gradient -> Rand g Gradient
    mutateGradient (Color r g b _, Color r' g' b' _) = do
      let cl = clamp 0 1
      (dr:dg:db:dr':dg':db':_) <- getRandomRs (-gradientColorMutationMax, gradientColorMutationMax)
      return $ (Color (cl $ r+dr)   (cl $ g+dg)   (cl $ b+db)  1,
                Color (cl $ r'+dr') (cl $ g'+dg') (cl $ b'+db') 1)

----------------------------------------------------------------------------------------------------
randomRadialPoints2 :: RandomGen g => Int -> Rand g [PolarPoint]
randomRadialPoints2 n = do
  rs <- getRandomRs nucleusRadiusRange
  let as = [0, 1/n'..1-1/n']
  return $ zipWith P2 rs as
  where
    n' = fromIntegral n

----------------------------------------------------------------------------------------------------
randomPeriodicFuns :: RandomGen g => (Frac, Frac) -> Rand g PeriodicFun
randomPeriodicFuns ampBounds = do
  amp    <- getRandomR ampBounds
  period <- getRandomR jigglePeriodBounds
  phase  <- getRandomR jigglePhaseBounds
  return $ (amp, period, phase)

----------------------------------------------------------------------------------------------------
polarPtToMovingPt :: RandomGen g => PolarPoint -> Rand g MovingPoint
polarPtToMovingPt (P2 r a) = do
  pfs <- randomPeriodicFuns jiggleRadiusAmplitudeBounds
  pfs' <- randomPeriodicFuns jiggleAngleAmplitudeBounds
  return $ ((r, pfs),(a, pfs'))

----------------------------------------------------------------------------------------------------
changeAlpha :: Double -> Color -> Color
changeAlpha a' (Color r g b _) = Color r g b a'

----------------------------------------------------------------------------------------------------
-- map over uniform pairs. Would be better to use a new data structure [data Pair a = Pair a a]
pmap :: (a -> b) -> (a,a) -> (b,b)
pmap f (a,b) = (f a, f b)

----------------------------------------------------------------------------------------------------
----
---- Produces n*n germs on a w x h screen
----
--tiledGerms :: RandomGen g => Int -> Int -> Int -> Rand g Anim
--tiledGerms n w h = do
--  germs <- replicateM (n*n) randomGermGfx
--  let germsAndCentres = zip germs centres
--  return $ \t -> do
--    forM_ germsAndCentres $ \(g, (x,y)) -> do
--      germGfxToRender g (w,h) (x, y) r t
--  where
--    n'      = fromIntegral n
--    r       = fromIntegral (min w h) / (n'*2)
--    vs      = [r,3*r..n'*2*r-1]
--    centres = [ (x,y) | x <- vs, y <- vs]

----------------------------------------------------------------------------------------------------
asGroup :: Render () -> Render ()
asGroup r = do
  pushGroup
  r
  popGroupToSource
  paint

----------------------------------------------------------------------------------------------------
--newGermAnim :: RandomGen g => (Int,Int) -> Rand g Anim
--newGermAnim (screenWidth, screenHeight) =
--  tiledGerms tileGermsPerRow screenWidth screenHeight

----------------------------------------------------------------------------------------------------

--
-- [textOfWidth fontFace c (x,y) w s] renders the text [s] at location [(x,y)] with width [w].
-- The text is centered at [(x,y)] and the height of the text depends on the [fontFace].
-- It returns the height of the rendered text
--
-- [textOfHeight] is the same except that it ensures the text is of a particular height and
-- returns the width of the rendered text.
--
textOfWidth, textOfHeight :: FontFace -> Gradient -> CairoPoint -> Double -> String -> Render Double

textOfWidth  = textConstrainedBy Width
textOfHeight = textConstrainedBy Height

--
-- Versions of [textOfWidth] and [textOfHeight] where we don't care about the return value
--

textOfWidth_ :: FontFace -> Gradient -> CairoPoint -> Double -> String -> Render ()
textOfWidth_ fontFace c (x,y) w s =  textOfWidth fontFace c (x,y) w s >> return ()

--
-- A version of [textOfWidth] where we don't care about the height
--
textOfHeight_ :: FontFace -> Gradient -> CairoPoint -> Double -> String -> Render ()
textOfHeight_ fontFace c (x,y) h s =  textOfHeight fontFace c (x,y) h s >> return ()


----------------------------------------------------------------------------------------------------
data TextConstraint = Width | Height deriving (Show, Eq)

ifWidthElse :: TextConstraint -> a -> a -> a
ifWidthElse tc wa ha = case tc of Width -> wa; Height -> ha

----------------------------------------------------------------------------------------------------


textConstrainedBy :: TextConstraint -> FontFace -> Gradient -> CairoPoint -> Double -> String
                  -> Render Double
textConstrainedBy tc fontFace (Color r g b a, Color r' g' b' a') (x,y) len s = do
  fi <- fontInfoForConstraint tc fontFace len s
  setFontFace fontFace
  setFontSize (fiFontSize fi)
  let (x', y') = (x + fiDx fi, y + fiDy fi)
      lenD' = ifWidthElse tc (fiHeight fi) (fiWidth fi)
  withLinearPattern 0 y' 0 (y'+ fiHeight fi) $ \p -> do
    patternSetExtend p ExtendRepeat
    patternAddColorStopRGBA p 0   r  g  b  a
    patternAddColorStopRGBA p 0.5 r' g' b' a'
    when False $ do
      let (w,h) = (fiWidth fi, fiHeight fi)
      setColor (Color 0.5 0.5 0.5 1)
      rectangle  (-w/2) (-h/2) w h
      fill
    transform $ M.Matrix 1 0 0 (-1) 0 0
    moveTo x' y'
    textPath s
    setSource p
    fillPreserve
    return lenD'

----------------------------------------------------------------------------------------------------

textLinesOfWidth :: FontFace -> Color -> CairoPoint -> Double -> [String] -> Render Double
textLinesOfWidth fontFace c (x,y) len ss = do
  -- find the longest line
  fis <- mapM (fontInfoForWidth fontFace len) ss
  let ssLen = length ss
      FontInfo { fiFontSize = fontSize, fiHeight = lineH }  =
        minimumBy (\fi fi' -> compare (fiFontSize fi) (fiFontSize fi'))
                  (filter (\fi -> fiFontSize fi > 0) fis)
      lines = zipWith3 (,,) fis ss [0..]
      showLine (fi,s,i) = do
        (TextExtents bx _ tw _ _ _) <- textExtents s
        let w = tw + bx
        moveTo (x - w/2) (y + fiDy fi + (-(fromIntegral ssLen/2)+i)*lineH)
        showText s
  --setColor (Color 0.5 0.5 0.5 1)
  --rectangle  (-lineW/2) (-h/2) lineW h
  --fill
  setFontSize fontSize
  setFontFace fontFace
  setColor c
  transform $ M.Matrix 1 0 0 (-1) 0 0
  mapM_ showLine lines
  return (lineH*(fromIntegral ssLen))
  where
    minimumBy f = maximumBy (flip f)
----------------------------------------------------------------------------------------------------

-- The return type for the font size is [Double] even though it's an integral value.
-- [setFontSize] takes a [Double] as an argument. Go figure.
data FontInfo = FontInfo { fiFontSize :: Double
                         , fiDx       :: Double
                         , fiDy       :: Double
                         , fiWidth    :: Double
                         , fiHeight   :: Double
                         } deriving (Show)

----------------------------------------------------------------------------------------------------

fontInfoForConstraint :: TextConstraint -> FontFace -> Double -> String
                      -> Render FontInfo
fontInfoForConstraint tc fontFace len s = do
  inContext $ do
    setFontFace fontFace
    let initFontSize = 1000 -- scaling it up gives more accurate TextExtents
    setFontSize initFontSize
    (TextExtents bx _ tw th _ _)  <- textExtents s
    let w      = tw + bx
        h      = th
        len'   = ifWidthElse tc w h
        scaleF = len/len'
        fractionalFontSize = scaleF*initFontSize
        fontSize = fromIntegral $ floor $ fractionalFontSize
    if len' > 0
      then do
        setFontSize fontSize
        (TextExtents bx' by' tw' th' _ _) <- textExtents s
        let w' = tw' + bx'
            h' = th'
            dx = -w'/2        -- distance to move in x direction right
            dy = -by' - th'/2 -- distance to move in y direction up
        -- _consoleLog $ printf "'%s', len=%.2f, w'=%.2f, h'=%.2f" s len w' h'
        return $ FontInfo { fiFontSize = fontSize
                          , fiDx = dx
                          , fiDy = dy
                          , fiWidth  = w' -- ifWidthElse tc len w'
                          , fiHeight = h' -- ifWidthElse tc h' len
                          }
      else return $ FontInfo 0 0 0 0 0




----------------------------------------------------------------------------------------------------
fontInfoForWidth, _fontInfoForHeight :: FontFace -> Double -> String -> Render FontInfo
fontInfoForWidth  = fontInfoForConstraint Width
_fontInfoForHeight = fontInfoForConstraint Height

----------------------------------------------------------------------------------------------------
--
-- This function is a bit of a hack. The use case is [textExtents] which returns
-- a data structure describing the dimensions of some rendered text.
-- The problem I had is that there's no way to get it out of the [Render] monad!
--
-- This sneaky little function will render the text to a 1x1 buffer in memory and return the
-- data structure inside the IO monad.
--
runWithoutRender :: Render a -> IO a
runWithoutRender r =
  allocaBytes (bytesPerWord32*n*n) $ \buffer -> do
    withImageSurfaceForData buffer FormatARGB32 n n (n*bytesPerWord32) $ \surface -> do
      renderWith surface r
  where
    n = 1
    bytesPerWord32 = 4

----------------------------------------------------------------------------------------------------
--
-- Use [consoleLog] to print out values to the console inside the [Render] monad.
--
_consoleLog :: String -> Render ()
_consoleLog = liftIO . debugLog