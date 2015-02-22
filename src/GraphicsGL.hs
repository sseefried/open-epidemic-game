{-# LANGUAGE ScopedTypeVariables #-}
module GraphicsGL (
    -- functions
    germGfxToGermGL, drawTextOfWidth, drawTextOfHeight, drawTextOfWidth_, drawTextOfHeight_,
    drawTextLinesOfWidth, drawTextLinesOfWidth_, drawLetterBox, drawAntibiotic,
    renderQuadWithColor
  ) where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.OpenGL.Raw
import           Foreign.Marshal.Alloc (allocaBytes, alloca)
import           Foreign.Marshal.Array (allocaArray, pokeArray)
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Word (Word8)
import           Control.Monad
import           Text.Printf


import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V

-- friends
import Types
import Graphics
import Platform

----------------------------------------------------------------------------------------------------
rgbFormat :: GLint
rgbFormat = fromIntegral $ case platform of
  Android     -> gl_BGRA
  IOSPlatform -> gl_RGBA
  _           -> gl_RGBA

----------------------------------------------------------------------------------------------------
-- Resolution of largest texture for mipmap is 2^powOfTwo
powOfTwo :: Int
powOfTwo = 7

----------------------------------------------------------------------------------------------------
bytesPerWord32 :: Int
bytesPerWord32 = 4

----------------------------------------------------------------------------------------------------
floatSize :: Int
floatSize = sizeOf (undefined :: GLfloat)

----------------------------------------------------------------------------------------------------
genTexture :: IO TextureId
genTexture =
  alloca $ \(ptr :: Ptr GLuint) -> do { glGenTextures 1 ptr; peek ptr }

----------------------------------------------------------------------------------------------------
--
-- Frees the [textureId] for reuse and deletes any bound textures.
--
delTexture :: TextureId -> IO ()
delTexture textureId =
  alloca $ \(ptr :: Ptr GLuint) -> do { poke ptr textureId; glDeleteTextures 1 ptr }

----------------------------------------------------------------------------------------------------
{-# INLINE forN #-}
forN :: Int -> Int -> (Int -> IO ()) -> IO ()
forN n i f | i < n = f i >> forN n (i+1) f
           | otherwise = return ()

{-# INLINE clearBuffer #-}
--
-- Clears a buffer of n 32-bit words to a particular color.
--
clearBuffer :: Color -> Int -> Ptr a -> IO ()
clearBuffer (Color r g b a) n ptr = do
  let for = forN n
  for 0 $ \i -> do
    let pk off x = pokeByteOff ptr (i*bytesPerWord32+off) (toColorWord x)
    pk 0 r
    pk 1 g
    pk 2 b
    pk 3 a
  where
    toColorWord :: Double -> Word8
    toColorWord x = floor (x*255.0)


----------------------------------------------------------------------------------------------------
renderCairoToTexture :: TextureId -> Maybe MipMapIndex -> (Int, Int) -> Render a -> IO a
renderCairoToTexture textureId mbIdx (w,h) cairoRender = do
  allocaBytes (w*h*bytesPerWord32) $ \buffer -> do
    -- zero the buffer
    clearBuffer backgroundColor (w*h) buffer
    res <- C.withImageSurfaceForData buffer C.FormatARGB32 w h (w*bytesPerWord32) $ \surface -> do
      C.renderWith surface cairoRender
    glBindTexture gl_TEXTURE_2D textureId
    glTexImage2D gl_TEXTURE_2D index rgbFormat w' h' 0 gl_BGRA gl_UNSIGNED_BYTE buffer
    return res
  where
    w' = fromIntegral w
    h' = fromIntegral h
    index = maybe 0 id mbIdx

----------------------------------------------------------------------------------------------------
--
-- If you're not using the texture anymore it must be freed.
--
_renderCairoToNewTexture :: (Int, Int) -> Render () -> IO TextureId
_renderCairoToNewTexture dims r =
  do { tid <- genTexture; renderCairoToTexture tid Nothing dims r; return tid }

----------------------------------------------------------------------------------------------------
--
-- Generates a new texture, does something with it, frees it.
--
withNewTexture :: (TextureId -> IO a) -> IO a
withNewTexture f = do
  textureId <- genTexture
  res <- f textureId
  delTexture textureId
  return res

----------------------------------------------------------------------------------------------------
--
-- Best for one-off renders that will not be updated regularly on the screen and keep the same
-- size (we're not using mipmapping).
--
--
-- The quad will be *centred* at (x',y')
-- The relative co-ordinate system for the Cairo graphics will have its origin at the
-- *centre* of the quad.
--
renderCairoToQuad :: (Double, Double) -> (Double, Double) -> Render a -> GLM a
renderCairoToQuad (x',y') (w',h') cairoRender  = GLM $ \glslAttrs -> do
  --
  -- Since Cairo must render to a texture buffer (which is an integral number of pixels)
  -- we take the ceiling of [w] and [h] and use that as our bounds.
  -- We then scale the [cairoRender] by that amount (which will be very close to 1.)
  --
  let positionIdx = glslPosition glslAttrs
      texCoordIdx = glslTexcoord glslAttrs
      drawTextureLoc = glslDrawTexture glslAttrs
      (x,y,w,h) = (f2gl (x' - w'/2), f2gl (y' - h'/2), f2gl w', f2gl h')
      scale     = realToFrac . screenScale . glslOrthoBounds $ glslAttrs
      cw        = w' * scale
      ch        = h' * scale
      wi        = ceiling cw
      hi        = ceiling ch
      sx        = (fromIntegral wi)/cw
      sy        = (fromIntegral hi)/ch
      ptsInPos  = 3
      ptsInTex  = 2
      ptsInQuad = 4
      (ptsInPos', ptsInTex') = (fromIntegral ptsInPos, fromIntegral ptsInTex)
      perVertex = ptsInPos + ptsInTex
      stride    = fromIntegral $ perVertex*floatSize
  withNewTexture $ \tid -> do
    res <- renderCairoToTexture tid Nothing (wi, hi) $ do
      C.scale (sx*scale) (sy*scale)
      C.translate (w'/2) (h'/2)
      cairoRender

    glBindTexture gl_TEXTURE_2D tid
    glUniform1i drawTextureLoc 1 -- set to 'true'

    --
    -- On GL ES 2.0 with "non power of two" width textures (as these ones usually
    -- are) you must set the texture wrap parameters below to "clamp to edge"
    --
    glTexParameteri gl_TEXTURE_2D  gl_TEXTURE_WRAP_S (fromIntegral gl_CLAMP_TO_EDGE)
    glTexParameteri gl_TEXTURE_2D  gl_TEXTURE_WRAP_T (fromIntegral gl_CLAMP_TO_EDGE)

    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)

    glEnableVertexAttribArray (glslPosition glslAttrs)
    glEnableVertexAttribArray (glslTexcoord glslAttrs)


    allocaArray (ptsInQuad*perVertex*floatSize) $ \(vs :: Ptr GLfloat) -> do
      pokeArray vs [ x  , y  , zMax, 0, 0  -- bottom-left
                   , x+w, y  , zMax, 1, 0  -- upper-left
                   , x  , y+h, zMax, 0, 1  -- bottom-right
                   , x+w, y+h, zMax, 1, 1  -- upper-right
                   ]
      glVertexAttribPointer positionIdx ptsInPos' gl_FLOAT (fromIntegral gl_FALSE) stride vs
      glVertexAttribPointer texCoordIdx ptsInTex' gl_FLOAT (fromIntegral gl_FALSE) stride
                                    (vs `plusPtr` (ptsInPos*floatSize))
      glDrawArrays gl_TRIANGLE_STRIP 0 (fromIntegral ptsInQuad)
      return res

----------------------------------------------------------------------------------------------------
renderQuadWithColor :: (Double, Double) -> (Double, Double) -> Color -> GLM ()
renderQuadWithColor (x,y) (w, h) (Color r g b a) = GLM $ \glslAttrs -> do
  let positionLoc = glslPosition glslAttrs
      drawTextureLoc = glslDrawTexture glslAttrs
      colorLoc       = glslColor glslAttrs
      ptsInPos  = 3
      ptsInQuad = 4
      i2i = fromIntegral
      f2f = realToFrac
      (x',y',w',h') = (f2f x, f2f y, f2f w, f2f h)

  glUniform1i drawTextureLoc 0 -- set to 'false'
  glUniform4f colorLoc (f2f r) (f2f g) (f2f b) (f2f a) -- set the color

  glEnableVertexAttribArray (glslPosition glslAttrs)
  allocaArray (ptsInQuad*ptsInPos*floatSize) $ \(vs :: Ptr GLfloat) -> do
    pokeArray vs [ x',    y'   , zMax  -- bottom-left
                 , x'+w', y'   , zMax  -- upper-left
                 , x'   , y'+h', zMax  -- bottom-right
                 , x'+w', y'+h', zMax  -- upper-right
                 ]
    glVertexAttribPointer positionLoc (i2i ptsInPos) gl_FLOAT (fromIntegral gl_FALSE) 0 vs
    glDrawArrays gl_TRIANGLE_STRIP 0 (i2i ptsInQuad)

----------------------------------------------------------------------------------------------------
f2gl :: Double -> GLfloat
f2gl = realToFrac

----------------------------------------------------------------------------------------------------
drawToMipmapTexture :: (Double -> C.Render ()) -> IO TextureId
drawToMipmapTexture renderFun = do
  textureId <- genTexture
  glBindTexture gl_TEXTURE_2D textureId
  -- gl_TEXTURE_MIN_FILTER accepts gl_NEAREST, gl_LINEAR, gl_NEAREST_MIPMAP_NEAREST,
  -- gl_NEAREST_MIPMAP_LINEAR, gl_LINEAR_MIPMAP_NEAREST or gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR_MIPMAP_LINEAR)
  -- gl_TEXTURE_MAG_FILTER accepts gl_NEAREST or gl_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
  forM_ (zip textureWidths [0..]) $ \(x,i) -> do
    renderCairoToTexture textureId (Just i) (x,x) $ renderFun (fromIntegral x/2)
  return textureId
  where
    textureWidths = map (2^) [powOfTwo, powOfTwo-1..0]
----------------------------------------------------------------------------------------------------
--
-- [rep] is used to create some missing triangles for the germ polygon. These triangles
-- go "around" the star polygon.
--

rep :: [a] -> [a]
rep [] = []
rep (x:xs) = (x:repEven xs) ++ [x]

repOdd :: [a] -> [a]
repOdd [] = []
repOdd (x:xs) = x:x:repEven xs

repEven :: [a] -> [a]
repEven [] = []
repEven (x:xs) = x:repOdd xs

--
-- Maps over a vector providing the index of the vector element to the mapping function.
--
forMi_ :: (Monad m, Unbox a) => Vector a -> (Int -> a -> m b) -> m ()
forMi_ v f = V.foldM' f' 0 v >> return ()
  where
    f' i a = f i a >> return (i+1)

----------------------------------------------------------------------------------------------------
--
-- This function is reponsible for drawing a wiggling germ.
--
-- The basic idea is to draw a polygon, set the texture co-ordinates correctly and then
-- "wiggle" the polygon vertices without varying the texture co-ordinates. This causes
-- the texture to deform leading to a wiggling effect.
--
-- The [GermGfx] structure contains lists of [MovingPoints]s. There are two functions
-- that operate on a [MovingPoint]. The first is [movingPtToPt]. This takes a time argument
-- and gives the position of the point at that time. The other is [movingPtToStaticPt] which
-- returns the position of the point at time zero. This is used for the texture co-ordinates,
-- while [movingPtToPt] is used for the polygon vertices.
--
germGfxToGermGL :: GermGfx -> GLM GermGL
germGfxToGermGL gfx = GLM $ const $ do
  textureId <- drawToMipmapTexture (germGfxRenderBody gfx)
  --
  -- We pre-allocate a bunch of unboxed vectors (from Data.Vector). (Data.Vector uses
  -- the type family trick to change arrays of tuples to tuples of arrays.)
  -- This means the performance is quite decent.
  --
  let germPts         = germGfxBody gfx
      len             = length germPts
      fanPts          = V.fromList $ ((0,(0,1,0)), (0,(0,1,0))):(take (len+1) $ cycle germPts)
      triPts          = V.fromList $ rep germPts
      lenTri          = V.length triPts
      ptsInPos        = 3
      ptsInTex        = 2
      (ptsInPos', ptsInTex') = (fromIntegral ptsInPos, fromIntegral ptsInTex)
      perVertex = ptsInPos + ptsInTex
      stride = fromIntegral $ perVertex * floatSize
      germGLFun = \zIndex (R2 x' y') t r scale -> GLM $ \glslAttrs -> do
        let positionIdx    = glslPosition glslAttrs
            texCoordIdx    = glslTexcoord glslAttrs
            drawTextureLoc = glslDrawTexture glslAttrs

        glBindTexture gl_TEXTURE_2D textureId
        glUniform1i drawTextureLoc 1 -- set to 'true'

        glEnableVertexAttribArray (glslPosition glslAttrs)
        glEnableVertexAttribArray (glslTexcoord glslAttrs)

        -- Create a star polygon.
        let drawPolys n arrayType movingPts = do
              allocaArray (n*perVertex) $ \(vertices :: Ptr Float) -> do
                forMi_ movingPts $ \i movingPt -> do
                  let (x,y)    = movingPtToStaticPt movingPt
                      (mx, my) = movingPtToPt t scale movingPt
                      vx       = (r*x + x')
                      vy       = (r*y + y')
                      vz       = fromIntegral zIndex * 0.001
                      tx       = (mx+1)/2
                      ty       = (my+1)/2
                      base     = i*perVertex*floatSize
                      texBase  = base + ptsInPos*floatSize
                      pk :: Int -> Double -> IO ()
                      pk off x = pokeByteOff vertices off (f2gl x)
                  pk base                vx
                  pk (base+  floatSize)  vy
                  pk (base+2*floatSize)  vz
                  pk texBase             tx
                  pk (texBase+floatSize) ty
                glVertexAttribPointer positionIdx ptsInPos' gl_FLOAT (fromIntegral gl_FALSE) stride
                                      vertices
                glVertexAttribPointer texCoordIdx ptsInTex' gl_FLOAT (fromIntegral gl_FALSE) stride
                                      (vertices `plusPtr` (ptsInPos*floatSize))
                glDrawArrays arrayType 0 (fromIntegral n)
        drawPolys (len+2) gl_TRIANGLE_FAN fanPts
        -- Add extra triangles in the "valleys" of the star to turn this into an n-gon. (Needed
        -- because there is texture to be drawn in these valleys.)
        drawPolys lenTri gl_TRIANGLES triPts
      finaliser = GLM . const $ delTexture textureId
  return $ GermGL germGLFun finaliser

----------------------------------------------------------------------------------------------------
--
-- Draw antibiotic *centred* at (x, y)
--
drawAntibiotic :: R2 -> Double -> GLM ()
drawAntibiotic (R2 x y) resistance = do
  let s = antibioticWidth
      r = s/2
  st <- getGLSLState
  renderCairoToQuad (x,y) (s,s) $ do
    C.setSourceRGBA 0.5 0.5 0.5 1 -- grey -- FIXME: Colour dependent on antibiotic
    circle (0,0) r
    C.fill
    textOfWidth_ (glslFontFace st) (Color 0 0 0 1, Color 1 1 1 1) -- FIXME: Make a constant
      (0,0) (s*0.8) (printf "%3.1f%%" $ resistance * 100.0)

----------------------------------------------------------------------------------------------------
drawText :: TextConstraint -> Gradient -> R2 -> Double -> String -> GLM Double
drawText tc grad (R2 x y) len s = do
  st <- getGLSLState
  let textR :: Render Double
      textR = textConstrainedBy tc (glslFontFace st) grad (0,0) len s
  lenD <- liftGLM $ runWithoutRender textR
  let (w',h') = case tc of
                  Width  -> (len,  lenD)
                  Height -> (lenD, len)
  renderCairoToQuad (x, y) (w',h') $ textR


----------------------------------------------------------------------------------------------------
drawTextOfWidth, drawTextOfHeight :: Gradient -> R2 -> Double -> String -> GLM Double
drawTextOfWidth  = drawText Width
drawTextOfHeight = drawText Height

----------------------------------------------------------------------------------------------------
drawTextOfWidth_, drawTextOfHeight_ :: Gradient -> R2 -> Double -> String -> GLM ()
drawTextOfWidth_ a b c d = drawText Width a b c d >> return ()
drawTextOfHeight_ a b c d= drawText Height a b c d >> return ()

----------------------------------------------------------------------------------------------------
drawTextLinesOfWidth :: Color -> R2 -> Double -> [String] -> GLM Double
drawTextLinesOfWidth color (R2 x y) w ss = do
  st <- getGLSLState
  let textR :: Render Double
      textR = textLinesOfWidth (glslFontFace st) color (0,0) w ss
  h <- liftGLM $ runWithoutRender textR
  renderCairoToQuad (x, y) (w,h) $ textR

----------------------------------------------------------------------------------------------------
drawTextLinesOfWidth_ :: Color -> R2 -> Double -> [String] -> GLM ()
drawTextLinesOfWidth_ a b c d = drawTextLinesOfWidth a b c d >> return ()

----------------------------------------------------------------------------------------------------
drawLetterBox :: (Double, Double) -> (Double, Double) -> GLM ()
drawLetterBox pos (w,h) =
  when (w > 0 && h > 0 ) $ renderQuadWithColor pos (w,h) (Color 0 0 0 1)
