{-# LANGUAGE ScopedTypeVariables #-}
module GraphicsGL (
    -- functions
    germGfxToGermGL, drawText, drawLetterBox
  ) where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.OpenGL.Raw
import           Foreign.Marshal.Alloc (allocaBytes, alloca)
import           Foreign.Marshal.Array (allocaArray, pokeArray)
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Word (Word8)
import           Control.Monad


import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V

-- friends
import Types
import Graphics
import Platform

----------------------------------------------------------------------------------------------------
rgbFormat :: GLint
rgbFormat = fromIntegral $ case platform of
  Android -> gl_BGRA
  _       -> gl_RGBA

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
renderCairoToTexture :: TextureId -> Maybe MipMapIndex -> (Int, Int) -> Render () -> IO ()
renderCairoToTexture textureId mbIdx (w,h) cairoRender = do
  allocaBytes (w*h*bytesPerWord32) $ \buffer -> do
    -- zero the buffer
    clearBuffer backgroundColor (w*h) buffer
    C.withImageSurfaceForData buffer C.FormatARGB32 w h (w*bytesPerWord32) $ \surface -> do
      C.renderWith surface cairoRender
    glBindTexture gl_TEXTURE_2D textureId
    glTexImage2D gl_TEXTURE_2D index rgbFormat w' h' 0 gl_BGRA gl_UNSIGNED_BYTE buffer
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
withNewTexture :: (TextureId -> IO ()) -> IO ()
withNewTexture f = do
  textureId <- genTexture
  f textureId
  delTexture textureId

----------------------------------------------------------------------------------------------------
--
-- Best for one-off renders that will not be updated regularly on the screen and keep the same
-- size (we're not using mipmapping).
--
renderCairoToQuad :: (Double, Double) -> (Double, Double) -> Render () -> GLM ()
renderCairoToQuad (x',y') (w',h') cairoRender  = GLM $ \glslAttrs -> do
  --
  -- Since Cairo must render to a texture buffer (which is an integral number of pixels)
  -- we take the ceiling of [w] and [h] and use that as our bounds.
  -- We then scale the [cairoRender] by that amount (which will be very close to 1.)
  --
  let positionIdx = glslPosition glslAttrs
      texCoordIdx = glslTexcoord glslAttrs
      drawTextureLoc = glslDrawTexture glslAttrs
      (x,y,w,h) = (f2gl x', f2gl y', f2gl w', f2gl h')
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
    renderCairoToTexture tid Nothing (wi, hi) $ do
      C.scale (sx*scale) (sy*scale)
      cairoRender

    glBindTexture gl_TEXTURE_2D tid
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
    glUniform1i drawTextureLoc 1 -- set to 'true'

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

----------------------------------------------------------------------------------------------------
renderQuadWithColor :: (Double, Double) -> (Double, Double) -> Color -> GLM ()
renderQuadWithColor (x,y) (w, h) (Color r g b a) = GLM $ \glslAttrs -> do
  let zMax = 0
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
    let xd = fromIntegral x
    renderCairoToTexture textureId (Just i) (x,x) $ do
      C.setSourceRGBA 1 1 1 0
      C.rectangle 0 0 xd xd
      C.fill
      renderFun (fromIntegral x/2)
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
germGfxToGermGL gfx = GLM . const $ do
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
      germGLFun = \zIndex (R2 x' y') t r  -> GLM $ \glslAttrs -> do
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
                      (mx, my) = movingPtToPt t movingPt
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
drawText :: Color -> R2 -> (Double,Double) -> String -> GLM ()
drawText color (R2 x y) (w,h) s =
  renderCairoToQuad (x - w'/2, y - h'/2) (w', h') $ do
    text "Helvetica" color (w/2,h/2) w s

  where
    w' = realToFrac w
    h' = realToFrac h


----------------------------------------------------------------------------------------------------
drawLetterBox :: (Double, Double) -> (Double, Double) -> GLM ()
drawLetterBox pos (w,h) =
  when (w > 0 && h > 0 ) $ renderQuadWithColor pos (w,h) (Color 0 0 0 1)
