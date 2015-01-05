{-# LANGUAGE ScopedTypeVariables #-}
module GraphicsGL where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.OpenGL.Raw
import           Text.Printf
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Marshal.Array (mallocArray, allocaArray, pokeArray, peekArray)
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Storable
import           Control.Monad
import           Util

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-- friends
import Types
import Graphics

-- Resolution of largest texture for mipmap is 2^powOfTwo
powOfTwo :: Int
powOfTwo = 7

bytesPerWord32 :: Int
bytesPerWord32 = 4


----------------------------------------------------------------------------------------------------
-- FIXME: For even more speed perhaps pre-allocate [maxGerms] of these buffers and re-use them.
--
drawToTexture :: (Double -> C.Render ()) -> IO TextureId
drawToTexture renderFun = do
  textureId <- allocaArray 1 $ \textures -> do
    glGenTextures 1 (textures :: Ptr GLuint)
    [textureId] <- peekArray 1 textures
    return textureId
  glBindTexture gl_TEXTURE_2D textureId

  -- gl_TEXTURE_MIN_FILTER accepts gl_NEAREST, gl_LINEAR, gl_NEAREST_MIPMAP_NEAREST,
  -- gl_NEAREST_MIPMAP_LINEAR, gl_LINEAR_MIPMAP_NEAREST or gl_LINEAR_MIPMAP_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR_MIPMAP_LINEAR)
  -- gl_TEXTURE_MAG_FILTER accepts gl_NEAREST or gl_LINEAR
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
  forM_ (zip textureWidths [0..]) $ \(x,i) -> do
    let x' = fromIntegral x
        xd = fromIntegral x
    allocaBytes (x*x*bytesPerWord32) $ \buffer -> do
      C.withImageSurfaceForData buffer C.FormatARGB32 x x (x*4) $ \surface ->
         C.renderWith surface $ do
           C.setSourceRGBA 1 1 1 1
           C.rectangle 0 0 xd xd
           C.fill
           renderFun (fromIntegral x/2)
      glTexImage2D gl_TEXTURE_2D i (fromIntegral gl_RGBA) x' x' 0 gl_RGBA gl_UNSIGNED_BYTE buffer
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




uForMi_ :: (Monad m, VU.Unbox a) => VU.Vector a -> (Int -> a -> m b) -> m ()
uForMi_ v f = VU.foldM' f' 0 v >> return ()
  where
    f' i a = f i a >> return (i+1)

forMi_ :: Monad m => V.Vector a -> (Int -> a -> m b) -> m ()
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
germGfxToGLFun :: GermGfx -> GLM GermGLFun
germGfxToGLFun gfx = GLM . const $ do
  textureId <- drawToTexture (germGfxRenderBody gfx)
  let germPts         = germGfxBody gfx
      len             = length germPts
      preFanPts       = (MP2 ((0,VU.empty), (0,VU.empty))):(take (len+1) $ cycle germPts)
      preFanPts'      = V.fromList preFanPts
      staticFanPts    = VU.fromList $ map movingPtToStaticPt preFanPts
      preTriPts       = rep germPts
      preTriPts'      = V.fromList preTriPts
      staticTriPts    = VU.fromList $ map movingPtToStaticPt preTriPts
      lenTri          = V.length preTriPts'

  return $ \(R2 x' y') t r  -> GLM $ \glslAttrs -> do
    let perVertex = 4 -- number of GLfloats per vertex. 2 for position, 2 for texture
        floatSize = sizeOf (undefined :: GLfloat)
        stride = fromIntegral $ perVertex * floatSize
        positionIdx = glslPosition glslAttrs
        texCoordIdx = glslTexcoord glslAttrs
    glBindTexture gl_TEXTURE_2D textureId
    glEnableVertexAttribArray (glslPosition glslAttrs)
    glEnableVertexAttribArray (glslTexcoord glslAttrs)
    -- Create a star polygon.
    let drawPolys n arrayType staticPts movingPts = do
          allocaArray (n*perVertex) $ \(vertices :: Ptr Float) -> do
            uForMi_ staticPts $ \i (x,y) -> do
              let vx = ((d2f r)*x + d2f x')
                  vy = ((d2f r)*y + d2f y')
                  base = i*perVertex*floatSize
              pokeByteOff vertices base             vx
              pokeByteOff vertices (base+floatSize) vy
            forMi_ movingPts $ \i (x,y) -> do
              let tx = (x+1)/2
                  ty = (y+1)/2
                  base = i*perVertex*floatSize
              pokeByteOff vertices (base+2*floatSize) tx
              pokeByteOff vertices (base+3*floatSize) ty
            glVertexAttribPointer positionIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride vertices
            glVertexAttribPointer texCoordIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride
              (vertices `plusPtr` (2*floatSize))
            glDrawArrays arrayType 0 (fromIntegral n)
    let n         = len + 2
        texCoords = V.map (movingPtToPt t) preFanPts'
    drawPolys n gl_TRIANGLE_FAN staticFanPts texCoords

    -- Add extra triangles in the "valleys" of the star to turn this into an n-gon. (Needed
    -- because there is texture to be drawn in these valleys.)
    let texCoords = V.map (movingPtToPt t) preTriPts'
    drawPolys lenTri gl_TRIANGLES staticTriPts texCoords
