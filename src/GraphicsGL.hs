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
  return $ \(R2 x' y') t r  -> GLM $ \glslAttrs -> do
    let bar ((x,y),(mx,my)) =
          let (tx, ty) = ((x+1)/2,(y+1)/2)
              (vx, vy) = (r*mx + x', r*my + y')
          in [vx,vy,tx,ty]
        perVertex = 4 -- number of GLfloats per vertex. 2 for position, 2 for texture
        splitPts = \pt -> (movingPtToStaticPt pt, movingPtToPt t pt)
        germPts  = germGfxBody gfx
        len      = length germPts
        floatSize = sizeOf (undefined :: GLfloat)
        stride = fromIntegral $ perVertex * floatSize
        positionIdx = glslPosition glslAttrs
        texCoordIdx = glslTexcoord glslAttrs
    glBindTexture gl_TEXTURE_2D textureId
    glEnableVertexAttribArray (glslPosition glslAttrs)
    glEnableVertexAttribArray (glslTexcoord glslAttrs)
    -- Create a star polygon.
    let vertexPts = concat $ [x',y',0.5,0.5]:(map (bar . splitPts) $ take (len+1) $ cycle germPts)
        n         = len + 2
    allocaArray (n*perVertex) $ \vertices -> do
      pokeArray vertices ({-# SCC "double2float-1" #-} map d2f vertexPts)
      glVertexAttribPointer positionIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride vertices
      glVertexAttribPointer texCoordIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride (vertices `plusPtr` (2*floatSize))
      glDrawArrays gl_TRIANGLE_FAN 0 (fromIntegral n)
    -- Add extra triangles in the "valleys" of the star to turn this into an n-gon. (Needed
    -- because there is texture to be drawn in these valleys.)
    let prePts    = map splitPts $ rep germPts
        vertexPts = concat $ map bar prePts
        n         = length prePts -- FIXME: pre-calculate
    allocaArray (n*perVertex) $ \vertices -> do
      pokeArray vertices ({-# SCC "double2float-2" #-} map d2f vertexPts)
      glVertexAttribPointer positionIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride vertices
      glVertexAttribPointer texCoordIdx 2 gl_FLOAT (fromIntegral gl_FALSE) stride (vertices `plusPtr` (2*floatSize))
      glDrawArrays gl_TRIANGLES 0 (fromIntegral n)

----------------------------------------------------------------------------------------------------
