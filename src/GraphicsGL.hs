{-# LANGUAGE ScopedTypeVariables #-}
module GraphicsGL where

import qualified Graphics.Rendering.Cairo as C
import           Graphics.Rendering.OpenGL
import           Text.Printf
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.C.Types
import           Control.Monad

-- friends
import Types
import Graphics

-- Resolution of largest texture for mipmap is 2^powOfTwo
powOfTwo :: Int
powOfTwo = 7

bytesPerWord32 :: Int
bytesPerWord32 = 4

----------------------------------------------------------------------------------------------------
drawToTextureObj :: (Double -> C.Render ()) -> IO TextureObject
drawToTextureObj renderFun = do
  ((textureObj :: TextureObject):_) <- genObjectNames 1
  textureBinding Texture2D $= Just textureObj
  textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
  textureFunction $= Decal
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
      texImage2D Texture2D NoProxy i RGBA' (TextureSize2D x' x') 0
            (PixelData BGRA UnsignedByte (buffer :: Ptr CUChar))
  return textureObj
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
germGfxToGLFun:: GermGfx -> GLM GermGLFun
germGfxToGLFun gfx = GLM $ do
  let texCoord2f :: (Double, Double) -> IO ()
      texCoord2f (x,y) = texCoord $ TexCoord2 (realToFrac x) (realToFrac y :: GLdouble)
      vertex2f :: (Double, Double) -> IO ()
      vertex2f (x,y) = vertex $ Vertex3  (realToFrac x) (realToFrac y) (0 :: GLdouble)
  textureObj <- drawToTextureObj (germGfxRenderBody gfx)
  return $ \(R2 x' y') t r -> GLM $ do
    let bar ((x,y),(mx,my)) = do
          let (tx, ty) = ((x+1)/2,(y+1)/2)
              (vx, vy) = (r*mx + x', r*my + y')
          texCoord2f (tx,ty)
          vertex2f (vx,vy)

    textureBinding Texture2D $= Just textureObj
    let splitPts = \pt -> (movingPtToStaticPt pt, movingPtToPt t pt)
    let pts = let pts' = germGfxBody gfx
                  pts'' = take (length pts'+1) (cycle pts')
              in map splitPts pts''
    color $ Color4 1 1 1 (1 :: GLdouble)
    -- Create a star polygon.
    renderPrimitive TriangleFan $ do
      texCoord2f (0.5, 0.5); vertex2f (x',y')
      mapM_ bar pts
    -- Add extra triangles in the "valleys" of the star to turn this into an n-gon. (Needed
    -- because there is texture to be drawn in these valleys.)
    renderPrimitive Triangles $ do
      mapM_ bar (map splitPts $ rep $ germGfxBody gfx)