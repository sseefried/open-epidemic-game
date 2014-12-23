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
dropEven, dropOdd :: [a] -> [a]
dropEven [] = []
dropEven (x:xs) = x:dropOdd xs

dropOdd [] = []
dropOdd (x:xs) = dropEven xs

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
germGfxToGLFun:: GermGfx -> GLM GermGLFun
germGfxToGLFun gfx = GLM $ do
  let texCoord2f :: (Double, Double) -> IO ()
      texCoord2f (x,y) = texCoord $ TexCoord2 (realToFrac x) (realToFrac y :: GLdouble)
      vertex2f :: (Double, Double) -> IO ()
      vertex2f (x,y) = vertex $ Vertex3  (realToFrac x) (realToFrac y) (0 :: GLdouble)
  textureObj <- drawToTextureObj (germGfxRenderBody gfx)
  return $ \(R2 x' y') t r -> GLM $ do
    let bar ((mx,my),(x, y)) = do
          let (tx, ty) = ((x+1)/2,(y+1)/2)
              (vx, vy) = (r*mx + x', r*my + y')
          texCoord2f (tx,ty)
          vertex2f (vx,vy)

    textureBinding Texture2D $= Just textureObj
    let splitPts = \pt -> (movingPtToPt t pt, movingPtToStaticPt pt )
    let pts = let pts' = germGfxBody gfx
                  pts'' = take (length pts'+1) (cycle pts')
              in map splitPts pts''
    color $ Color4 1 1 1 (1 :: GLdouble)
    renderPrimitive TriangleFan $ do
      texCoord2f (0.5, 0.5); vertex2f (x',y')
      mapM_ bar pts
    renderPrimitive Triangles $ do
      mapM_ bar (map splitPts $ rep $ germGfxBody gfx)