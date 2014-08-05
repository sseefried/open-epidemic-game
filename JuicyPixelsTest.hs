module Main where

import           Data.Vector.Storable (Vector(..))
import qualified Data.Vector.Storable as V
import           GHC.Word
import           Codec.Picture

image :: Image PixelRGBA8
image = Image { imageWidth = w, imageHeight = h, imageData = img  }
  where
    w = 512
    h = 512
    img :: Vector Word8
    img = V.generate (w*h*4) f
    f :: Int -> Word8
    f x = let n = x `div` 4
          in if elem (x `mod` 4) [0,2,3] then fracOf n else 0
    fracOf n = floor (0xFF * fromIntegral n/(fromIntegral (w*h)))


main :: IO ()
main = do
  writePng "test.png" image