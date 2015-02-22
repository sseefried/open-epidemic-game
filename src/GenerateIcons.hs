module Main where

import Graphics.Rendering.Cairo
import Text.Printf
import Control.Monad.Random
import Control.Monad

-- friends
import Graphics

type Opacity = Maybe Double -- Nothing means opaque

generateIcons :: [(Int, Opacity)] -> IO ()
generateIcons args = do
  gfx     <- evalRandIO randomGermGfx
  forM_ args $ \(size, opacity) -> do
    let size' = fromIntegral size
        filePath = printf "icon_%dx%d.png" size size
        opacity' = maybe 1 id opacity
    printf "Generating '%s'\n" filePath

    let r = do

               germGfxRenderGerm gfx (size'/2)

               setOperator OperatorDestOut

               setSourceRGBA 1 1 1 (1-opacity')
               rectangle 0 0 size' size'
               fill

    surface <- createImageSurface FormatARGB32 size size
    renderWith surface r
    surfaceWriteToPNG surface filePath

main :: IO ()
main = do
  generateIcons ((1200, Just 0.10):zip [512, 144, 120, 114, 96, 80, 72, 57, 58, 29, 48] (repeat Nothing))


