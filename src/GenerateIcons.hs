module Main where

import Graphics.Rendering.Cairo
import Text.Printf
import Control.Monad.Random
import Control.Monad

-- friends
import Graphics

generateIcons :: [Int] -> IO ()
generateIcons sizes = do
  gfx     <- evalRandIO randomGermGfx
  forM_ sizes $ \size -> do
    let size' = fromIntegral size
        filePath = printf "icon_%dx%d.png" size size
    printf "Generating '%s'\n" filePath
    let r = drawGerm gfx (size,size) (size'/2, size'/2) (size'/2) 0
    surface <- createImageSurface FormatARGB32 size size
    renderWith surface r
    surfaceWriteToPNG surface filePath

main :: IO ()
main = generateIcons [144, 96, 72, 48]


