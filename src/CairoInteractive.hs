module CairoInteractive where

import System.Directory
import System.Posix.Temp
import System.Process
import System.IO
import Text.Printf
import Graphics.Rendering.Cairo hiding (width, height)
import qualified Graphics.Rendering.Cairo.Matrix as M

-- friends
import Types
import Graphics


width, height :: Int
width = 512
height = width

displayCairo :: Render () -> IO ()
displayCairo r = do
  (tmpFile, h) <- mkstemps "/tmp/cairo-image" ".png" -- cairo-imageXXXXXX.png
  hClose h
  surf <- createImageSurface FormatARGB32 width height
  renderWith surf $ do
    let s = fromIntegral $ min width height
    transform $ M.Matrix 1 0 0 (-1) 0 0
    translate (fromIntegral width/2) (-fromIntegral height/2)
    scale s s
    r
  surfaceWriteToPNG surf tmpFile
  system $ printf "open -a Preview %s" tmpFile
  return ()

test :: Render ()
test = do
  setSourceRGBA 1 0 0 1
  arc 0 0 0.5 0 pi
  fill
  setSourceRGBA 0 1 0 1
  setLineWidth 0.01
  moveTo 0 0
  lineTo 0.5 0.5
  stroke




