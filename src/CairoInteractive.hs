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
import FreeType

width, height :: Int
width = 960
height = 640

width', height' :: Double
width' = fromIntegral width
height' = fromIntegral height

displayCairo :: Render a -> IO a
displayCairo r = do
  (tmpFile, h) <- mkstemps "/tmp/cairo-image" ".png" -- cairo-imageXXXXXX.png
  hClose h
  surf <- createImageSurface FormatARGB32 width height
  res <- renderWith surf $ do
    let s = fromIntegral $ min width height
    transform $ M.Matrix 1 0 0 (-1) 0 0
    translate (fromIntegral width/2) (-fromIntegral height/2)
    r
  surfaceWriteToPNG surf tmpFile
  system $ printf "open -a Preview %s" tmpFile
  return res


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

fontTest :: Render ()
fontTest = do
  let s = "SCORE:"
      fontSize = 1000
  ff <- liftIO $ loadFontFace "../assets/font.ttf"
  save
  let len = 114
  setSourceRGBA 0.9 1 0.9 1
  rectangle (-len/2) (-height'/2) len (height')
  fill
  ----

  setSourceRGBA 0 0 0 1
  setFontFace ff
  setFontSize fontSize
  (TextExtents bx _ tw th ax _) <- textExtents s
  let len' = ax -- tw + bx
      scaleF = len/len'
  liftIO $ printf "tw+bx=%.2f, th=%.2f, fontSize=%.2f, height=%.2f scaleF=%f\n"
             (tw+bx) th (scaleF*fontSize) (th*scaleF) (scaleF)
  setFontSize (scaleF*fontSize)
  transform $ M.Matrix 1 0 0 (-1) 0 0
  moveTo (-len/2) 0
  textPath s
  fillPreserve


--  textOfWidth_ ff (Color 0 0 0 1, Color 0 0 0 1) (0,0) len s
  restore

fontTest2 :: Render ()
fontTest2 = do
  let s = "SCORE:"
      fontSize = 1000
  ff <- liftIO $ loadFontFace "../assets/font.ttf"
  save

  ----
  setFontFace ff
  setFontSize 38.4
  (TextExtents bx _ tw th ax _) <- textExtents s
  let len = tw + bx
  liftIO $ printf "tw+bx=%.2f\n" (tw+bx)

  setSourceRGBA 0.9 1 0.9 1

  rectangle (-len/2) (-height'/2) len (height')
  fill

  setSourceRGBA 0 0 0 1

  transform $ M.Matrix 1 0 0 (-1) 0 0
  moveTo (-len/2) 0
  textPath s
  fillPreserve
  restore

fontInfoTest :: Render ()
fontInfoTest = do
  ff <- liftIO $ loadFontFace "../assets/font.ttf"
  fi <- fontInfoForWidth ff 100 ""
  liftIO $ putStrLn $ show fi
  return ()


-------
--
-- We're going to be lazy and just sample at the midpoint.
-- This is 1-dimensional. We use it in a 2-pass filter
--
