module FontTest where

import           Foreign.Marshal.Alloc (allocaBytes)
import           Graphics.Rendering.Cairo
import           Text.Printf
-- friends
import           FreeType


----------------------------------------------------------------------------------------------------
test :: String -> IO ()
test s = do
  ff <- loadFontFace
  pp <- runWithoutRender $ do
    setFontSize 1000
    setFontFace ff
    (TextExtents bx by tw th ax ay) <- textExtents s
    return $ printf "%.2f %.2f %.2f %.2f %.2f %.2f\n" bx by tw th ax ay
  putStrLn pp

----------------------------------------------------------------------------------------------------
runWithoutRender :: Render a -> IO a
runWithoutRender r =
  allocaBytes bytesPerWord32 $ \buffer -> do
    withImageSurfaceForData buffer FormatARGB32 1 1 bytesPerWord32 $ \surface -> do
      renderWith surface r
  where
    bytesPerWord32 = 4
