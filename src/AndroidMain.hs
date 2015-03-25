{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module AndroidMain where



-- friends
import Game
import Backend.SDL as B
import Foreign
import CUtil
import ProfileGraphics

foreign export ccall "haskell_main" main :: CString -> IO ()

main :: CString -> IO ()
main cstr = do
  resourcePath <- peekCString cstr
  if profilingGraphics then profileGraphics (Just resourcePath) else gameMain resourcePath

gameMain :: String -> IO ()
gameMain resourcePath = do
  besRef <- B.initialize "Epidemic" (Just resourcePath)
  B.mainLoop besRef handleEvent