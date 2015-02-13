{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
--
-- This module is used in the Android and iOS build
--
module AndroidMain where

import Foreign.C.String

-- friends
import Game
import Backend.SDL as B

foreign export ccall "haskell_main" main :: CString -> IO ()

main :: CString -> IO ()
main cstr = do
  resourcePath <- peekCString cstr
  besRef <- B.initialize "Epidemic" (Just resourcePath)
  B.mainLoop besRef handleEvent