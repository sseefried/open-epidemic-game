{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module CUtil where

import Foreign.C.Types
import Foreign.C.String

--
-- When compiling the GHC ARM cross-compiler with LLVM 3.0 it produces incorrect
-- assembly code for the function __decodeFloat_Int in StgPrimFloat.c in the GHC source.
-- decodeFloat. (See https://ghc.haskell.org/trac/ghc/ticket/9125.)
-- It's going to be way too hard for me, a mere mortal, to fix this compiler bug so I'm
-- just going to do everything with Double`s which work fine.
--

foreign import ccall "float2double" cFloatToCDouble :: CFloat -> CDouble
foreign import ccall "set_no_buffering" setNoBuffering ::  IO ()
#ifdef ANDROID
foreign import ccall "androidLog" cAndroidLog :: CString -> IO ()
#endif

cFloatToDouble :: CFloat -> Double
cFloatToDouble = uncurry encodeFloat . decodeFloat . cFloatToCDouble

androidLog :: String -> IO ()
#ifdef ANDROID
androidLog s = withCString s $ \cstr -> cAndroidLog cstr
#else
-- dummy function for when we're not building on Android
androidLog _ = return ()
#endif

resourcePath :: IO String
#ifdef ANDROID
resourcePath = return "unknown"
#else
foreign import ccall "resource_path" cResourcePath :: IO CString
resourcePath = do
  cstr <- cResourcePath
  peekCString cstr

#endif