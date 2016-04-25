{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module CUtil where

-- friends
import Foreign


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
#else
#ifdef IOS
foreign import ccall "ns_log" cNSLog :: CString -> IO ()
#endif
#endif

cFloatToDouble :: CFloat -> Double
cFloatToDouble = uncurry encodeFloat . decodeFloat . cFloatToCDouble

androidLog, nsLog :: String -> IO ()
#ifdef ANDROID
androidLog s = withCString s $ \cstr -> cAndroidLog cstr
nsLog _ = return ()
#else
-- dummy function for when we're not building on Android
androidLog _ = return ()
#ifdef IOS
nsLog s = withCString s cNSLog
#else
nsLog _ = return ()
#endif
#endif

iOSResourcePath :: IO String
#ifdef ANDROID
iOSResourcePath = return "This is an Android build. There is no iOS resource path"
#else
#if defined(IOS) || defined(MACOSX)
foreign import ccall "ios_resource_path" ciOSResourcePath :: IO CString
iOSResourcePath = ciOSResourcePath >>= peekCString
#else
iOSResourcePath = return ""
#endif
#endif

profilingGraphics :: Bool
#ifdef PROFILE_GRAPHICS
profilingGraphics = True
#else
profilingGraphics = False
#endif

profiling :: Bool
#ifdef PROFILE
profiling = True
#else
profiling = False
#endif
