{-# LANGUAGE CPP #-}
module Platform where

import CUtil

data Platform = MacOSX
              | IOSPlatform
              | Android
              | Linux
              -- FIXME: NoSound should not be a platform!
              | NoSound deriving (Eq, Show)

debugGame, debugSystem :: Bool
#ifdef DEBUG_GAME
debugGame = True
#else
debugGame = False
#endif

#ifdef DEBUG_SYSTEM
debugSystem = True
#else
debugSystem = False
#endif


#ifdef ANDROID
platform = Android
#else
#ifdef IOS
platform = IOSPlatform
#else
#ifdef MACOSX
platform = MacOSX
#else
#ifdef LINUX
platform = Linux
#else
#ifdef NOSOUND
platform = NoSound
#else
platform = error "This platform is not supported. See Platform.hs"
#endif /* NOSOUND */
#endif /* LINUX */
#endif /* MACOSX */
#endif /* IOS */
#endif /* ANDROID */

isMobile, isDesktop :: Bool
isMobile = platform `elem` [IOSPlatform, Android]

isDesktop = not isMobile


debugLog :: String -> IO ()
debugLog = case platform of
  Android     -> androidLog
  IOSPlatform -> nsLog
  MacOSX      -> nsLog
  _           -> putStrLn

--
-- [Nothing] means fullscreen. [Just (w,h)] means set screen size to width [w] and height [h]
--
screenDimensions :: Maybe (Int,Int)
screenDimensions = case platform of
  Android     -> Nothing
  IOSPlatform -> Nothing
  _           -> Just (1280,720)

