{-# LANGUAGE CPP #-}
module Platform where

import CUtil

data Platform = MacOSX
              | IOSPlatform
              | Android
              | NoSound deriving (Eq, Show)

debugGame :: Bool
#ifdef DEBUG
debugGame = True
#else
debugGame = False
#endif


#ifdef ANDROID
platform = Android
#else
#ifdef IOS
platform = IOSPlatform
#else
#ifdef NOSOUND
platform = NoSound
#else
platform = MacOSX
#endif /* IOS */
#endif /* NOSOUND */
#endif /* ANDROID */

isMobile, isDesktop :: Bool
isMobile = platform `elem` [IOSPlatform, Android]

isDesktop = not isMobile


debugLog :: String -> IO ()
debugLog = case platform of
  Android -> androidLog
  _       -> putStrLn

--
-- [Nothing] means fullscreen. [Just (w,h)] means set screen size to width [w] and height [h]
--
screenDimensions :: Maybe (Int,Int)
screenDimensions = case platform of
  Android     -> Nothing
  IOSPlatform -> Nothing
  _           -> Just (1280, 720)
