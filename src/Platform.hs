{-# LANGUAGE CPP #-}
module Platform where

data Platform = MacOSX
              | IOSPlatform
              | Android
              | NoSound deriving (Eq, Show)

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
