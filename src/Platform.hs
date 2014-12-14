{-# LANGUAGE CPP #-}
module Platform where

data Platform = MacOSX
              | IOSPlatform
              | Android
              | NoSound

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
