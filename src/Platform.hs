{-# LANGUAGE CPP #-}
module Platform where

data Platform = MacOSX
              | IOS
              | Android
              | NoSound

#ifdef ANDROID
platform = Android
#else
#ifdef IOS
platform = IOS
#else
#ifdef NOSOUND
platform = NoSound
#else
platform = MacOSX
#endif /* IOS */
#endif /* NOSOUND */
#endif /* ANDROID */
