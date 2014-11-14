{-# LANGUAGE CPP #-}
module Platform where

data Platform = MacOSX
              | IOS
              | Android

#ifdef ANDROID
platform = Android
#else
#ifdef IOS
platform = IOS
#else
platform = MacOSX
#endif /* IOS */
#endif /* ANDROID */
