{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module FreeType where

import Graphics.Rendering.Cairo.Types (FontFace, mkFontFace)

-- friends
import Foreign



foreign import ccall "load_font_face" cLoadFontFace :: CString -> IO (Ptr FontFace)

loadFontFace :: String -> IO FontFace
loadFontFace s = withCString s $ \path -> cLoadFontFace path >>= mkFontFace

