{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module FreeType where

-- import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Graphics.Rendering.Cairo.Types (FontFace, mkFontFace)


foreign import ccall "load_font_face" cLoadFontFace :: CString -> IO (Ptr FontFace)

loadFontFace :: String -> IO FontFace
loadFontFace s = withCString s $ \path -> cLoadFontFace path >>= mkFontFace

