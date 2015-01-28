{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module FreeType where

-- import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.Cairo.Types (FontFace, mkFontFace)


foreign import ccall "load_font_face" cLoadFontFace :: IO (Ptr FontFace)

loadFontFace :: IO FontFace
loadFontFace = cLoadFontFace >>= mkFontFace

