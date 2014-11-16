{-# LANGUAGE ForeignFunctionInterface #-}
module CUtil where

import Foreign.C.Types

--
-- When compiling the GHC ARM cross-compiler with LLVM 3.0 it produces incorrect
-- assembly code for the function __decodeFloat_Int in StgPrimFloat.c in the GHC source.
-- decodeFloat. (See https://ghc.haskell.org/trac/ghc/ticket/9125.)
-- It's going to be way too hard for me, a mere mortal, to fix this compiler bug so I'm
-- just going to do everything with Double`s which work fine.
--

foreign import ccall "float2double" cFloatToCDouble :: CFloat -> CDouble

cFloatToDouble :: CFloat -> Double
cFloatToDouble = uncurry encodeFloat . decodeFloat . cFloatToCDouble

