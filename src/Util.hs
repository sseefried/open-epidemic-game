module Util where

import System.Exit
import Foreign.C.Types (CFloat(..))

-- friends
import CUtil
import Platform

exitWithError :: String -> IO a
exitWithError errorStr = debugLog errorStr >> exitWith (ExitFailure 1)

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational


toInt :: Integral a => a -> Int
toInt = fromIntegral

d2f :: Double -> Float
d2f = uncurry encodeFloat . decodeFloat

f2d :: Float -> Double
f2d = cFloatToDouble . CFloat
