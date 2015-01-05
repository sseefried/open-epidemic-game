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

--
-- Takes two lists, not necessarily of the same length.
-- Returns a list that alternates between elements of the
-- first list and the second list up to the point where
-- the shorter list runs out of values. The remaning elements
-- are from the longer list.
--
-- Examples:
--
-- alternate [1,2]     [10,20]     == [1,10,2,20]
-- alternate [1,2]     [10,20,30]  == [1,10,2,20,30]
-- alternate [1,2,3,4] [10,20]     == [1,10,2,20,3,4]
--
alternate :: [a] -> [a] -> [a]
alternate [] ys = ys
alternate xs [] = xs
alternate (x:xs) (y:ys) = x:y:alternate xs ys

--
-- Like mod but for RealFrac's
--
fmod :: RealFrac a => a -> a -> a
fmod a b = snd (properFraction (a / b)) * b
