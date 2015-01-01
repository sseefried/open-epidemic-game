module Util where

import System.Exit

-- friends
import Platform

exitWithError :: String -> IO a
exitWithError errorStr = debugLog errorStr >> exitWith (ExitFailure 1)

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational


toInt :: Integral a => a -> Int
toInt = fromIntegral