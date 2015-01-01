module Util where

import System.Exit

exitWithError :: String -> IO a
exitWithError errorStr = putStrLn errorStr >> exitWith (ExitFailure 1)

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational


toInt :: Integral a => a -> Int
toInt = fromIntegral