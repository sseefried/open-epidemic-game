--
-- Just a grab-bag of useful utilities.
--
module Util (
  module Util,
  module Control.Applicative,
  module Control.Monad,
  module System.Exit,
  module Text.Printf,
  module Data.Maybe,
  module Data.List,
  module Data.IORef
)
  where

-- re-exported modules
import Control.Applicative
import Control.Monad
import System.Exit
import Text.Printf
import Data.Maybe
import Data.List
import Data.IORef



import Data.Char (toUpper)


-- friends
import CUtil
import Platform
import Foreign (CFloat(..))

exitWithError :: String -> IO a
exitWithError errorStr = debugLog errorStr >> exitWith (ExitFailure 1)

--
-- Specialised instances of a few conversion functions.
-- Helps me with type inference and also in choosing implementations.
--

{-# INLINE x2i #-}
x2i :: Integral a => a -> Int
x2i = fromIntegral

{-# INLINE x2d #-}
x2d :: Real a => a -> Double
x2d = realToFrac

{-# INLINE d2f #-}
d2f :: Double -> Float
d2f = realToFrac

{-# INLINE f2d #-}
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

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal x = (minVal `max` x) `min` maxVal

uppercase :: String -> String
uppercase = map toUpper

--
-- Compose two monads together with [(>>)] but the return value is the return value of the first
-- monad. The return value of the second monad is thrown away.
--
(*>>) :: Monad m => m a -> m b -> m a
m *>> m' = do { r <- m; m'; return r}

--
-- Swaps pair if condition is true
--
swapOn :: (b -> Bool) -> (a,a) -> b -> (a,a)
swapOn f (x,y) b = if f b then (y,x) else (x,y)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ []     = return ([], [])
partitionM f (x:xs) = do
  b <- f x
  (as,bs) <- partitionM f xs
  return $ if b then (x:as, bs) else (as, x:bs)

----------------------------------------------------------------------------------------------------
{-# INLINE forN #-}
forN :: Int -> Int -> (Int -> IO ()) -> IO ()
forN n i f | i < n = f i >> forN n (i+1) f
           | otherwise = return ()
