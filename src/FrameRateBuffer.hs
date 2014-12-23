module FrameRateBuffer (
  FRBuf,
  initFRBuf, addTick, averageTick
) where

import Data.Array.IO
-- import Data.Array.MArray
import Data.IORef
import Text.Printf
import Control.Monad (forM_)

data FR = FR { tickIndex :: Int
             , ticks     :: IOUArray Int Double
             , tickSum   :: Double
             , tickSamples :: Int }

type FRBuf = IORef FR


windowSize :: Int
windowSize = 100

initFRBuf :: IO FRBuf
initFRBuf = do
  a <- newArray (0,windowSize - 1) 0
  newIORef $ FR 0 a 0 0

addTick :: FRBuf -> Double -> IO ()
addTick frRef tick = do
  fr <- readIORef frRef
  let i = tickIndex fr
  oldTick <- readArray (ticks fr) i
  writeArray (ticks fr) i tick
  let tickIndex' = if i == windowSize - 1 then 0 else i + 1
  let tickSum' = tickSum fr - oldTick + tick
  writeIORef frRef $ fr { tickIndex = tickIndex', tickSum = tickSum'
                        , tickSamples = min windowSize (tickSamples fr + 1) }

averageTick :: FRBuf -> IO Double
averageTick frRef = readIORef frRef >>= \fr -> return $ tickSum fr / (fromIntegral $ tickSamples fr)

----------------------------------------------------------------------------------------------------



test :: IO ()
test = do
  frBuf <- initFRBuf
  forM_ (take 20 $ cycle [30,25,20]) $ \tick -> do
    addTick frBuf tick
    rate <- averageTick frBuf
    printf "%.2f\n" rate
