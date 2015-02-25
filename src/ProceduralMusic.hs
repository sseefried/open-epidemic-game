{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
module Main where

import qualified Graphics.UI.SDL             as S
import qualified Graphics.UI.SDL.Mixer       as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import qualified Graphics.UI.SDL.Keycode     as SK
import           System.Exit
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Control.Monad
import           Foreign.Storable
import           Foreign.ForeignPtr
import           Text.Printf
import           Foreign.Ptr
import           GHC.Word
import           Data.Int
import           Data.IORef
import           Data.Time
import           Control.Concurrent


w, h :: Int
w = 320
h = 200

base = "/Users/sseefried/code/games/epidemic-game/resources" -- I'll change this later
----------------------------------------------------------------------------------------------------
-- Some constants
outputChannels, mixChannels, bytesPerSample :: Int
outputChannels = 2 -- 2 is for stereo, 1 is for mono
mixChannels = 10
bytesPerSample  = 2
sampleRate = 48000
----------------------------------------------------------------------------------------------------
data State = State { stLastTime            :: UTCTime
                   , stWindow              :: S.Window -- you need to keep a reference to this
                   }

----------------------------------------------------------------------------------------------------
hook :: IORef Time -> M.MusicHook
hook ref ptr bytes = do
  t <- readIORef ref
  --
  -- If you were asked for 8192 bytes and the sampleRate was 44100, outPutChannels == 2
  -- and bytesPerSample == 2, then 4 bytes would be used for each 2-channel sample.
  -- 8192 / (44100 * 4) == 0.0464s
  --
  let duration = fromIntegral bytes / fromIntegral (sampleRate*outputChannels*bytesPerSample)
  -- Now get the bytes
  createChunkFromSound ptr sampleRate bytes sin440 t
  writeIORef ref $ t + duration

main :: IO ()
main = do
  S.init [S.InitVideo, S.InitAudio]
  window  <- S.createWindow "Procedural Music" (S.Position 0 (-500)) (S.Size w h) [S.WindowShown]
  M.openAudio sampleRate S.AudioS16Sys outputChannels (2^16)
  M.allocateChannels mixChannels
  soundTimeRef <- newIORef 0
  -- now copy the chunk to ptr
  M.hookMusic (hook soundTimeRef)
  S.unlockAudio
  t <- getCurrentTime
  sref <- newIORef $ State { stLastTime = t
                           , stWindow = window }
  mainLoop sref

----------------------------------------------------------------------------------------------------
type Time = Double -- Continuous time! :-)
type Sound = Time -> Double -- Time to Amplitude (a value between -1 and 1)
----------------------------------------------------------------------------------------------------
sin440 :: Sound
sin440 t = sin (440*2*pi*t)

silence :: Sound
silence _ = 0

-- Creates a Sound that starts at a different time.
fromTime :: Sound -> Time -> Sound
snd `fromTime` startTime = \t -> snd (t + startTime)

----------------------------------------------------------------------------------------------------
--
-- I'm going to change this function so that it doesn't take seconds as an argument but
-- takes a number of bytes to produce instead.
--

{-# INLINE createChunkFromSound #-}
createChunkFromSound :: Ptr Word8 -> Int -> Int -> Sound -> Time -> IO ()
createChunkFromSound buf sampleRate bytes sound startTime = do
  t <- getCurrentTime
  let numSamples = bytes `div` (outputChannels * bytesPerSample)
      sampleSizeInSeconds = 1.0 / (fromIntegral sampleRate)
      scale :: Int
      scale = 2^15 - 1
      -- a block is the number of bytes for one sample per channel.
      blockSize = bytesPerSample*outputChannels
  -- printf "numSamples = %d, bytes = %d\n" numSamples bytes
  let loop i | i < 0 || i >= numSamples = return ()
             | otherwise = do
                let n = fromIntegral i * sampleSizeInSeconds
                    sample = sound (n + startTime)         -- now sample snd at n
                    scaledSample :: Int16
                    scaledSample = floor (fromIntegral scale * sample)
                forM_ [0..outputChannels-1] $ \j -> do
                  pokeByteOff buf (i*blockSize+j*bytesPerSample) scaledSample
                loop (i+1)
  loop 0
  t' <- getCurrentTime
  printf "Chunk creation: %.4fs, bytes: %d\n" (realToFrac $ diffUTCTime t' t :: Double) bytes
  return ()

--------------------------------------------------------------------------------------------------
mainLoop :: IORef State -> IO ()
mainLoop sref = do
  t <- getCurrentTime
  S.delay 1000 -- So we don't consume 100% CPU
  mbEvent <- S.pollEvent
  case mbEvent of
    Just e | checkForQuit e -> exitWith ExitSuccess
    _ -> return ()
  -- Update the last time each time through the loop
  modifyIORef sref $ \s -> s { stLastTime = t }
  mainLoop sref

----------------------------------------------------------------------------------------------------
checkForQuit e = case S.eventData e of
      S.Quit                    -> True
      _ | b <- isKeyDown e SK.Q -> b

----------------------------------------------------------------------------------------------------
isKeyDown :: S.Event -> SK.Keycode -> Bool
isKeyDown e code = case S.eventData e of
  S.Keyboard {  S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ code' _ } -> code == code'
  _ -> False

