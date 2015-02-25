{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
module Main where

import qualified Graphics.UI.SDL             as S
import qualified Graphics.UI.SDL.Mixer       as M
import qualified Graphics.UI.SDL.Mixer.Types as M
import qualified Graphics.UI.SDL.Keycode     as SK
import           System.Exit
import           Foreign.Marshal.Alloc
import           Control.Monad
import           Foreign.Storable
import           Foreign.ForeignPtr
import           Text.Printf
import           Foreign.Ptr
import           GHC.Word
import           Data.Int
import           Data.IORef
import           Data.Time

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

----------------------------------------------------------------------------------------------------
data State = State { stChunkQueue          :: [SoundChunk]
                   , stCurrentChunk        :: Maybe SoundChunk
                   , stCurrentChunkElapsed :: Double
                   , stLastTime            :: UTCTime
                   , stWindow              :: S.Window -- you need to keep a reference to this
                   }

data SoundChunk = SoundChunk { scChunk :: M.Chunk, scDuration :: Time }
----------------------------------------------------------------------------------------------------
type MusicHook = Ptr Word8 -> Int -> IO ()

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  S.init [S.InitVideo, S.InitAudio]
  window  <- S.createWindow "Procedural Music" (S.Position 0 (-500)) (S.Size w h) [S.WindowShown]
  let sampleRate = 48000
  M.openAudio sampleRate S.AudioS16Sys outputChannels 8192
  M.allocateChannels mixChannels


  --let f :: Sound -> Time -> IO SoundChunk
  --    f snd startTime = createChunkFromSound sampleRate 1 (snd `fromTime` startTime)
  --chunkQueue <- mapM (f sin440) (take 1 [0..])
  t <- getCurrentTime
  sref <- newIORef $ State { stChunkQueue = []
                           , stCurrentChunk = Nothing
                           , stCurrentChunkElapsed = 0
                           , stLastTime = t
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
createChunkFromSound :: Int -> Double -> Sound -> IO SoundChunk
createChunkFromSound sampleRate seconds snd = do
  t <- getCurrentTime
  let numSamples = ceiling (fromIntegral sampleRate * seconds)
      bytes = numSamples * outputChannels * bytesPerSample
      sampleSizeInSeconds = seconds / fromIntegral sampleRate
      scale :: Int
      scale = 2^15 - 1
      -- a block is the number of bytes for one sample per channel.
      blockSize = bytesPerSample*outputChannels
  (abuf :: Ptr Word8) <- mallocBytes bytes
  -- printf "numSamples = %d, bytes = %d\n" numSamples bytes
  forM_ [0..numSamples-1] $ \i -> do
    let n = fromIntegral i * sampleSizeInSeconds
        sample = snd n         -- now sample snd at n
        scaledSample :: Int16
        scaledSample = ceiling (fromIntegral scale *  sample)
    forM_ [0..outputChannels-1] $ \j -> do
      pokeByteOff abuf (i*blockSize+j*bytesPerSample) scaledSample
  let chunkStruct = M.ChunkStruct 1 abuf (fromIntegral bytes) 128
  chunkPtr <- malloc
  poke chunkPtr chunkStruct
  chunk <- M.mkFinalizedChunk chunkPtr
  t' <- getCurrentTime
  printf "Chunk creation: %.2fs\n" $ (realToFrac $ diffUTCTime t' t :: Double)
  return $ SoundChunk { scChunk = chunk, scDuration = seconds }

--------------------------------------------------------------------------------------------------
mainLoop :: IORef State -> IO ()
mainLoop sref = do
  t <- getCurrentTime
  -- S.delay 10 -- So we don't consume 100% CPU
  mbEvent <- S.pollEvent
  case mbEvent of
    Just e | checkForQuit e -> exitWith ExitSuccess
    _ -> return ()

  let playNextChunk = do
        t <- getCurrentTime
        mbNextChunk <- do
          s <- readIORef sref
          case stChunkQueue s of
            []             -> do
              writeIORef sref $ s { stCurrentChunk = Nothing }
              return Nothing
            (chunk:chunks) -> do
              putStrLn $ show t
              writeIORef sref $ s { stChunkQueue = chunks
                                  , stCurrentChunk = Just chunk
                                  , stCurrentChunkElapsed = 0 }
              return $ Just chunk
        maybe (return ()) (\sc -> M.playChannelTimed (-1) (scChunk sc) 0 1000 >> return ())
              mbNextChunk

  -- Pull the next chunk off the queue and play it, but only if one isn't playing.
  s <- readIORef sref
  case stCurrentChunk s of
    Nothing -> playNextChunk
    Just sc -> do
      let elapsed = stCurrentChunkElapsed s
--      printf "Elapsed time: %.2f\n" elapsed
      if elapsed >= scDuration sc
       then writeIORef sref $ s { stCurrentChunk = Nothing }
       else do
         -- update duration
         let d = realToFrac $ diffUTCTime t (stLastTime s)
         modifyIORef sref $ \s -> s { stCurrentChunkElapsed = elapsed + d }


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

