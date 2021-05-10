{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Euterpea.IO.Audio.IO (
    outFile,  outFileNorm,
--    outFileA, outFileNormA, RecordStatus,
    maxSample, rtRender) where

import           Codec.Wav
import           Control.Arrow.ArrowP
import           Control.Concurrent
import           Control.Monad
import           Control.SF.SF
import           Data.Array.Unboxed
import           Data.Audio
import           Data.Int
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import Euterpea.IO.Audio.Types
    ( AudioSample(collapse, numChans), Clock(..) )
import           GHC.Float
import           Linear                       (V2 (..))
import Options.Applicative
    ( auto, help, info, long, option, value, execParser, Parser )
import           System.PortAudio             (Device (deviceName), Status,
                                               StreamCallbackResult (Continue),
                                               getDevices, noConnection,
                                               setStreamFinishedCallback,
                                               streamParameters, withPortAudio,
                                               withStartStream, withStream)

type Signal clk a b = ArrowP SF clk a b

-- | Writes sound to a wave file (.wav)
outFile :: forall a p. (AudioSample a, Clock p) =>
           String              -- ^ Filename to write to.
        -> Double              -- ^ Duration of the wav in seconds.
        -> Signal p () a       -- ^ Signal representing the sound.
        -> IO ()
outFile = outFileHelp' id


normList :: [Double] -> [Double]
normList xs = map (/ mx) xs
    where mx = max 1.0 (maximum (map abs xs))

-- | Like outFile, but normalizes the output if the amplitude of
-- the signal goes above 1.  If the maximum sample is less than
-- or equal to 1, the output is not normalized.
-- Currently this requires storing the entire output stream in memory
-- before writing to the file.
outFileNorm :: forall a p. (AudioSample a, Clock p) =>
            String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
         -> IO ()
outFileNorm = outFileHelp' normList

outFileHelp :: forall a p. (AudioSample a, Clock p) =>
            ([Double] -> [Double]) -- ^ Post-processing function.
         -> String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
         -> IO ()
outFileHelp f filepath dur sf =
  let sr          = rate (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
      dat         = map (fromSample . (*0.999))
                        (f (toSamples dur sf)) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in exportFile filepath aud

{-
Alternative definition of the above that enforces a clipping behavior when
the value exceeds the [-1.0, 1.0] range. The overflow behavior makes it
very hard to debug sound modeling problems that involve certain waveforms,
such as saw waves. Clipping is also a more common behavior in other audio
software rather than overflowing or wrap-around.
-}

outFileHelp' :: forall a p. (AudioSample a, Clock p) =>
            ([Double] -> [Double]) -- ^ Post-processing function.
         -> String              -- ^ Filename to write to.
         -> Double              -- ^ Duration of the wav in seconds.
         -> Signal p () a       -- ^ Signal representing the sound.
         -> IO ()
outFileHelp' f filepath dur sf =
  let sr          = rate (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
      dat         = map (fromSample . (*0.999) . clipFix)
                        (f (toSamples dur sf)) :: [Int32]
      array       = listArray (0, numSamples-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in exportFile filepath aud



toSamples :: forall a p. (AudioSample a, Clock p) => Double -> Signal p () a -> [Double]
toSamples dur sf =
  let sr          = rate     (undefined :: p)
      numChannels = numChans (undefined :: a)
      numSamples  = truncate (dur * sr) * numChannels
  in take numSamples $ concatMap collapse $ unfold $ strip sf

-- | Compute the maximum sample of an SF in the first 'dur' seconds.
maxSample :: forall a p. (AudioSample a, Clock p) =>
             Double -> Signal p () a -> Double
maxSample dur sf = maximum (map abs (toSamples dur sf))

-- Helpers

clipFix :: (Ord p, Fractional p) => p -> p
clipFix x
  | x > 1.0 = 1.0
  | x < -1.0 = -1.0
  | otherwise = x

-- Portaudio render


-- TODO:
-- Create an interpolation algorithm to map one table onto another (up/downsample)

bufferSize :: Int
bufferSize = 64

writeSamples :: Int -> SF () Double -> Int -> Int -> Int -> MV.IOVector (V2 Float) -> IO ()
writeSamples dur (SF f) buffCount tick buffLength outBuff
  | tick == buffLength = return ()
  | otherwise = do
    let (y, sf) = f ()
        v = clipFix $ double2Float y * 0.999
    MV.write outBuff tick (V2 v v)
    y `seq` sf `seq` writeSamples dur sf buffCount (tick + 1) buffLength outBuff



-- runRt :: SF a b -> a -> b
-- runRt (SF f) inp =
--   let (y, f') = f inp
--   in y `seq` f' `seq` (y, run f' xs)

-- The main audio callback
-- This gets called each buffer cycle and contains a function that fills the output buffer each cycle

callback :: forall a p input. (AudioSample Double, Clock p) =>
           MVar Int
        -> Int
        -> Signal p () Double
        -> Status
        -> input
        -> MV.IOVector (V2 Float)
        -> IO StreamCallbackResult
callback phase dur sf _ input output = do
  -- Get the size of the output buffer frame
  let n = MV.length output
      sf' = strip sf
  -- Get the phase
  i0 <- takeMVar phase
  -- Write values into the output buffer using a signal function starting at 0
  writeSamples dur sf' i0 0 n output
  -- print i0
  -- incriment the phase by the buffer size
  putMVar phase $ i0 + n
  -- Stream callback result
  return Continue


-- IO stuff

app :: forall a p. (AudioSample Double, Clock p) =>
           Int              -- ^ Duration of the wav in seconds.
        -> Signal p () Double      -- ^ Signal representing the sound.
        -> Parser (IO ())
app dur sf = do
  let sr = rate (undefined :: p)
  rate <- option auto $ long "rate" <> help "sampling rate" <> value sr
  buf <- option auto $ long "buffer" <> help "number of samples for the buffer" <> value bufferSize
  device <- option auto $ long "device" <> help "device index" <> value (-1)
  pure $ withPortAudio $ do
    phase <- newMVar 0
    (inDevs, outDevs) <- getDevices
    do
      putStrLn "–––––––––––"
      putStrLn $ "Sample rate: " ++ show rate ++ "hz"
      putStrLn $ "buffer size: " ++ show buf ++ " samples"
      putStrLn "–––––––––––"

      -- Retrive all input devices and manually select one

      -- putStrLn "Please choose an input decvice:"
      -- forM_ (zip [0 :: Int ..] inDevs) $ \(i, dev) ->
      --   putStrLn $ show i ++ ": " ++ deviceName dev
      -- devI <- getLine
      -- let inDev = inDevs !! read devI
      -- let input = streamParameters inDev 0

      -- Retrive all output devices and manually select one

      putStrLn "Please choose an output decvice:"
      forM_ (zip [0 :: Int ..] outDevs) $ \(i, dev) ->
        putStrLn $ show i ++ ": " ++ deviceName dev
      devO <- getLine
      let outDev = outDevs !! read devO
      let output = streamParameters outDev 0
      withStream rate buf noConnection  output mempty (callback phase dur sf)
        $ \s -> do
          setStreamFinishedCallback s $ putStrLn "Done"
          withStartStream s $ threadDelay $ 1000 * dur

rtRender :: forall a p. (AudioSample Double, Clock p) =>
           Int              -- ^ Duration of the wav in seconds.
        -> Signal p () Double       -- ^ Signal representing the sound.
        -> IO ()
rtRender dur sf = join $ execParser (info (app dur sf) mempty)



