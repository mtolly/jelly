{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Audio (load, stretch, convert, supply) where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.MVar          (MVar, tryTakeMVar)
import           Control.Monad                    (forM_, when)
import           Control.Monad.Fix                (fix)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Data.Conduit                     as C
import qualified Data.Conduit.List                as CL
import           Data.Int                         (Int16)
import qualified Data.Vector.Storable             as V
import           Foreign.Storable                 (sizeOf)
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV
import           Sound.OpenAL                     (($=))
import qualified Sound.OpenAL                     as AL
import qualified Sound.RubberBand                 as RB

-- | Given a vector with interleaved samples, like @[L0, R0, L1, R1, ...]@,
-- converts it into @[[L0, L1, ...], [R0, R1, ...]]@.
deinterleave :: (V.Storable a)
  => Int -- ^ The number of channels to split into.
  -> V.Vector a
  -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  return $ V.generate len $ \j -> v V.! (n * j + i)

-- | Used as an arbitrary maximum size for audio input/output blocks.
maxBlockSize :: Int
maxBlockSize = 5000

-- | Loads sample blocks from the given audio files.
-- Seek the handles beforehand to load from a certain point in the file.
load :: (Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e) =>
  [Snd.Handle] -> C.Source IO [V.Vector e]
load = C.mapOutput concat . C.sequenceSources . map loadOne

loadOne :: (Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e) =>
  Snd.Handle -> C.Source IO [V.Vector e]
loadOne h = let
  chans = Snd.channels $ Snd.hInfo h
  in fix $ \loop -> liftIO (Snd.hGetBuffer h maxBlockSize) >>= \case
    Nothing -> return ()
    Just buf -> do
      C.yield $ deinterleave chans $ SndV.fromBuffer buf
      loop

breakBlock
  :: Int -- ^ Maximum size of the output blocks
  -> [V.Vector Float] -- ^ One vector per channel
  -> [[V.Vector Float]]
breakBlock maxSize chans = if V.length (head chans) <= maxSize
  then [chans]
  else let
    pairs = map (V.splitAt maxSize) chans
    in map fst pairs : breakBlock maxSize (map snd pairs)

-- | An arbitrary \"this thread has nothing to do immediately\" wait.
shortWait :: (MonadIO m) => m ()
shortWait = liftIO $ threadDelay 1000

stretch :: RB.SampleRate -> RB.NumChannels -> RB.TimeRatio -> RB.PitchScale
  -> C.Conduit [V.Vector Float] IO [V.Vector Float]
stretch a b c d = do
  let opts = RB.defaultOptions { RB.oProcess = RB.RealTime }
  s <- liftIO $ RB.new a b opts c d
  liftIO $ RB.setMaxProcessSize s maxBlockSize
  fix $ \loop -> liftIO (RB.available s) >>= \case
    Nothing -> return () -- shouldn't happen?
    Just 0 -> liftIO (RB.getSamplesRequired s) >>= \case
      0 -> shortWait >> loop
      _ -> C.await >>= \case
        Nothing    -> return ()
        Just block -> do
          forM_ (breakBlock maxBlockSize block) $ \blk ->
            liftIO $ RB.process s blk False
          loop
    Just n -> liftIO (RB.retrieve s $ min n maxBlockSize) >>= C.yield >> loop

-- | Translates samples from floats (Rubber Band) to 16-bit ints (OpenAL).
convert :: (Monad m) => C.Conduit [V.Vector Float] m [V.Vector Int16]
convert =
  CL.map $ map $ V.map $ \f -> round $ f * fromIntegral (maxBound :: Int16)

-- | Converts a vector of samples to an OpenAL buffer.
makeBuffer :: V.Vector Int16 -> IO AL.Buffer
makeBuffer v = do
  buf <- liftIO AL.genObjectName
  V.unsafeWith v $ \p -> do
    let mem = AL.MemoryRegion p $ fromIntegral vsize
        vsize = V.length v * sizeOf (v V.! 0)
    AL.bufferData buf $= AL.BufferData mem AL.Mono16 44100
  return buf

-- | Continually feeds the OpenAL sources with sample blocks from upstream.
supply
  :: [AL.Source]
  -> Int -- ^ Sources will be supplied if they have less than this many blocks.
  -> MVar () -- ^ To stop the pipeline, place () in here, and wait until empty.
  -> C.Sink [V.Vector Int16] IO ()
supply srcs n mvar = fix $ \loop -> do
  -- Check if pipeline has been requested to stop
  mval <- liftIO $ tryTakeMVar mvar
  case mval of
    Just () -> return () -- done!
    Nothing -> do
      -- First, check if old buffers need to be removed
      pr <- liftIO $ fmap minimum $ mapM (AL.get . AL.buffersProcessed) srcs
      when (pr /= 0) $ liftIO $ forM_ srcs $ \src ->
        AL.unqueueBuffers src pr >>= AL.deleteObjectNames
      -- Then, check if we need to add new buffers
      qu <- liftIO $ fmap (fromIntegral . minimum) $ mapM (AL.get . AL.buffersQueued) srcs
      if qu >= n
        then shortWait >> loop
        else C.await >>= \case
          -- If there are still queued buffers but no more input,
          -- we must loop more to dequeue the remaining buffers.
          Nothing -> when (qu /= 0) $ shortWait >> loop
          Just vs -> do
            forM_ (zip srcs vs) $ \(src, v) -> do
              buf <- liftIO $ makeBuffer v
              liftIO $ AL.queueBuffers src [buf]
            loop
