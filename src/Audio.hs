{-# LANGUAGE LambdaCase #-}
module Audio where

import qualified Sound.OpenAL as AL
import qualified Sound.RubberBand as RB
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Vector.Storable as V
import Foreign.Storable (sizeOf)
import Data.Int (Int16)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV

deinterleave :: (V.Storable a) => Int -> V.Vector a -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  return $ V.generate len $ \j -> v V.! ((n * j) + i)

load :: Snd.Handle -> Snd.Count -> Source IO [V.Vector Float]
load h c = let
  chans = Snd.channels $ Snd.hInfo h
  in fix $ \loop -> liftIO (Snd.hGetBuffer h c) >>= \case
    Nothing -> return ()
    Just buf -> do
      yield $ deinterleave chans $ SndV.fromBuffer buf
      loop

stretch :: RB.SampleRate -> RB.NumChannels -> RB.TimeRatio -> RB.PitchScale
  -> Int -> Conduit [V.Vector Float] IO [V.Vector Float]
stretch _ _ 1 1 _       = C.map id
stretch a b c d maxSize = do
  let opts = RB.defaultOptions { RB.oProcess = RB.RealTime }
  s <- liftIO $ RB.new a b opts c d
  liftIO $ RB.setMaxProcessSize s maxSize
  fix $ \loop -> liftIO (RB.available s) >>= \case
    Nothing -> return () -- shouldn't happen?
    Just 0 -> liftIO (RB.getSamplesRequired s) >>= \case
      0 -> liftIO (threadDelay 100) >> loop -- adjust this delay?
      _ -> await >>= \case
        Nothing    -> return ()
        Just block -> liftIO (RB.process s block False) >> loop -- always False?
    Just n -> liftIO (RB.retrieve s n) >>= yield >> loop

convertAudio :: V.Vector Float -> V.Vector Int16
convertAudio = V.map $ \f -> round $ f * fromIntegral (maxBound :: Int16)

supply :: AL.Source -> Int -> Sink (V.Vector Float) IO ()
supply src n = fix $ \loop -> do
  -- First, check if old buffers need to be removed
  pr <- liftIO $ AL.get $ AL.buffersProcessed src
  when (pr /= 0) $ liftIO $ AL.unqueueBuffers src pr >>= AL.deleteObjectNames
  -- Then, check if we need to add new buffers
  qu <- liftIO $ fmap fromIntegral $ AL.get $ AL.buffersQueued src
  if qu >= n
    then liftIO (threadDelay 100) >> loop
    else await >>= \case
      -- If there are still queued buffers but no more input,
      -- we must loop more to dequeue the remaining buffers.
      Nothing -> when (qu /= 0) $ liftIO (threadDelay 100) >> loop
      Just vf -> do
        let v = convertAudio vf
        liftIO $ do
          buf <- liftIO AL.genObjectName
          V.unsafeWith v $ \p -> do
            let mem = AL.MemoryRegion p $ fromIntegral vsize
                vsize = V.length v * sizeOf (v V.! 0)
            AL.bufferData buf AL.$= AL.BufferData mem AL.Mono16 44100
          AL.queueBuffers src [buf]
        loop
