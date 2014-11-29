{-# LANGUAGE LambdaCase #-}
module Audio (load, stretch, supply) where

import qualified Sound.OpenAL as AL
import qualified Sound.RubberBand as RB
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Vector.Storable as V
import Foreign.Storable (sizeOf)
import Data.Int (Int16)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, forM_)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV

deinterleave :: (V.Storable a) => Int -> V.Vector a -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  return $ V.generate len $ \j -> v V.! ((n * j) + i)

maxLoad :: Int
maxLoad = 5000

load :: Snd.Handle -> Source IO [V.Vector Float]
load h = let
  chans = Snd.channels $ Snd.hInfo h
  in fix $ \loop -> liftIO (Snd.hGetBuffer h maxLoad) >>= \case
    Nothing -> return ()
    Just buf -> do
      yield $ deinterleave chans $ SndV.fromBuffer buf
      loop

breakBlock :: Int -> [V.Vector Float] -> [[V.Vector Float]]
breakBlock maxSize chans = if V.length (head chans) <= maxSize
  then [chans]
  else let
    pairs = map (V.splitAt maxSize) chans
    in map fst pairs : breakBlock maxSize (map snd pairs)

maxStretchInput :: Int
maxStretchInput = 5000

maxStretchOutput :: Int
maxStretchOutput = 5000

shortWait :: (MonadIO m) => m ()
shortWait = liftIO $ threadDelay 1000

stretch :: RB.SampleRate -> RB.NumChannels -> RB.TimeRatio -> RB.PitchScale
  -> Conduit [V.Vector Float] IO [V.Vector Float]
stretch _ _ 1 1 = C.map id
stretch a b c d = do
  let opts = RB.defaultOptions { RB.oProcess = RB.RealTime }
  s <- liftIO $ RB.new a b opts c d
  liftIO $ RB.setMaxProcessSize s maxStretchInput
  fix $ \loop -> liftIO (RB.available s) >>= \case
    Nothing -> return () -- shouldn't happen?
    Just 0 -> liftIO (RB.getSamplesRequired s) >>= \case
      0 -> shortWait >> loop
      _ -> await >>= \case
        Nothing    -> return ()
        Just block -> do
          forM_ (breakBlock maxStretchInput block) $ \blk ->
            liftIO $ RB.process s blk False
          loop
    Just n -> liftIO (RB.retrieve s $ min n maxStretchOutput) >>= yield >> loop

convertAudio :: V.Vector Float -> V.Vector Int16
convertAudio = V.map $ \f -> round $ f * fromIntegral (maxBound :: Int16)

makeBuffer :: V.Vector Int16 -> IO AL.Buffer
makeBuffer v = do
  buf <- liftIO AL.genObjectName
  V.unsafeWith v $ \p -> do
    let mem = AL.MemoryRegion p $ fromIntegral vsize
        vsize = V.length v * sizeOf (v V.! 0)
    AL.bufferData buf AL.$= AL.BufferData mem AL.Mono16 44100
  return buf

{-
bufferSize :: AL.Buffer -> IO Int
bufferSize buf = do
  AL.BufferData (AL.MemoryRegion _ size) _ _ <- AL.get $ AL.bufferData buf
  return $ fromIntegral size
-}

supply :: [AL.Source] -> Int -> Sink [V.Vector Float] IO ()
supply srcs n = fix $ \loop -> do
  -- First, check if old buffers need to be removed
  pr <- liftIO $ fmap minimum $ mapM (AL.get . AL.buffersProcessed) srcs
  when (pr /= 0) $ liftIO $ forM_ srcs $ \src ->
    AL.unqueueBuffers src pr >>= AL.deleteObjectNames
  -- Then, check if we need to add new buffers
  qu <- liftIO $ fmap (fromIntegral . minimum) $ mapM (AL.get . AL.buffersQueued) srcs
  if qu >= n
    then shortWait >> loop
    else await >>= \case
      -- If there are still queued buffers but no more input,
      -- we must loop more to dequeue the remaining buffers.
      Nothing -> when (qu /= 0) $ shortWait >> loop
      Just vs -> do
        forM_ (zip srcs vs) $ \(src, v) -> do
          buf <- liftIO $ makeBuffer $ convertAudio v
          liftIO $ AL.queueBuffers src [buf]
        loop
