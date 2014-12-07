{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Audio (load, stretch, convert, supply) where

import           Control.Concurrent               (threadDelay)
import           Control.Monad                    (forM, forM_, when)
import           Control.Monad.Fix                (fix)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Data.Conduit                     as C
import qualified Data.Conduit.List                as CL
import           Data.Functor                     (void)
import           Data.Int                         (Int16)
import qualified Data.Vector.Storable             as V
import           Foreign.Storable                 (sizeOf)
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV
import           Sound.OpenAL                     (($=))
import qualified Sound.OpenAL                     as AL
import qualified Sound.RubberBand                 as RB

import Control.Monad.Trans.Resource

import qualified Data.Set as Set
import Data.IORef

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

(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —

load :: (Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e) =>
  Int -> [FilePath] -> C.Source (ResourceT IO) [V.Vector e]
load pos = C.mapOutput concat . C.sequenceSources . map (loadOne' pos)

loadOne' :: (Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e) =>
  Int -> FilePath -> C.Source (ResourceT IO) [V.Vector e]
loadOne' pos f = C.bracketP
  — Snd.openFile f Snd.ReadMode Snd.defaultInfo
  — Snd.hClose
  — \h -> do
    _ <- liftIO $ Snd.hSeek h Snd.AbsoluteSeek pos
    loadOne h

loadOne :: (MonadIO m, Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e) =>
  Snd.Handle -> C.Source m [V.Vector e]
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

stretch :: (MonadIO m)
  => RB.SampleRate -> RB.NumChannels -> RB.TimeRatio -> RB.PitchScale
  -> C.Conduit [V.Vector Float] m [V.Vector Float]
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

supply
  :: [AL.Vertex3 AL.ALfloat]
  -> [AL.Gain]
  -> C.Sink [V.Vector Int16] (ResourceT IO) ()
supply ps gs = void $ C.sequenceSinks $ do
  (p, g, i) <- zip3 ps gs [0..]
  return $ C.mapInput (!! i) (const Nothing) $ supplyOne p g

supplyOne
  :: AL.Vertex3 AL.ALfloat
  -> AL.Gain
  -> C.Sink (V.Vector Int16) (ResourceT IO) ()
supplyOne pos g = C.bracketP
  — do
    src <- AL.genObjectName
    ref <- newIORef Set.empty
    return (src, ref)
  — do
    \(src, ref) -> do
      AL.stop [src]
      AL.buffer src $= Nothing
      bufs <- Set.toList `fmap` readIORef ref
      let loop = do
            _ <- AL.get AL.alErrors
            AL.deleteObjectNames bufs
            errs <- AL.get AL.alErrors
            case errs of
              [] -> return ()
              _  -> threadDelay 10000 >> loop
      loop
  — \(src, ref) -> do
    liftIO $ AL.sourcePosition src $= pos
    liftIO $ AL.sourceGain src $= g
    let loop q = do
          p <- liftIO $ do
            bufs <- AL.get (AL.buffersProcessed src) >>= AL.unqueueBuffers src
            modifyIORef ref $ \s -> Set.difference s $ Set.fromList bufs
            len <- fmap (fromIntegral . sum) $ forM bufs $ \buf -> do
              AL.BufferData (AL.MemoryRegion _ size) _ _
                <- AL.get $ AL.bufferData buf
              return $ size `quot` 2
            AL.deleteObjectNames bufs
            return len
          let q' = q - p
          if q' >= 2 * 44100
            then shortWait >> loop q'
            else C.await >>= \case
              Nothing -> return ()
              Just v -> do
                buf <- liftIO $ makeBuffer v
                liftIO $ modifyIORef ref $ Set.insert buf
                let q'' = q' + V.length v
                liftIO $ AL.queueBuffers src [buf]
                when (q'' >= 44100) $ do
                  liftIO $ AL.get (AL.sourceState src) >>= \case
                    AL.Playing -> return ()
                    _          -> AL.play [src]
                loop q''
    loop 0
