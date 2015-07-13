{- |

A self-contained audio playback machine.
* Load from any number of audio files
* Combine them in an arbitrary mix configuration
* Play/pause audio at any time, with precise knowledge of audio timing
Currently assumes all audio files are stereo, 44.1 kHz.

-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
module Jelly.AudioPipe
( AudioPipe, withPipe, seekTo, playPause, setVolumes
) where

import           Jelly.Prelude

import           Control.Arrow                    ((***))
import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Concurrent.MVar          (MVar, isEmptyMVar,
                                                   newEmptyMVar, newMVar,
                                                   readMVar, tryPutMVar)
import           Control.Exception                (bracket, bracket_)
import           Control.Monad.Fix                (fix)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Conduit                     (($$), (=$=))
import qualified Data.Conduit                     as C
import qualified Data.Conduit.List                as CL
import           Data.Int                         (Int16)
import           Data.IORef                       (IORef, newIORef, readIORef,
                                                   writeIORef)
import           Data.List                        (elemIndex, foldl1', nub)
import qualified Data.Set                         as Set
import qualified Data.Vector.Storable             as V
import           Foreign
import           Foreign.C
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV
import           Sound.AL
import           Sound.ALC
import qualified Sound.RubberBand                 as RB

type Stoppable = (MVar (), MVar ())

newStoppable :: IO Stoppable
newStoppable = liftA2 (,) newEmptyMVar newEmptyMVar

newStopped :: IO Stoppable
newStopped = liftA2 (,) (newMVar ()) (newMVar ())

stop :: Stoppable -> IO ()
stop s = do
  requestStop s
  waitTillStopped s

requestStop :: Stoppable -> IO ()
requestStop (v1, _) = do
  _ <- tryPutMVar v1 ()
  pure ()

waitTillStopped :: Stoppable -> IO ()
waitTillStopped (_, v2) = readMVar v2

stopRequested :: Stoppable -> IO Bool
stopRequested (v1, _ ) = fmap not $ isEmptyMVar v1

markStopped :: Stoppable -> IO ()
markStopped (_, v2) = do
  _ <- tryPutMVar v2 ()
  pure ()

type ALsource = ALuint
type ALbuffer = ALuint

data AudioPipe = AudioPipe
  { handles_ :: [Snd.Handle]
  , sources_ :: [(ALsource, ALsource)]
  , filler_  :: IORef Stoppable
  , playing_ :: IORef Bool
  , rewire_  :: forall a. (Num a, V.Storable a) =>
      [(V.Vector a, V.Vector a)] -> [(V.Vector a, V.Vector a)]
  , lenmax_  :: Double
  , lenmin_  :: Double
  , full_    :: IORef Bool
  }

type Input = ([FilePath], [FilePath])

genSources :: Int -> IO [ALsource]
genSources n = allocaArray n $ \p -> do
  alGenSources (fromIntegral n) p
  peekArray n p

-- | Makes a new pipe, and starts loading data from position zero at 100% speed.
makePipe
  :: Double -- ^ In seconds, max length of audio data loaded into the queues
  -> Double -- ^ In seconds, min length of audio data before playing sources
  -> [Input]
  -> IO AudioPipe
makePipe lenmax lenmin ins = do
  srcsL <- genSources $ length ins
  srcsR <- genSources $ length ins
  forM_ srcsL $ \src -> alSource3f src al_POSITION (-1) 0 0
  forM_ srcsR $ \src -> alSource3f src al_POSITION 1    0 0
  let allFiles = nub $ ins >>= uncurry (++)
  hnds <- forM allFiles $ \f -> Snd.openFile f Snd.ReadMode Snd.defaultInfo
  let fileIndex :: FilePath -> Int
      fileIndex f = case elemIndex f allFiles of
        Nothing -> error "makePipe: impossible! file not found in list"
        Just i  -> i
  let insNumbered :: [([Int], [Int])]
      insNumbered = map (map fileIndex *** map fileIndex) ins
  let rewire :: (Num a, V.Storable a) =>
        [(V.Vector a, V.Vector a)] -> [(V.Vector a, V.Vector a)]
      rewire loaded = do
        (pos, neg) <- insNumbered
        let pos' = map (loaded !!) pos
            neg' = map (V.map negate *** V.map negate) $ map (loaded !!) neg
        pure $ foldl1'
          (\(l1, r1) (l2, r2) -> (V.zipWith (+) l1 l2, V.zipWith (+) r1 r2))
          $ pos' ++ neg'
  filler <- newStopped >>= newIORef
  playing <- newIORef False
  full <- newIORef False
  let pipe = AudioPipe
        { handles_ = hnds
        , sources_ = zip srcsL srcsR
        , filler_ = filler
        , playing_ = playing
        , rewire_ = rewire
        , lenmax_ = lenmax
        , lenmin_ = lenmin
        , full_   = full
        }
  seekTo 0 1 pipe
  pure pipe

closePipe :: AudioPipe -> IO ()
closePipe pipe = do
  readIORef (filler_ pipe) >>= stop
  writeIORef (playing_ pipe) False
  forM_ (handles_ pipe) Snd.hClose
  forM_ (sources_ pipe) $ \(srcL, srcR) -> withArrayLen' [srcL, srcR] alDeleteSources

withPipe :: Double -> Double -> [Input] -> (AudioPipe -> IO a) -> IO a
withPipe lenmax lenmin ins = withALContext
  . bracket (makePipe lenmax lenmin ins) closePipe

withALContext :: IO a -> IO a
withALContext act = bracket
  — do throwIfNull "couldn't open audio device" $ alcOpenDevice nullPtr
  — alcCloseDevice
  — \dev -> bracket
    — do throwIfNull "couldn't create audio context" $ alcCreateContext dev nullPtr
    — alcDestroyContext
    — \ctxt -> bracket_
      — alcMakeContextCurrent ctxt
      — alcMakeContextCurrent nullPtr
      — act

withArrayLen' :: (Storable a, Num n, MonadIO m) => [a] -> (n -> Ptr a -> IO b) -> m b
withArrayLen' xs f = liftIO $ withArrayLen xs $ \n p -> f (fromIntegral n) p

-- | If paused, seeks to the new position and begins queueing data.
-- If playing, pauses, seeks to the new position, and plays once data is queued.
seekTo
  :: Double -- ^ Position in seconds
  -> Double -- ^ Playing speed: ratio of heard seconds to file seconds
  -> AudioPipe
  -> IO ()
seekTo pos speed pipe = readIORef (playing_ pipe) >>= \case
  True -> playPause pipe >> seekTo pos speed pipe >> playPause pipe
  False -> do
    readIORef (filler_ pipe) >>= stop
    forM_ (handles_ pipe) $ \h -> let
      len = Snd.frames $ Snd.hInfo h
      newPos = min len $ floor $ pos * 44100
      in Snd.hSeek h Snd.AbsoluteSeek newPos
    s <- newStoppable
    let source :: C.Source IO [V.Vector Int16]
        source = case speed of
          1 -> load (handles_ pipe) =$= rewire
          _ -> load (handles_ pipe) =$= rewire =$= stretch 44100 numOut speed 1 =$= convert
        rewire :: (V.Storable e, Num e) => C.Conduit [V.Vector e] IO [V.Vector e]
        rewire = CL.map $ breakPairs . rewire_ pipe . joinPairs
        numOut = length sourcesFlat
        sourcesFlat = breakPairs $ sources_ pipe
    _ <- forkIO $ source $$ do
      let loop :: Int -> Set.Set ALbuffer -> C.Sink [V.Vector Int16] IO ()
          loop q bufs = liftIO (stopRequested s) >>= \case
            True -> liftIO $ do
              withArrayLen' sourcesFlat alSourceStopv
              forM_ sourcesFlat $ \src ->
                alSourcei src al_BUFFER al_NONE
              withArrayLen' sourcesFlat alSourceRewindv
              writeIORef (full_ pipe) True
              fix $ \loopCleanup -> do
                _ <- alGetError
                withArrayLen' (Set.toList bufs) alDeleteBuffers
                alGetError >>= \err -> if err == al_NO_ERROR
                  then markStopped s
                  else shortWait >> loopCleanup
            False -> do
              canRemove <- liftIO $ fmap minimum $ forM sourcesFlat $ \src -> do
                alloca $ \p -> do
                  alGetSourcei src al_BUFFERS_PROCESSED p
                  peek p
              bufsRemoved <- liftIO $ forM sourcesFlat $ \src -> do
                allocaArray (fromIntegral canRemove) $ \p -> do
                  alSourceUnqueueBuffers src canRemove p
                  peekArray (fromIntegral canRemove) p
              p <- liftIO $ fmap sum $ forM (head bufsRemoved) $ \buf -> do
                size <- alloca $ \p -> do
                  alGetBufferi buf al_SIZE p
                  peek p
                pure $ fromIntegral size `quot` 2
              let q' = q - p
                  bufs' = Set.difference bufs $ Set.fromList $ concat bufsRemoved
              withArrayLen' (concat bufsRemoved) alDeleteBuffers
              if q' >= floor (44100 * lenmax_ pipe)
                then shortWait >> loop q' bufs'
                else C.await >>= \case
                  Nothing -> do -- shouldn't happen
                    liftIO $ requestStop s
                    loop q' bufs'
                  Just vs -> do
                    bufsNew <- liftIO $ mapM makeBuffer vs
                    let q'' = q' + V.length (head vs)
                        bufs'' = Set.union bufs' $ Set.fromList bufsNew
                    liftIO $ forM_ (zip sourcesFlat bufsNew) $ \(src, buf) ->
                      withArrayLen' [buf] $ alSourceQueueBuffers src
                    liftIO $ when (q'' >= floor (44100 * lenmin_ pipe))
                      $ writeIORef (full_ pipe) True
                    loop q'' bufs''
      loop 0 Set.empty
    writeIORef (filler_ pipe) s

-- | Converts a vector of samples to an OpenAL buffer.
makeBuffer :: V.Vector Int16 -> IO ALbuffer
makeBuffer v = do
  buf <- alloca $ \p -> alGenBuffers 1 p >> peek p
  V.unsafeWith v $ \p -> do
    let vsize = fromIntegral $ V.length v * sizeOf (v V.! 0)
    alBufferData buf al_FORMAT_MONO16 p vsize 44100
  pure buf

-- | Translates samples from floats (Rubber Band) to 16-bit ints (OpenAL).
convert :: (Monad m) => C.Conduit [V.Vector Float] m [V.Vector Int16]
convert =
  CL.map $ map $ V.map $ \f -> round $ f * fromIntegral (maxBound :: Int16)

loadBlockSize :: Int
loadBlockSize = 5000

load
  :: (MonadIO m, Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e, Num e)
  => [Snd.Handle] -> C.Source m [V.Vector e]
load = C.mapOutput concat . C.sequenceSources . map loadOne

loadOne
  :: (MonadIO m, Snd.Sample e, V.Storable e, Snd.Buffer SndV.Buffer e, Num e)
  => Snd.Handle -> C.Source m [V.Vector e]
loadOne h = let
  chans = Snd.channels $ Snd.hInfo h
  fullBlock b = if V.length b == loadBlockSize
    then b
    else V.take loadBlockSize $ b V.++ V.replicate loadBlockSize 0
  in fix $ \loop -> liftIO (Snd.hGetBuffer h loadBlockSize) >>= \case
    Nothing -> do
      C.yield $ replicate chans $ V.replicate loadBlockSize 0
      loop
    Just buf -> do
      C.yield $ map fullBlock $ deinterleave chans $ SndV.fromBuffer buf
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
  liftIO $ RB.setMaxProcessSize s loadBlockSize
  fix $ \loop -> liftIO (RB.available s) >>= \case
    Nothing -> pure () -- shouldn't happen?
    Just 0 -> liftIO (RB.getSamplesRequired s) >>= \case
      0 -> shortWait >> loop
      _ -> C.await >>= \case
        Nothing    -> pure ()
        Just block -> do
          forM_ (breakBlock loadBlockSize block) $ \blk ->
            liftIO $ RB.process s blk False
          loop
    Just n -> liftIO (RB.retrieve s $ min n loadBlockSize) >>= C.yield >> loop

-- | If paused, waits till data is queued, then starts playing.
-- If playing, pauses. Data will already be queued.
playPause :: AudioPipe -> IO ()
playPause pipe = readIORef (playing_ pipe) >>= \case
  True -> do
    writeIORef (playing_ pipe) False
    withArrayLen' sourcesFlat alSourcePausev
  False -> fix $ \loop -> readIORef (full_ pipe) >>= \case
    False -> shortWait >> loop
    True -> do
      writeIORef (playing_ pipe) True
      withArrayLen' sourcesFlat alSourcePlayv
  where sourcesFlat = breakPairs $ sources_ pipe

breakPairs :: [(a, a)] -> [a]
breakPairs xs = xs >>= \(x, y) -> [x, y]

joinPairs :: [a] -> [(a, a)]
joinPairs (x : y : xs) = (x, y) : joinPairs xs
joinPairs [] = []
joinPairs [_] = error "joinPairs: odd number of elements"

-- | Sets volumes of the audio sources. Doesn't care whether playing/paused.
setVolumes :: [Double] -> AudioPipe -> IO ()
setVolumes vols pipe = forM_ (zip vols $ sources_ pipe) $ \(vol, (srcL, srcR)) -> do
  alSourcef srcL al_GAIN $ realToFrac vol
  alSourcef srcR al_GAIN $ realToFrac vol

-- | Given a vector with interleaved samples, like @[L0, R0, L1, R1, ...]@,
-- converts it into @[[L0, L1, ...], [R0, R1, ...]]@.
deinterleave :: (V.Storable a)
  => Int -- ^ The number of channels to split into.
  -> V.Vector a
  -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  pure $ V.generate len $ \j -> v V.! (n * j + i)
