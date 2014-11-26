{-# LANGUAGE LambdaCase #-}
module Audio where

import qualified Sound.OpenAL as AL
import qualified Sound.RubberBand as RB
import Data.Conduit
import qualified Data.Vector.Storable as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndV

-- TODO: implementation that doesn't suck
deinterleave :: (V.Storable a) => Int -> V.Vector a -> [V.Vector a]
deinterleave n v = let
  frames = splitInto n $ V.toList v
  splitInto len xs = case splitAt len xs of
    ([], _) -> []
    (a , b) -> a : splitInto len b
  in do
    i <- [0 .. n - 1]
    return $ V.fromList $ map (!! i) frames

load :: Snd.Handle -> Snd.Count -> Source IO [V.Vector Float]
load h c = let
  chans = Snd.channels $ Snd.hInfo h
  in fix $ \loop -> liftIO (Snd.hGetBuffer h c) >>= \case
    Nothing -> return ()
    Just buf -> do
      yield $ deinterleave chans $ SndV.fromBuffer buf
      loop

stretch :: RB.SampleRate -> RB.NumChannels -> RB.TimeRatio -> RB.PitchScale
  -> Conduit [V.Vector Float] IO [V.Vector Float]
stretch a b c d = do
  let opts = RB.defaultOptions { RB.oProcess = RB.RealTime }
  s <- liftIO $ RB.new a b opts c d
  fix $ \loop -> liftIO (RB.available s) >>= \case
    Nothing -> return () -- shouldn't happen?
    Just 0 -> liftIO (RB.getSamplesRequired s) >>= \case
      0 -> liftIO (threadDelay 100) >> loop -- adjust this delay?
      _ -> await >>= \case
        Nothing    -> return ()
        Just block -> liftIO (RB.process s block False) >> loop -- always False?
    Just n -> liftIO (RB.retrieve s n) >>= yield >> loop
