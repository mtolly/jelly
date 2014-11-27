{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign hiding (void)
import Foreign.C
import Control.Monad (unless, forM, forM_)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay, forkIO)
import System.FilePath ((</>))
import qualified Sound.OpenAL as AL
import qualified Sound.File.Sndfile as Snd
import Data.Conduit
import Data.Functor (void)
import System.Environment (getArgs, getProgName)

import Jammit
import Audio

main :: IO ()
main = do
  song <- getArgs >>= \case
    [song] -> return song
    _      -> getProgName >>= \pn -> error $ "Usage: "++pn++" song-folder"

  zero $ SDL.init $ SDL.SDL_INIT_TIMER .|. SDL.SDL_INIT_VIDEO
  Image.imgInit [Image.InitPNG]

  window <- withCString "Jelly" $ \str ->
    notNull $ SDL.createWindow
      str -- title
      SDL.SDL_WINDOWPOS_UNDEFINED -- x
      SDL.SDL_WINDOWPOS_UNDEFINED -- y
      724 -- width
      480 -- height
      SDL.SDL_WINDOW_RESIZABLE -- flags
  rend <- notNull $ SDL.createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED

  Just info <- loadInfo song
  Just trks <- loadTracks song
  putStrLn $ "Title: " ++ title info
  let fileTrks = filter (\t -> trackClass t == "JMFileTrack") trks
      img      = song </> identifier (head fileTrks) ++ "_jcfn_00"
      audio    = map (\t -> song </> identifier t ++ "_jcfx") fileTrks
  putStrLn $ "Tracks: " ++ show (map trackTitle fileTrks)
  Right tex <- Image.imgLoadTexture rend img

  Just dev <- AL.openDevice Nothing
  Just ctxt <- AL.createContext dev []
  AL.currentContext AL.$= Just ctxt
  hnds <- forM audio $ \a ->
    Snd.openFile a Snd.ReadMode Snd.defaultInfo
  srcs <- AL.genObjectNames $ length hnds * 2
  forM_ (zip srcs $ cycle [-1, 1]) $ \(src, x) ->
    AL.sourcePosition src AL.$= AL.Vertex3 x 0 0
  let source = mapOutput concat $ sequenceSources $ map (`load` 1000) hnds
      sink = void $ sequenceSinks $ do
        (i, src) <- zip [0..] srcs
        return $ mapInput (!! i) (const Nothing) $ supply src 5
      isFull = fmap (all (>= 4)) $ mapM (AL.get . AL.buffersQueued) srcs
  _tid <- forkIO $ source $$ stretch 44100 (length srcs) 1.2 1 =$= sink
  fix $ \loop -> isFull >>= \b -> unless b loop
  AL.play srcs

  let draw = do
        zero $ SDL.renderCopy rend tex nullPtr nullPtr
        SDL.renderPresent rend
  draw
  fix $ \loop -> do
    pollEvent >>= \case
      Just (SDL.QuitEvent {}) -> do
        SDL.destroyWindow window
        Image.imgQuit
        SDL.quit
      Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_SIZE_CHANGED }) -> do
        -- Let user adjust height, but reset width to 724
        height <- alloca $ \pw -> alloca $ \ph -> do
          SDL.getWindowSize window pw ph
          peek ph
        SDL.setWindowSize window 724 height
        draw
        loop
      Just _ -> loop
      Nothing -> threadDelay 1000 >> loop

notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else return p

zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing
