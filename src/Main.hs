{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign
import Foreign.C
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import Jammit
import System.FilePath ((</>))

main :: IO ()
main = do
  zero $ SDL.init $ SDL.SDL_INIT_TIMER .|. SDL.SDL_INIT_VIDEO
  Image.imgInit [Image.InitPNG]

  window <- withCString "Jelly" $ \str ->
    notNull $ SDL.createWindow
      str -- title
      SDL.SDL_WINDOWPOS_UNDEFINED -- x
      SDL.SDL_WINDOWPOS_UNDEFINED -- y
      640 -- width
      480 -- height
      SDL.SDL_WINDOW_RESIZABLE -- flags
  rend <- notNull $ SDL.createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED

  let song = "jammit/0F9BABA8-84AA-4E06-B5DE-D88AD05FB659/"
  Just info <- loadInfo song
  Just trks <- loadTracks song
  putStrLn $ "Title: " ++ title info
  let trk = head trks
      img = song </> identifier trk ++ "_jcfn_00"
  putStrLn $ "Track: " ++ show (trackTitle trk)
  Right tex <- Image.imgLoadTexture rend img

  fix $ \loop -> do
    zero $ SDL.renderCopy rend tex nullPtr nullPtr
    SDL.renderPresent rend
    pollEvent >>= \case
      Just (SDL.QuitEvent {}) -> do
        SDL.destroyWindow window
        Image.imgQuit
        SDL.quit
      _ -> threadDelay 1000 >> loop

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
