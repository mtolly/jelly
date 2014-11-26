{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import BasicPrelude
import qualified Graphics.UI.SDL as SDL
import Foreign
import Foreign.C
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  zero $ SDL.init $ SDL.SDL_INIT_TIMER .|. SDL.SDL_INIT_VIDEO
  window <- withCString "Jelly" $ \str ->
    notNull $ SDL.createWindow
      str -- title
      SDL.SDL_WINDOWPOS_UNDEFINED -- x
      SDL.SDL_WINDOWPOS_UNDEFINED -- y
      640 -- width
      480 -- height
      0 -- flags
  _rend <- notNull $ SDL.createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED
  fix $ \loop -> do
    pollEvent >>= \case
      Just (SDL.QuitEvent {}) -> do
        SDL.destroyWindow window
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
