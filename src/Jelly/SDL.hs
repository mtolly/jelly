{-# LANGUAGE NoImplicitPrelude #-}
module Jelly.SDL where

import           Jelly.Prelude

import           Control.Exception     (bracket, bracket_)
import           Foreign               (Ptr, Word32, nullPtr, (.|.))
import           Foreign.C             (CInt, peekCString, withCString)
import qualified Graphics.UI.SDL       as SDL
import qualified Graphics.UI.SDL.Image as Image

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else pure p

-- | Extracts and throws an SDL error if the action doesn't return zero.
zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_
  — do zero $ SDL.init $ foldr (.|.) 0 flags
  — SDL.quit

withSDLImage :: [Image.InitFlag] -> IO a -> IO a
withSDLImage flags = bracket_
  — Image.imgInit flags
  — Image.imgQuit

withWindowAndRenderer :: String -> CInt -> CInt -> Word32
  -> (SDL.Window -> SDL.Renderer -> IO a) -> IO a
withWindowAndRenderer name w h flags act = bracket
  — do
    withCString name $ \s -> notNull $ SDL.createWindow
      s -- title
      SDL.SDL_WINDOWPOS_UNDEFINED -- x
      SDL.SDL_WINDOWPOS_UNDEFINED -- y
      w -- width
      h -- height
      flags -- flags
  — SDL.destroyWindow
  — \window -> bracket
    — do notNull $ SDL.createRenderer window (-1) 0
    — SDL.destroyRenderer
    — \renderer -> act window renderer
