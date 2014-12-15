module Util where

import Foreign (Ptr, nullPtr)
import Foreign.C (peekCString)
import qualified Graphics.UI.SDL as SDL
import Control.Monad (unless)

-- | Clever syntax trick: turn function call arguments into \"bulleted lists\".
-- See definitions of "withWindowAndRenderer", "withALContext", etc.
(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else return p

-- | Extracts and throws an SDL error if the action doesn't return zero.
zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error
