{- |
An encapsulation of a grid-based GUI, where rows and columns of textures
are rendered at 1:1 aspect ratio, and specific parts of the grid can be
labelled with the intent of locating mouse clicks.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Arrangement where

import qualified Graphics.UI.SDL as SDL
import Foreign
import Foreign.C
import Util
import Control.Applicative
import Control.Monad

data Arrangement a
  = Empty
  | Whole SDL.Texture
  | Crop SDL.Rect SDL.Texture
  | Rectangle (CInt, CInt) SDL.Color
  | Arrangement a `Beside` Arrangement a -- ^ left, right
  | Arrangement a `Above`  Arrangement a -- ^ above, below
  | Arrangement a `Behind` Arrangement a -- ^ back, front
  | Label a (Arrangement a)
  deriving (Eq, Show, Functor)

row, column, layers :: [Arrangement a] -> Arrangement a
row    = foldr Beside Empty
column = foldr Above  Empty
layers = foldr Behind Empty

-- | Given a location, finds the label that it is located in, as well as its
-- position within the label.
findLabel :: (CInt, CInt) -> Arrangement a -> IO (Maybe (a, (CInt, CInt)))
findLabel (x, y) = \case
  Whole _ -> return Nothing
  Crop _ _ -> return Nothing
  Rectangle _ _ -> return Nothing
  Label lbl arr -> do
    (w, h) <- getDims arr
    return $ do
      guard $ (0 <= x && x < w) && (0 <= y && y < h)
      Just (lbl, (x, y))
  Empty -> return Nothing
  a0 `Beside` a1 -> do
    w <- getWidth a0
    if x < w
      then findLabel (x    , y) a0
      else findLabel (x - w, y) a1
  a0 `Above` a1 -> do
    h <- getHeight a0
    if y < h
      then findLabel (x, y    ) a0
      else findLabel (x, y - h) a1
  a0 `Behind` a1 -> findLabel (x, y) a1 >>= \case
    Just res -> return $ Just res
    Nothing  -> findLabel (x, y) a0

getDims :: Arrangement a -> IO (CInt, CInt)
getDims (Whole tex) = alloca $ \pw -> alloca $ \ph -> do
  zero $ SDL.queryTexture tex nullPtr nullPtr pw ph
  liftA2 (,) (peek pw) (peek ph)
getDims (Crop (SDL.Rect _ _ w h) _) = return (w, h)
getDims (a0 `Beside` a1) = do
  (w0, h0) <- getDims a0
  (w1, h1) <- getDims a1
  return (w0 + w1, max h0 h1)
getDims (a0 `Above` a1) = do
  (w0, h0) <- getDims a0
  (w1, h1) <- getDims a1
  return (max w0 w1, h0 + h1)
getDims (a0 `Behind` a1) = do
  (w0, h0) <- getDims a0
  (w1, h1) <- getDims a1
  return (max w0 w1, max h0 h1)
getDims (Rectangle dims _) = return dims
getDims (Label _ arr) = getDims arr
getDims Empty = return (0, 0)

getWidth, getHeight :: Arrangement a -> IO CInt
getWidth  = fmap fst . getDims
getHeight = fmap snd . getDims

render :: SDL.Renderer -> (CInt, CInt) -> Arrangement a -> IO ()
render rend pn arrange = do
  (windowW, windowH) <- alloca $ \pw -> alloca $ \ph -> do
    zero $ SDL.getRendererOutputSize rend pw ph
    liftA2 (,) (peek pw) (peek ph)
  let go (x, y) arr = when (x < windowW && y < windowH) $ case arr of
        Whole tex -> do
          (w, h) <- getDims arr
          zero $ with (SDL.Rect x y w h) $ SDL.renderCopy rend tex nullPtr
        Crop r tex -> zero $
          with r $ \p0 ->
            with (SDL.Rect x y (SDL.rectW r) (SDL.rectH r)) $ \p1 ->
              SDL.renderCopy rend tex p0 p1
        a0 `Beside` a1 -> do
          w <- getWidth a0
          go (x    , y) a0
          go (x + w, y) a1
        a0 `Above` a1 -> do
          h <- getHeight a0
          go (x, y    ) a0
          go (x, y + h) a1
        a0 `Behind` a1 -> do
          go (x, y) a0
          go (x, y) a1
        Rectangle (w, h) (SDL.Color r g b a) -> do
          zero $ SDL.setRenderDrawColor rend r g b a
          zero $ with (SDL.Rect x y w h) $ SDL.renderDrawRect rend
        Label _ ar -> go (x, y) ar
        Empty -> return ()
  go pn arrange
