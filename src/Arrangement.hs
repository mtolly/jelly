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
  = Whole SDL.Texture
  | Crop SDL.Rect SDL.Texture
  | Rectangle (CInt, CInt) SDL.Color
  | Label a (Arrangement a)
  | Row [Arrangement a]
  | Column [Arrangement a]
  deriving (Eq, Show, Functor)

findLabel :: (CInt, CInt) -> Arrangement a -> IO (Maybe a)
findLabel (x, y) = \case
  Whole _ -> return Nothing
  Crop _ _ -> return Nothing
  Rectangle _ _ -> return Nothing
  Label lbl arr -> do
    (w, h) <- getDims arr
    return $ do
      guard $ (0 <= x && x < w) && (0 <= y && y < h)
      Just lbl
  Row [] -> return Nothing
  Row (ar : ars) -> do
    (w, _) <- getDims ar
    if x < w
      then findLabel (x, y) ar
      else findLabel (x - w, y) $ Row ars
  Column [] -> return Nothing
  Column (ar : ars) -> do
    (_, h) <- getDims ar
    if y < h
      then findLabel (x, y) ar
      else findLabel (x, y - h) $ Column ars

getDims :: Arrangement a -> IO (CInt, CInt)
getDims (Whole tex) = alloca $ \pw -> alloca $ \ph -> do
  zero $ SDL.queryTexture tex nullPtr nullPtr pw ph
  liftA2 (,) (peek pw) (peek ph)
getDims (Crop (SDL.Rect _ _ w h) _) = return (w, h)
getDims (Row arrs) = do
  dims <- mapM getDims arrs
  return (sum $ map fst dims, foldr max 0 $ map snd dims)
getDims (Column arrs) = do
  dims <- mapM getDims arrs
  return (foldr max 0 $ map fst dims, sum $ map snd dims)
getDims (Rectangle dims _) = return dims
getDims (Label _ arr) = getDims arr

render :: SDL.Renderer -> (CInt, CInt) -> Arrangement a -> IO ()
render rend pn a0 = do
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
        Row [] -> return ()
        Row (ar : ars) -> do
          (arx, _) <- getDims ar
          go (x, y) ar
          go (x + arx, y) $ Row ars
        Column [] -> return ()
        Column (ar : ars) -> do
          (_, ary) <- getDims ar
          go (x, y) ar
          go (x, y + ary) $ Column ars
        Rectangle (w, h) (SDL.Color r g b a) -> do
          zero $ SDL.setRenderDrawColor rend r g b a
          zero $ with (SDL.Rect x y w h) $ SDL.renderDrawRect rend
        Label _ ar -> go (x, y) ar
  go pn a0
