{-# LANGUAGE LambdaCase #-}
module TTF
( withTTF
, openFont
, renderUTF8Blended
) where

import Control.Exception (bracket_)
import Util
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL

foreign import ccall unsafe "TTF_Init" c_initTTF :: IO CInt
foreign import ccall unsafe "TTF_Quit" c_quitTTF :: IO ()

withTTF :: IO a -> IO a
withTTF = bracket_
  — c_initTTF >>= \case
    0 -> return ()
    e -> error $ "withTTF: TTF_Init returned " ++ show e
  — c_quitTTF

data Font_
type Font = Ptr Font_

foreign import ccall unsafe "TTF_OpenFont" c_openFont
  :: CString -> CInt -> IO Font

openFont :: FilePath -> Int -> IO Font
openFont s pt = withCString s $ \pc -> notNull $ c_openFont pc $ fromIntegral pt

foreign import ccall unsafe "hs_TTF_RenderUTF8_Blended" c_renderUTF8Blended
  :: Font -> CString -> Word8 -> Word8 -> Word8 -> Word8 -> IO (Ptr SDL.Surface)

renderUTF8Blended :: Font -> String -> SDL.Color -> IO (Ptr SDL.Surface)
renderUTF8Blended f s (SDL.Color r g b a) =
  withCString s $ \pc -> notNull $ c_renderUTF8Blended f pc r g b a
