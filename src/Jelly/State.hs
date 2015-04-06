{-# LANGUAGE NoImplicitPrelude #-}
module Jelly.State where

import Data.Word (Word32)
import qualified Control.Lens as L
import Control.Lens.Operators ((^.))
import Control.Monad.Trans.RWS (RWST, evalRWST)
import qualified Graphics.UI.SDL as SDL
import Jelly.Jammit
import Jelly.AudioPipe
import Jelly.Arrangement
import Jelly.Prelude

-- | State that doesn't change once everything is loaded
data Static = Static
  { _window     :: SDL.Window
  , _renderer   :: SDL.Renderer
  , _audioPipe  :: AudioPipe
  , _audioParts :: [Maybe Part]
  , _sheetParts :: [(SheetPart, [Arrangement ()])]
  , _beats      :: [Beat]
  , _interface  :: String -> SDL.Texture
  }

data Volatile = Volatile
  { _stoppedPosn :: Double -- ^ seconds
  , _playSpeed   :: Double -- ^ ratio of playback secs to original secs
  , _audioGains  :: [Double]
  , _sheetShow   :: [Bool]
  , _startTicks  :: Maybe Word32 -- ^ sdl ticks, Nothing if stopped
  } deriving (Eq, Ord, Show, Read)

type App = RWST Static () Volatile IO

window :: L.Lens' Static SDL.Window
window = L.lens _window (\s x -> s { _window = x })
renderer :: L.Lens' Static SDL.Renderer
renderer = L.lens _renderer (\s x -> s { _renderer = x })
audioPipe :: L.Lens' Static AudioPipe
audioPipe = L.lens _audioPipe (\s x -> s { _audioPipe = x })
audioParts :: L.Lens' Static [Maybe Part]
audioParts = L.lens _audioParts (\s x -> s { _audioParts = x })
sheetParts :: L.Lens' Static [(SheetPart, [Arrangement ()])]
sheetParts = L.lens _sheetParts (\s x -> s { _sheetParts = x })
beats :: L.Lens' Static [Beat]
beats = L.lens _beats (\s x -> s { _beats = x })
interface :: L.Lens' Static (String -> SDL.Texture)
interface = L.lens _interface (\s x -> s { _interface = x })

stoppedPosn :: L.Lens' Volatile Double
stoppedPosn = L.lens _stoppedPosn (\s x -> s { _stoppedPosn = x })
playSpeed :: L.Lens' Volatile Double
playSpeed = L.lens _playSpeed (\s x -> s { _playSpeed = x })
audioGains :: L.Lens' Volatile [Double]
audioGains = L.lens _audioGains (\s x -> s { _audioGains = x })
sheetShow :: L.Lens' Volatile [Bool]
sheetShow = L.lens _sheetShow (\s x -> s { _sheetShow = x })
startTicks :: L.Lens' Volatile (Maybe Word32)
startTicks = L.lens _startTicks (\s x -> s { _startTicks = x })

initialVolatile :: Static -> Volatile
initialVolatile static = Volatile
  { _stoppedPosn = 0
  , _playSpeed   = 1
  , _audioGains  = map (const 1) $ static ^. audioParts
  , _sheetShow   = zipWith const (True : repeat False) $ static ^. sheetParts
  , _startTicks  = Nothing
  }

evalApp :: (Applicative m, Monad m) => RWST r () s m a -> r -> s -> m a
evalApp f r s = do
  (x, ()) <- evalRWST f r s
  pure x
