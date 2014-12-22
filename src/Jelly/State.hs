{-# LANGUAGE TemplateHaskell #-}
module Jelly.State where

import Data.Word (Word32)
import qualified Control.Lens as L
import Control.Lens.Operators ((^.))
import Control.Monad.Trans.RWS (RWST)
import qualified Graphics.UI.SDL as SDL
import Jelly.Jammit
import Jelly.AudioPipe
import Jelly.Arrangement

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

L.makeLenses ''Static
L.makeLenses ''Volatile

initialVolatile :: Static -> Volatile
initialVolatile static = Volatile
  { _stoppedPosn = 0
  , _playSpeed   = 1
  , _audioGains  = map (const 1) $ static ^. audioParts
  , _sheetShow   = zipWith const (True : repeat False) $ static ^. sheetParts
  , _startTicks  = Nothing
  }
