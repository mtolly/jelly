{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import           Control.Applicative     ((<$>))
import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import           Control.Exception       (bracket, bracket_)
import           Control.Monad           (forM, forM_, unless, when)
import           Control.Monad.Fix       (fix)
import           Data.Conduit            (($$), (=$=))
import           Data.Maybe              (catMaybes)
import           Foreign                 (Ptr, Word32, alloca, nullPtr, peek,
                                          poke, (.|.))
import           Foreign.C               (CInt, peekCString, withCString)
import qualified Graphics.UI.SDL         as SDL
import qualified Graphics.UI.SDL.Image   as Image
import qualified Sound.File.Sndfile      as Snd
import           Sound.OpenAL            (($=))
import qualified Sound.OpenAL            as AL
import           System.Environment      (getArgs, getProgName)

import           Audio
import           Jammit

main :: IO ()
main = do

  song <- getArgs >>= \case
    [song] -> return song
    _      -> getProgName >>= \pn -> error $ "Usage: "++pn++" song-folder"

  withALContext
    $ withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
    $ withSDLImage [Image.InitPNG]
    $ withWindowAndRenderer "Jelly" sheetWidth 480 SDL.SDL_WINDOW_RESIZABLE
    $ \window rend -> do

    Just info <- loadInfo   song
    Just trks <- loadTracks song
    Just bts  <- loadBeats  song
    let fileTrks = filter (\t -> trackClass t == "JMFileTrack") trks

    imgs       <- findNotation (head fileTrks) song
    Right texs <- sequence <$> mapM (Image.imgLoadTexture rend) imgs
    let rows   =  splitRows (head fileTrks) texs
    audio      <- catMaybes <$> mapM (`findAudio` song) fileTrks

    putStrLn $ "Title: " ++ title info
    putStrLn $ "Tracks: " ++ show (map trackTitle fileTrks)

    hnds <- forM audio $ \a ->
      Snd.openFile a Snd.ReadMode Snd.defaultInfo
    srcs <- AL.genObjectNames $ length hnds * 2
    forM_ (zip srcs $ cycle [-1, 1]) $ \(src, x) ->
      AL.sourcePosition src $= AL.Vertex3 x 0 0

    let
      updatePosition ps = do
        let ss = stopState ps
        tend <- fromIntegral <$> SDL.getTicks
        let diffSeconds = fromIntegral (tend - startTicks ps) / 1000
        return $ audioPosn ss + diffSeconds / playSpeed ss
    let
      isFull = all (>= sinkQueueSize) <$> mapM (AL.get . AL.buffersQueued) srcs
      start ss = do
        -- Seek all the audio handles to our saved position
        forM_ hnds $ \hnd -> Snd.hSeek hnd Snd.AbsoluteSeek $ round $
          audioPosn ss * fromIntegral (Snd.samplerate $ Snd.hInfo hnd)
        -- Start the audio conduit, and wait for it to fill the queues
        mvar <- newEmptyMVar
        let sink = supply srcs sinkQueueSize mvar
            pipeline 1 = load hnds $$ sink
            pipeline speed
              =   load hnds
              $$  stretch 44100 (length srcs) speed 1
              =$= convert
              =$= sink
        _ <- forkIO $ pipeline $ playSpeed ss
        fix $ \loop -> isFull >>= \full -> unless full loop
        -- Mark the sdl ticks when we started audio
        starttks <- fromIntegral <$> SDL.getTicks
        -- Start audio playback
        AL.play srcs
        return $ PlayState
          { startTicks  = starttks
          , audioThread = mvar
          , stopState   = ss
          }
    let
      stop ps = do
        newpn <- updatePosition ps
        -- Kill the audio thread
        putMVar (audioThread ps) () -- signal the thread
        putMVar (audioThread ps) () -- wait until thread removes the last ()
        -- Stop audio, clear queues and delete buffers
        AL.rewind srcs
        forM_ srcs $ \src
          ->  AL.get (AL.buffersProcessed src)
          >>= AL.unqueueBuffers src
          >>= AL.deleteObjectNames
        return $ (stopState ps) { audioPosn = newpn }
    let
      draw s = do
        pn <- case s of
          Stopped StopState{audioPosn = pn} -> return pn
          Playing playstate -> updatePosition playstate
        let rowN = rowNumber bts pn
        zero $ SDL.renderClear rend
        renderRow rend (concat $ drop rowN rows) (0, 0)
        SDL.renderPresent rend
    let
      editStopped s statef = case s of
        Stopped ss -> return $ Stopped $ statef ss
        Playing ps -> do
          ss <- stop ps
          threadDelay 100000
          fmap Playing $ start $ statef ss
    let
      loop s = do
        draw s
        threadDelay 16000 -- ehhh, fix this later
        eloop s
      eloop s = pollEvent >>= \case
        Just (SDL.QuitEvent {}) -> case s of
          Playing ps -> stop ps >> return ()
          Stopped _  -> return ()
        Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
          -- Let user adjust height, but reset width to sheetWidth
          height <- alloca $ \pw -> alloca $ \ph -> do
            SDL.getWindowSize window pw ph
            peek ph
          SDL.setWindowSize window sheetWidth height
          eloop s
        Just (KeyPress SDL.SDL_SCANCODE_SPACE) -> case s of
          Playing ps -> stop ps >>= eloop . Stopped
          Stopped ss -> start ss >>= eloop . Playing
        Just (KeyPress SDL.SDL_SCANCODE_LEFT) ->
          editStopped s (\ss -> ss { audioPosn = max 0 $ audioPosn ss - 5 }) >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_RIGHT) ->
          editStopped s (\ss -> ss { audioPosn = audioPosn ss + 5 }) >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_S) ->
          editStopped s (\ss -> ss { playSpeed = if playSpeed ss == 1 then 1.25 else 1 })
            >>= eloop
        Just _  -> eloop s
        Nothing -> loop  s

    loop $ Stopped StopState{ audioPosn = 0, playSpeed = 1 }

-- | The number of blocks that audio sources should be filled up to.
sinkQueueSize :: (Integral a) => a
sinkQueueSize = 10

data StopState = StopState
  { audioPosn :: Double -- seconds
  , playSpeed :: Double -- ratio of playback secs to original secs
  } deriving (Eq, Ord, Show, Read)

data PlayState = PlayState
  { startTicks  :: Int -- sdl ticks
  , audioThread :: MVar ()
  , stopState   :: StopState
  } deriving (Eq)

data State
  = Stopped StopState
  | Playing PlayState
  deriving (Eq)

pattern KeyPress scan <- SDL.KeyboardEvent
  { SDL.eventType           = SDL.SDL_KEYDOWN
  , SDL.keyboardEventRepeat = 0
  , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scan }
  }

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

withALContext :: IO a -> IO a
withALContext act = bracket
  — AL.openDevice Nothing
    >>= maybe (error "couldn't open audio device") return
  — AL.closeDevice
  — \dev -> bracket
    — AL.createContext dev []
      >>= maybe (error "couldn't create audio context") return
    — AL.destroyContext
    — \ctxt -> bracket_
      — AL.currentContext $= Just ctxt
      — AL.currentContext $= Nothing
      — act

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

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> Just <$> peek pevt
  _ -> return Nothing

-- | Renders a sequence of images at 1:1 aspect ratio in a vertical sequence.
-- Stops rendering images once they go below the window's bottom edge.
renderRow :: SDL.Renderer -> [(SDL.Texture, SDL.Rect)] -> (CInt, CInt) -> IO ()
renderRow _    []              _      = return ()
renderRow rend ((t, r) : rest) (x, y) = do
  h <- alloca $ \ph -> do
    zero $ SDL.getRendererOutputSize rend nullPtr ph
    peek ph
  when (y < h) $ do
    alloca $ \p0 -> alloca $ \p1 -> do
      poke p0 r
      poke p1 $ SDL.Rect x y (SDL.rectW r) (SDL.rectH r)
      zero $ SDL.renderCopy rend t p0 p1
    renderRow rend rest (x, y + SDL.rectH r)

-- | Splits sheet music images into a list of rows, where each row is a
-- sequence of texture sections to be rendered in a vertical requence.
splitRows :: Track -> [SDL.Texture] -> [[(SDL.Texture, SDL.Rect)]]
splitRows trk = go 0 where
  Just height = fromIntegral <$> scoreSystemInterval trk
  go _ []          = []
  go y ts@(t : tt) = case compare (y + height) sheetHeight of
    LT -> [(t, SDL.Rect 0 y sheetWidth height)] : go (y + height) ts
    EQ -> [(t, SDL.Rect 0 y sheetWidth height)] : go 0 tt
    GT -> let
      thisRect = SDL.Rect 0 y sheetWidth $ sheetHeight - y
      in case tt of
        []     -> [(t, thisRect)] : []
        t' : _ -> let
          nextHeight = height - SDL.rectH thisRect
          nextRect = SDL.Rect 0 0 sheetWidth nextHeight
          in [(t, thisRect), (t', nextRect)] : go nextHeight tt

-- | Given the contents of beats.plist, translates a position in seconds
-- to the sheet music row number we're on (starting from 0).
rowNumber :: [Beat] -> Double -> Int
rowNumber bts pos = let
  downs = map position $ filter isDownbeat bts
  in div (max 0 $ length (takeWhile (< pos) downs) - 2) 2

-- | Clever syntax trick: turn function call arguments into \"bulleted lists\".
-- See definitions of "withWindowAndRenderer", "withALContext", etc.
(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —
