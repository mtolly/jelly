{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign hiding (void)
import Foreign.C
import Control.Monad (unless, forM, forM_)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Exception (bracket, bracket_)
import qualified Sound.OpenAL as AL
import qualified Sound.File.Sndfile as Snd
import Data.Conduit
import Data.Functor (void)
import System.Environment (getArgs, getProgName)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Data.IORef

import Jammit
import Audio

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

    Just info <- loadInfo song
    Just trks <- loadTracks song
    let fileTrks = filter (\t -> trackClass t == "JMFileTrack") trks
    Just bts <- loadBeats song

    imgs <- findNotation (head fileTrks) song
    Right texs <- sequence <$> mapM (Image.imgLoadTexture rend) imgs
    let rows = splitRows (head fileTrks) texs
    audio <- catMaybes <$> mapM (`findAudio` song) fileTrks

    putStrLn $ "Title: " ++ title info
    putStrLn $ "Tracks: " ++ show (map trackTitle fileTrks)

    hnds <- forM audio $ \a ->
      Snd.openFile a Snd.ReadMode Snd.defaultInfo
    srcs <- AL.genObjectNames $ length hnds * 2
    forM_ (zip srcs $ cycle [-1, 1]) $ \(src, x) ->
      AL.sourcePosition src AL.$= AL.Vertex3 x 0 0
    let source = mapOutput concat $ sequenceSources $ map (`load` 1000) hnds
        sink = void $ sequenceSinks $ do
          (i, src) <- zip [0..] srcs
          return $ mapInput (!! i) (const Nothing) $ supply src 5
        pipeline speed = source $$ stretch 44100 (length srcs) speed 1 =$= sink
        isFull = all (>= 4) <$> mapM (AL.get . AL.buffersQueued) srcs

    audioPosn   <- newIORef 0       -- seconds
    startedAt   <- newIORef Nothing -- sdl ticks
    audioThread <- newIORef Nothing -- thread id
    playSpeed   <- newIORef 1       -- ratio of playback secs to original secs
    let start = do
          -- Seek all the audio handles to our saved position
          pn <- readIORef audioPosn
          forM_ hnds $ \hnd -> Snd.hSeek hnd Snd.AbsoluteSeek
            $ round $ pn * fromIntegral (Snd.samplerate $ Snd.hInfo hnd)
          -- Start the audio conduit, and wait for it to fill the queues
          speed <- readIORef playSpeed
          forkIO (pipeline speed) >>= writeIORef audioThread . Just
          fix $ \loop -> isFull >>= \full -> unless full loop
          -- Mark the sdl ticks when we started audio
          SDL.getTicks >>= writeIORef startedAt . Just
          -- Start audio playback
          AL.play srcs
        stop = readIORef startedAt >>= \case
          Nothing -> return ()
          Just tstart -> do
            -- Update the audio position
            writeIORef startedAt Nothing
            tend <- SDL.getTicks
            let diffSeconds = fromIntegral (tend - tstart) / 1000
            speed <- readIORef playSpeed
            modifyIORef audioPosn (+ diffSeconds / speed)
            -- Stop audio, clear queues and delete buffers
            AL.rewind srcs
            forM_ srcs $ \src
              ->  AL.get (AL.buffersProcessed src)
              >>= AL.unqueueBuffers src
              >>= AL.deleteObjectNames
            -- Kill the audio thread
            readIORef audioThread >>= maybe (return ()) killThread
            writeIORef audioThread Nothing
        draw = do
          pn <- readIORef startedAt >>= \case
            Nothing -> readIORef audioPosn
            Just tstart -> do
              tend <- SDL.getTicks
              let diffSeconds = fromIntegral (tend - tstart) / 1000
              speed <- readIORef playSpeed
              (+ diffSeconds / speed) <$> readIORef audioPosn
          let rowN = rowNumber bts pn
          zero $ SDL.renderClear rend
          renderRow rend (concat $ drop rowN rows) (0, 0)
          SDL.renderPresent rend
        pauseThenDo act = readIORef startedAt >>= \case
          Nothing -> act
          Just _  -> stop >> act >> threadDelay 100000 >> start
    fixFrames 16 $ \loop -> do
      draw
      fix $ \eloop -> pollEvent >>= \case
        Just (SDL.QuitEvent {}) -> stop
        Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
          -- Let user adjust height, but reset width to sheetWidth
          height <- alloca $ \pw -> alloca $ \ph -> do
            SDL.getWindowSize window pw ph
            peek ph
          SDL.setWindowSize window sheetWidth height
          eloop
        Just (KeyPress SDL.SDL_SCANCODE_SPACE) -> readIORef startedAt >>= \case
          Nothing -> start >> eloop
          Just _  -> stop  >> eloop
        Just (KeyPress SDL.SDL_SCANCODE_LEFT) -> do
          pauseThenDo $ modifyIORef audioPosn $ \pn -> max 0 $ pn - 5
          eloop
        Just (KeyPress SDL.SDL_SCANCODE_RIGHT) -> do
          pauseThenDo $ modifyIORef audioPosn (+ 5)
          eloop
        Just (KeyPress SDL.SDL_SCANCODE_S) -> do
          pauseThenDo $ modifyIORef playSpeed $ \case
            1 -> 1.25
            _ -> 1
          eloop
        Just _ -> eloop
        Nothing -> loop

pattern KeyPress scan <- SDL.KeyboardEvent
  { SDL.eventType = SDL.SDL_KEYDOWN
  , SDL.keyboardEventRepeat = 0
  , SDL.keyboardEventKeysym = SDL.Keysym
    { SDL.keysymScancode = scan }
  }

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_
  (zero $ SDL.init $ foldr (.|.) 0 flags)
  SDL.quit

withSDLImage :: [Image.InitFlag] -> IO a -> IO a
withSDLImage flags = bracket_
  (Image.imgInit flags)
  Image.imgQuit

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
    — notNull (SDL.createRenderer window (-1) 0)
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
      — AL.currentContext AL.$= Just ctxt
      — AL.currentContext AL.$= Nothing
      — act

-- | Like using "fix" to create a loop, except we start 2 iterations of the loop
-- no faster than the given number of milliseconds apart.
fixFrames :: Word32 -> (IO a -> IO a) -> IO a
fixFrames frameTime loop = go where
  go = do
    start <- SDL.getTicks
    loop $ do
      end <- SDL.getTicks
      let micro :: Int
          micro = fromIntegral frameTime - (fromIntegral end - fromIntegral start)
      threadDelay $ max 0 micro * 1000
      go

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
  h <- alloca $ \pw -> alloca $ \ph -> do
    zero $ SDL.getRendererOutputSize rend pw ph
    peek ph
  if y >= h
    then return ()
    else do
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

(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —
