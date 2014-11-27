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

import Jammit
import Audio

main :: IO ()
main = do
  song <- getArgs >>= \case
    [song] -> return song
    _      -> getProgName >>= \pn -> error $ "Usage: "++pn++" song-folder"

  withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
    $ withSDLImage [Image.InitPNG]
    $ withWindowAndRenderer "Jelly" sheetWidth 480 SDL.SDL_WINDOW_RESIZABLE
    $ \window rend -> do

    Just info <- loadInfo song
    Just trks <- loadTracks song
    let fileTrks = filter (\t -> trackClass t == "JMFileTrack") trks
    Just bts <- loadBeats song

    imgs <- findNotation (head fileTrks) song
    Right texs <- fmap sequence $ mapM (Image.imgLoadTexture rend) imgs
    let rows = splitRows (head fileTrks) texs
    audio <- fmap catMaybes $ mapM (`findAudio` song) fileTrks

    putStrLn $ "Title: " ++ title info
    putStrLn $ "Tracks: " ++ show (map trackTitle fileTrks)

    withALContext $ do
      hnds <- forM audio $ \a ->
        Snd.openFile a Snd.ReadMode Snd.defaultInfo
      srcs <- AL.genObjectNames $ length hnds * 2
      forM_ (zip srcs $ cycle [-1, 1]) $ \(src, x) ->
        AL.sourcePosition src AL.$= AL.Vertex3 x 0 0
      let source = mapOutput concat $ sequenceSources $ map (`load` 1000) hnds
          sink = void $ sequenceSinks $ do
            (i, src) <- zip [0..] srcs
            return $ mapInput (!! i) (const Nothing) $ supply src 5
          isFull = fmap (all (>= 4)) $ mapM (AL.get . AL.buffersQueued) srcs
          stretchTime = 1.2
      tid <- forkIO $ source $$ stretch 44100 (length srcs) stretchTime 1 =$= sink
      fix $ \loop -> isFull >>= \b -> unless b loop
      start <- SDL.getTicks
      AL.play srcs

      let draw = do
            now <- SDL.getTicks
            let rowN = rowNumber bts $ (fromIntegral (now - start) / 1000) / stretchTime
            zero $ SDL.renderClear rend
            renderRow rend (rows !! rowN) (0, 0)
            SDL.renderPresent rend
      fixFrames 16 $ \loop -> do
        draw
        fix $ \eloop -> pollEvent >>= \case
          Just (SDL.QuitEvent {}) -> do
            killThread tid
          Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
            -- Let user adjust height, but reset width to sheetWidth
            height <- alloca $ \pw -> alloca $ \ph -> do
              SDL.getWindowSize window pw ph
              peek ph
            SDL.setWindowSize window sheetWidth height
            eloop
          Just _ -> eloop
          Nothing -> loop

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
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

-- | Renders a sequence of images at 1:1 aspect ratio in a vertical sequence.
renderRow :: SDL.Renderer -> [(SDL.Texture, SDL.Rect)] -> (CInt, CInt) -> IO ()
renderRow _    []              _      = return ()
renderRow rend ((t, r) : rest) (x, y) = do
  alloca $ \p0 -> alloca $ \p1 -> do
    poke p0 r
    poke p1 $ SDL.Rect x y (SDL.rectW r) (SDL.rectH r)
    zero $ SDL.renderCopy rend t p0 p1
  renderRow rend rest (x, y + SDL.rectH r)

-- | Splits sheet music images into a list of rows, where each row is a
-- sequence of texture sections to be rendered in a vertical requence.
splitRows :: Track -> [SDL.Texture] -> [[(SDL.Texture, SDL.Rect)]]
splitRows trk = go 0 where
  Just height = fmap fromIntegral $ scoreSystemInterval trk
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
