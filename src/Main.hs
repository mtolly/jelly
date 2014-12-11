{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP             #-}
module Main (main) where

import           Control.Applicative     ((<$>), liftA2)
import           Control.Concurrent      (threadDelay)
import           Control.Exception       (bracket, bracket_)
import           Control.Monad           (forM, guard, unless, when)
import           Data.Char               (toLower)
import           Data.List               (intercalate, nub, transpose)
import           Data.Maybe              (fromJust)
import           Foreign                 (Ptr, Word32, alloca, nullPtr, peek,
                                          with, (.|.))
import           Foreign.C               (CInt, peekCString, withCString)
import qualified Graphics.UI.SDL         as SDL
import qualified Graphics.UI.SDL.Image   as Image
import           Sound.OpenAL            (($=))
import qualified Sound.OpenAL            as AL
import           System.FilePath         ((<.>), (</>))

import           Jammit
import           AudioPipe

#ifndef MACAPP
import Paths_jelly (getDataFileName)
import System.Environment (getArgs, getProgName)
#else
import Foreign (free)
import Foreign.C (CString)
import System.Environment.FindBin (getProgPath)

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = getProgPath >>= \d ->
  return $ d </> "../Resources" </> f

foreign import ccall unsafe
  "macSelectDir"
  c_macSelectDir :: CString -> IO CString

macSelectDir :: IO (Maybe FilePath)
macSelectDir = do
  jmt <- findJammitDir
  p <- case jmt of
    Nothing -> c_macSelectDir nullPtr
    Just d  -> withCString d c_macSelectDir
  if p == nullPtr
    then return Nothing
    else do
      s <- peekCString p
      free p
      return $ Just s
#endif

data Sheet = Sheet
  { sheetPart :: SheetPart
  , sheetRows :: [[(SDL.Texture, SDL.Rect)]]
  }

main :: IO ()
main = do

#ifdef MACAPP
  songs <- macSelectDir >>= \case
    Nothing   -> error "No song folder selected; quitting app."
    Just song -> return [song]
#else
  songs <- getArgs >>= \case
    []    -> getProgName >>= \pn -> error $ "Usage: "++pn++" dir1 [dir2 ...]"
    songs -> return songs
#endif

  withALContext
    $ withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
    $ withSDLImage [Image.InitPNG]
    $ withWindowAndRenderer "Jelly" sheetWidth 480 SDL.SDL_WINDOW_RESIZABLE
    $ \window rend -> do

    Just allInfo <- fmap sequence $ mapM loadInfo   songs
    Just allTrks <- fmap sequence $ mapM loadTracks songs
    Just bts     <- loadBeats $ head songs

    theTitle <- case nub $ map title allInfo of
      [x] -> return x
      xs -> error $ "Different song titles: " ++ show xs
    _ <- case nub $ map artist allInfo of
      [x] -> return x
      xs -> error $ "Different song artists: " ++ show xs
    let theInstruments = map instrument allInfo

    let fileTrks = do
          (song, inst, instTrks) <- zip3 songs theInstruments allTrks
          trk <- instTrks
          guard $ trackClass trk == "JMFileTrack"
          let Just apart = titleToAudioPart (fromJust $ trackTitle trk) inst
          return (song, apart, trk)

    sheets <- fmap concat $ forM fileTrks $ \(song, apart, trk) -> case apart of
      Without _ -> return []
      Only part -> do
        let findRows f = do
              pages <- f trk song
              Right texs <- sequence <$> mapM (Image.imgLoadTexture rend) pages
              return $ splitRows trk texs
        notes <- findRows findNotation
        tab   <- findRows findTab
        return $ if null tab
          then [Sheet (Notation part) notes]
          else [Sheet (Notation part) notes, Sheet (Tab part) tab]
    audio <- forM fileTrks $ \(song, apart, trk) -> do
      Just aud <- findAudio trk song
      return (apart, aud)

    let toggleImages = (++)
          — map (map toLower . drop 4 . show) ([minBound .. maxBound] :: [Part])
          — words "band guitar1tab guitar2tab basstab slow"
        simpleImages = words "play stop divider left right"
        allImages = simpleImages ++ toggleImages ++ map ("no_" ++) toggleImages
    images <- forM allImages $ \img -> do
      dataLoc <- getDataFileName $ "img" </> img <.> "png"
      Right tex <- Image.imgLoadTexture rend dataLoc
      return (img, tex)
    let getImage str = case lookup str images of
          Just img -> img
          Nothing  -> error $ "Couldn't find image: " ++ show str

    putStrLn $ "Loaded: " ++ theTitle ++ " " ++ show theInstruments

    let normalAudio = [ (p, f) | (Only p, f) <- audio ]
        backAudio = [ (i, f) | (Without i, f) <- audio ]
        bestBack :: (Instrument, FilePath)
        bestBack = head $ do
          i <- [Drums, Guitar, Keyboard, Bass, Vocal]
          filter ((i ==) . fst) backAudio
        normalNegative = do
          (p, f) <- normalAudio
          guard $ partToInstrument p /= fst bestBack
          return f
        labels = map (Only . fst) normalAudio ++ [Without $ fst bestBack]
        inputs :: [([FilePath], [FilePath])]
        inputs = [ ([f], []) | (_, f) <- normalAudio ] ++ [([snd bestBack], normalNegative)]

    withPipe 2 1 inputs
      $ \pipe -> let

      updatePosition ps = do
        let ss = stopState ps
        tend <- fromIntegral <$> SDL.getTicks
        let diffSeconds = fromIntegral (tend - startTicks ps) / 1000
        return $ audioPosn ss + diffSeconds / playSpeed ss

      start ss = do
        playPause pipe
        -- Mark the sdl ticks when we started audio
        starttks <- fromIntegral <$> SDL.getTicks
        return $ PlayState
          { startTicks  = starttks
          , stopState   = ss
          }

      stop ps = do
        playPause pipe
        newpn <- updatePosition ps
        return $ (stopState ps) { audioPosn = newpn }

      draw s = do
        pn <- case s of
          Stopped StopState{audioPosn = pn} -> return pn
          Playing playstate -> updatePosition playstate
        let (rowN, frac) = rowNumber bts pn
            sheetsToDraw = map snd $ filter fst $ zip (sheetShow $ getStopState s) sheets
            sheetStream = case sheetsToDraw of
              [sheet] -> concat $ drop rowN $ sheetRows sheet
              _ -> concat $ intercalate [[(getImage "divider", SDL.Rect 0 0 724 2)]] $
                transpose $ map (drop rowN . sheetRows) sheetsToDraw
            sheetButtons = map
              — do
                \(sheet, isShown) -> (++)
                  — do if isShown then "" else "no_"
                  — case sheetPart sheet of
                    Notation p -> partToFile p
                    Tab p -> partToFile p ++ "tab"
              — do zip sheets $ sheetShow $ getStopState s
            audioButtons = map
              — do
                \(apart, gain) -> (++)
                  — do if gain /= 0 then "" else "no_"
                  — case apart of
                    Only p -> partToFile p
                    Without _ -> "band"
              — do zip labels $ audioGains $ getStopState s
            partToFile = map toLower . drop 4 . show
            buttonRect = SDL.Rect 0 0 100 30
            largeBtnRect = SDL.Rect 0 0 80 60
            thinLargeBtnRect = SDL.Rect 0 0 40 60
            systemHeight = sum $ do
              sheet <- sheetsToDraw
              let row = head $ sheetRows sheet
              rect <- map snd row
              return $ SDL.rectH rect
        zero $ SDL.setRenderDrawColor rend 0 0 0 255
        zero $ SDL.renderClear rend
        renderHorizSeq rend
          [ case s of
            Stopped _ -> (getImage "play", largeBtnRect)
            Playing _ -> (getImage "stop", largeBtnRect)
          , (getImage "left", thinLargeBtnRect)
          , (getImage "right", thinLargeBtnRect)
          , case playSpeed $ getStopState s of
            1 -> (getImage "no_slow", largeBtnRect)
            _ -> (getImage "slow"   , largeBtnRect)
          ] (0, 0)
        renderHorizSeq rend [ (getImage b, buttonRect) | b <- sheetButtons ] (240, 0)
        renderHorizSeq rend [ (getImage b, buttonRect) | b <- audioButtons ] (240, 30)
        renderVertSeq rend sheetStream (0, 60)
        zero $ SDL.setRenderDrawColor rend 255 0 0 255
        zero $ with (SDL.Rect (floor $ frac * 724) 60 2 systemHeight) $
          SDL.renderFillRect rend
        SDL.renderPresent rend

      whileStopped s f = case s of
        Stopped ss -> fmap Stopped $ f ss
        Playing ps -> fmap Playing $ do
          ss <- stop ps
          ps' <- f ss
          threadDelay 75000
          start ps'
      toggleVolume i s = let
        ss = getStopState s
        vol = audioGains ss !! i
        vol' = if vol == 1 then 0 else 1
        gains' = updateNth i vol' $ audioGains ss
        s' = setStopState (ss { audioGains = gains' }) s
        in do
          setVolumes gains' pipe
          return s'
      updateNth i x xs = case splitAt i xs of
        (ys, zs) -> ys ++ [x] ++ drop 1 zs
      flipNth i = zipWith ($) $ updateNth i not $ repeat id
      toggleSheet i = mapStopState $
        \ss -> ss { sheetShow = flipNth i $ sheetShow ss }

      loop s = do
        draw s
        threadDelay 16000 -- ehhh, fix this later
        eloop s
      eloop s = pollEvent >>= \case
        Just (SDL.QuitEvent {}) -> quit
        Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
          -- Let user adjust height, but make width to at least sheetWidth
          (width, height) <- alloca $ \pw -> alloca $ \ph -> do
            SDL.getWindowSize window pw ph
            liftA2 (,) (peek pw) (peek ph)
          SDL.setWindowSize window (max sheetWidth width) height
          eloop s
        Just (KeyPress SDL.SDL_SCANCODE_SPACE) -> togglePlaying
        Just (KeyPress SDL.SDL_SCANCODE_LEFT) -> moveLeft
        Just (KeyPress SDL.SDL_SCANCODE_RIGHT) -> moveRight
        Just (KeyPress SDL.SDL_SCANCODE_LSHIFT) -> toggleSpeed
        Just (KeyPress SDL.SDL_SCANCODE_Z) -> toggleVolume 0 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_X) -> toggleVolume 1 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_C) -> toggleVolume 2 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_V) -> toggleVolume 3 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_B) -> toggleVolume 4 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_N) -> toggleVolume 5 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_M) -> toggleVolume 6 s >>= eloop
        Just (KeyPress SDL.SDL_SCANCODE_A) -> eloop $ toggleSheet 0 s
        Just (KeyPress SDL.SDL_SCANCODE_S) -> eloop $ toggleSheet 1 s
        Just (KeyPress SDL.SDL_SCANCODE_D) -> eloop $ toggleSheet 2 s
        Just (KeyPress SDL.SDL_SCANCODE_F) -> eloop $ toggleSheet 3 s
        Just (KeyPress SDL.SDL_SCANCODE_G) -> eloop $ toggleSheet 4 s
        Just (KeyPress SDL.SDL_SCANCODE_H) -> eloop $ toggleSheet 5 s
        Just (KeyPress SDL.SDL_SCANCODE_J) -> eloop $ toggleSheet 6 s
        Just (SDL.MouseButtonEvent
          { SDL.eventType = SDL.SDL_MOUSEBUTTONDOWN
          , SDL.mouseButtonEventButton = SDL.SDL_BUTTON_LEFT
          , SDL.mouseButtonEventX = mx
          , SDL.mouseButtonEventY = my
          })
          | (mx, my) `insideRect` SDL.Rect 0 0 80 60 -> togglePlaying
          | (mx, my) `insideRect` SDL.Rect 80 0 40 60 -> moveLeft
          | (mx, my) `insideRect` SDL.Rect 120 0 40 60 -> moveRight
          | (mx, my) `insideRect` SDL.Rect 160 0 80 60 -> toggleSpeed
          | (mx, my) `insideRect` SDL.Rect 240 0 9999 30 ->
            eloop $ toggleSheet (fromIntegral $ div (mx - 240) 100) s
          | (mx, my) `insideRect` SDL.Rect 240 30 9999 30 ->
            toggleVolume (fromIntegral $ div (mx - 240) 100) s >>= eloop
#ifdef MACAPP
        -- This should get handled automatically by SDL,
        -- but for some reason it breaks in .app mode
        -- so we have to handle it manually.
        Just SDL.KeyboardEvent
          { SDL.eventType           = SDL.SDL_KEYDOWN
          , SDL.keyboardEventRepeat = 0
          , SDL.keyboardEventKeysym = SDL.Keysym
            { SDL.keysymScancode = SDL.SDL_SCANCODE_Q
            , SDL.keysymMod      = keymod
            }
          } | keymod `elem` [SDL.KMOD_LGUI, SDL.KMOD_RGUI]
          -> quit
#endif
        Just _  -> eloop s
        Nothing -> loop  s
        where moveLeft = do
                s' <- whileStopped s $ \ss -> do
                  let newPosn = max 0 $ audioPosn ss - 5
                  seekTo newPosn (playSpeed ss) pipe
                  return $ ss { audioPosn = newPosn }
                eloop s'
              moveRight = do
                s' <- whileStopped s $ \ss -> do
                  let newPosn = audioPosn ss + 5
                  seekTo newPosn (playSpeed ss) pipe
                  return $ ss { audioPosn = newPosn }
                eloop s'
              toggleSpeed = do
                s' <- whileStopped s $ \ss -> do
                  let newSpeed = if playSpeed ss == 1 then 1.25 else 1
                  seekTo (audioPosn ss) newSpeed pipe
                  return $ ss { playSpeed = newSpeed }
                eloop s'
              togglePlaying = case s of
                Playing ps -> stop  ps >>= eloop . Stopped
                Stopped ss -> start ss >>= eloop . Playing
              quit = case s of
                Playing ps -> stop ps >> return ()
                Stopped _  -> return ()

      in loop $ Stopped StopState
        { audioPosn  = 0
        , playSpeed  = 1
        , audioGains = map (const 1) labels
        , sheetShow  = zipWith const (True : repeat False) sheets
        }

insideRect :: (Integral a) => (a, a) -> SDL.Rect -> Bool
(x, y) `insideRect` (SDL.Rect rx ry w h) = and
  [ rx <= xi
  , xi < rx + w
  , ry <= yi
  , yi < ry + h
  ] where xi = fromIntegral x
          yi = fromIntegral y

data StopState = StopState
  { audioPosn  :: Double -- seconds
  , playSpeed  :: Double -- ratio of playback secs to original secs
  , audioGains :: [Double]
  , sheetShow  :: [Bool]
  } deriving (Eq, Ord, Show, Read)

data PlayState = PlayState
  { startTicks  :: Int -- sdl ticks
  , stopState   :: StopState
  } deriving (Eq)

data State
  = Stopped StopState
  | Playing PlayState
  deriving (Eq)

getStopState :: State -> StopState
getStopState (Playing PlayState{ stopState = ss }) = ss
getStopState (Stopped ss) = ss

setStopState :: StopState -> State -> State
setStopState ss = \case
  Playing ps -> Playing ps{ stopState = ss }
  Stopped _  -> Stopped ss

mapStopState :: (StopState -> StopState) -> State -> State
mapStopState f s = setStopState (f $ getStopState s) s

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
renderVertSeq :: SDL.Renderer -> [(SDL.Texture, SDL.Rect)] -> (CInt, CInt) -> IO ()
renderVertSeq _    []              _      = return ()
renderVertSeq rend ((t, r) : rest) (x, y) = do
  h <- alloca $ \ph -> do
    zero $ SDL.getRendererOutputSize rend nullPtr ph
    peek ph
  when (y < h) $ do
    with r $ \p0 ->
      with (SDL.Rect x y (SDL.rectW r) (SDL.rectH r)) $ \p1 ->
        zero $ SDL.renderCopy rend t p0 p1
    renderVertSeq rend rest (x, y + SDL.rectH r)

-- | Renders a sequence of images at 1:1 aspect ratio in a horizontal sequence.
-- Stops rendering images once they go past the window's right edge.
renderHorizSeq :: SDL.Renderer -> [(SDL.Texture, SDL.Rect)] -> (CInt, CInt) -> IO ()
renderHorizSeq _    []              _      = return ()
renderHorizSeq rend ((t, r) : rest) (x, y) = do
  w <- alloca $ \pw -> do
    zero $ SDL.getRendererOutputSize rend pw nullPtr
    peek pw
  when (x < w) $ do
    with r $ \p0 ->
      with (SDL.Rect x y (SDL.rectW r) (SDL.rectH r)) $ \p1 ->
        zero $ SDL.renderCopy rend t p0 p1
    renderHorizSeq rend rest (x + SDL.rectW r, y)

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

rowStarts :: [Beat] -> [Double]
rowStarts = takeOdds . map position . filter isDownbeat where
  takeOdds (_ : x : xs) = x : takeOdds xs
  takeOdds _            = []

-- | Given the contents of beats.plist, translates a position in seconds
-- to the sheet music row number we're on (starting from 0).
rowNumber :: [Beat] -> Double -> (Int, Double)
rowNumber bts pos = case span (< pos) $ rowStarts bts of
  ([], _ ) -> (0, 0)
  (xs, []) -> (length xs - 1, 0)
  (xs, ys) -> let
    rowStart = last xs
    rowEnd   = head ys
    in (length xs - 1, (pos - rowStart) / (rowEnd - rowStart))

-- | Clever syntax trick: turn function call arguments into \"bulleted lists\".
-- See definitions of "withWindowAndRenderer", "withALContext", etc.
(—) :: (a -> b) -> a -> b
(—) = ($)
infixl 0 —
