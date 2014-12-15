{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP             #-}
module Main (main) where

import           Control.Applicative     ((<$>), liftA2)
import           Control.Concurrent      (threadDelay)
import           Control.Exception       (bracket, bracket_)
import           Control.Monad           (forM, guard)
import           Data.Char               (toLower)
import           Data.List               (elemIndex, intercalate, nub, transpose)
import           Data.Maybe              (fromJust)
import           Foreign                 (Word32, alloca, peek, (.|.))
import           Foreign.C               (CInt, withCString)
import qualified Graphics.UI.SDL         as SDL
import qualified Graphics.UI.SDL.Image   as Image
import           Sound.OpenAL            (($=))
import qualified Sound.OpenAL            as AL
import           System.FilePath         ((<.>), (</>))

import           Jammit
import           AudioPipe
import           TTF
import           Util
import           Arrangement

import System.Environment (getArgs, getProgName)
#ifndef LOCALRESOURCES
import Paths_jelly (getDataFileName)
#else
import System.Environment.FindBin (getProgPath)

getDataFileName :: FilePath -> IO FilePath
getDataFileName fp = do
  bin <- getProgPath
  return $ bin </> fp
#endif

-- | State that doesn't change once everything is loaded
data Static = Static
  { window_ :: SDL.Window
  , renderer_ :: SDL.Renderer
  , audioPipe_ :: AudioPipe
  , audioParts_ :: [Maybe Part]
  , sheetParts_ :: [(SheetPart, [Arrangement ()])]
  , beats_ :: [Beat]
  , getImage_ :: String -> SDL.Texture
  }

withLoad :: [FilePath] -> (Static -> IO a) -> IO a
withLoad songs act = withALContext
  $ withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
  $ withSDLImage [Image.InitPNG]
  $ withTTF
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
            return $ splitRows' trk texs
      notes <- findRows findNotation
      tab   <- findRows findTab
      return $ if null tab
        then [(Notation part, notes)]
        else [(Notation part, notes), (Tab part, tab)]
  audio <- forM fileTrks $ \(song, apart, trk) -> do
    Just aud <- findAudio trk song
    return (apart, aud)

  let toggleImages = (++)
        — map (map toLower . drop 4 . show) ([minBound .. maxBound] :: [Part])
        — words "band guitar1tab guitar2tab basstab slow"
      simpleImages = words "play stop divider left right"
      allImages = simpleImages ++ toggleImages ++ map ("no_" ++) toggleImages
  images <- forM allImages $ \img -> do
    dataLoc <- getDataFileName $ "resources" </> img <.> "png"
    Image.imgLoadTexture rend dataLoc >>= \case
      Right tex -> return (img, tex)
      Left err -> error $ "Error in loading image (" ++ dataLoc ++ "): " ++ show err
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
      labels = map (Just . fst) normalAudio ++ [Nothing]
      inputs :: [([FilePath], [FilePath])]
      inputs = [ ([f], []) | (_, f) <- normalAudio ] ++ [([snd bestBack], normalNegative)]

  withPipe 2 1 inputs $ \pipe ->
    act $ Static
      { window_ = window
      , renderer_ = rend
      , audioPipe_ = pipe
      , audioParts_ = labels
      , sheetParts_ = sheets
      , beats_ = bts
      , getImage_ = getImage
      }

data Command
  = ToggleSheet SheetPart
  | ToggleAudio (Maybe Part)
  | ToggleSlow
  | PlayPause
  | MoveLeft
  | MoveRight
  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do

  songs <- getArgs >>= \case
    []    -> getProgName >>= \pn -> error $ "Usage: "++pn++" dir1 [dir2 ...]"
    songs -> return songs

  withLoad songs $ \static -> let
    rend = renderer_ static
    getImage = getImage_ static

    updatePosition ps = do
      let ss = stopState ps
      tend <- fromIntegral <$> SDL.getTicks
      let diffSeconds = fromIntegral (tend - startTicks ps) / 1000
      return $ audioPosn ss + diffSeconds / playSpeed ss

    start ss = do
      playPause $ audioPipe_ static
      -- Mark the sdl ticks when we started audio
      starttks <- fromIntegral <$> SDL.getTicks
      return $ PlayState
        { startTicks  = starttks
        , stopState   = ss
        }

    stop ps = do
      playPause $ audioPipe_ static
      newpn <- updatePosition ps
      return $ (stopState ps) { audioPosn = newpn }

    menu s = row
      [ Label PlayPause $ Whole $ getImage $ case s of
        Stopped _ -> "play"
        Playing _ -> "stop"
      , Label MoveLeft $ Whole $ getImage "left"
      , Label MoveRight $ Whole $ getImage "right"
      , Label ToggleSlow $ Whole $ getImage $ case playSpeed $ getStopState s of
        1 -> "no_slow"
        _ -> "slow"
      , column
        [ row $ do
          ((sheet, _), isShown) <- zip (sheetParts_ static) $ sheetShow $ getStopState s
          let prefix = if isShown then "" else "no_"
              filename = case sheet of
                Notation p -> partToFile p
                Tab p -> partToFile p ++ "tab"
          return $ Label (ToggleSheet sheet) $ Whole $ getImage $ prefix ++ filename
        , row $ do
          (apart, gain) <- zip (audioParts_ static) $ audioGains $ getStopState s
          let prefix = if gain /= 0 then "" else "no_"
              filename = maybe "band" partToFile apart
          return $ Label (ToggleAudio apart) $ Whole $ getImage $ prefix ++ filename
        ]
      ] where partToFile = map toLower . drop 4 . show

    draw s = do
      pn <- case s of
        Stopped StopState{audioPosn = pn} -> return pn
        Playing playstate -> updatePosition playstate
      let (rowN, frac) = rowNumber (beats_ static) pn
          sheetsToDraw = map snd $ filter fst
            $ zip (sheetShow $ getStopState s) $ sheetParts_ static
          sheetStream :: Arrangement ()
          sheetStream = column $ case sheetsToDraw of
            [sheet] -> drop rowN $ snd sheet
            _ -> intercalate [Whole $ getImage "divider"] $
              transpose $ map (drop rowN . snd) sheetsToDraw
          thisMenu = menu s
      systemHeight <- fmap sum $ forM sheetsToDraw $ \sheet ->
        fmap snd $ getDims $ head $ snd sheet
      zero $ SDL.setRenderDrawColor rend 0 0 0 255
      zero $ SDL.renderClear rend
      render rend (0, 0) $ column
        [ thisMenu
        , layers
          [ fmap undefined sheetStream
          , row
            [ Space (floor $ frac * sheetWidth, 0)
            , Rectangle (2, systemHeight) (SDL.Color 255 0 0 255)
            ]
          ]
        ]
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
        setVolumes gains' $ audioPipe_ static
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
    runCommand :: Command -> State -> IO State
    runCommand cmd s = case cmd of
      ToggleSheet sp -> let
        Just i = elemIndex sp $ map fst $ sheetParts_ static
        in return $ toggleSheet i s
      ToggleAudio aud -> let
        Just i = elemIndex aud $ audioParts_ static
        in toggleVolume i s
      ToggleSlow -> whileStopped s $ \ss -> do
        let newSpeed = if playSpeed ss == 1 then 1.25 else 1
        seekTo (audioPosn ss) newSpeed $ audioPipe_ static
        return $ ss { playSpeed = newSpeed }
      PlayPause -> case s of
        Playing ps -> stop  ps >>= return . Stopped
        Stopped ss -> start ss >>= return . Playing
      MoveLeft -> whileStopped s $ \ss -> do
        let newPosn = max 0 $ audioPosn ss - 5
        seekTo newPosn (playSpeed ss) $ audioPipe_ static
        return $ ss { audioPosn = newPosn }
      MoveRight -> whileStopped s $ \ss -> do
        let newPosn = audioPosn ss + 5
        seekTo newPosn (playSpeed ss) $ audioPipe_ static
        return $ ss { audioPosn = newPosn }
    eloop s = pollEvent >>= \case
      Just (SDL.QuitEvent {}) -> quit
      Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
        -- Let user adjust height, but make width to at least sheetWidth
        (width, height) <- alloca $ \pw -> alloca $ \ph -> do
          SDL.getWindowSize (window_ static) pw ph
          liftA2 (,) (peek pw) (peek ph)
        SDL.setWindowSize (window_ static) (max sheetWidth width) height
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
        }) -> findLabel (fromIntegral mx, fromIntegral my) (menu s) >>= \case
          Just (act, _) -> runCommand act s >>= eloop
          Nothing       -> eloop s
      Just _  -> eloop s
      Nothing -> loop  s
      where moveLeft = runCommand MoveLeft s >>= eloop
            moveRight = runCommand MoveRight s >>= eloop
            toggleSpeed = runCommand ToggleSlow s >>= eloop
            togglePlaying = runCommand PlayPause s >>= eloop
            quit = case s of
              Playing ps -> stop ps >> return ()
              Stopped _  -> return ()

    in loop $ Stopped StopState
      { audioPosn  = 0
      , playSpeed  = 1
      , audioGains = map (const 1) $ audioParts_ static
      , sheetShow  = zipWith const (True : repeat False) $ sheetParts_ static
      }

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

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> Just <$> peek pevt
  _ -> return Nothing

splitRows' :: Track -> [SDL.Texture] -> [Arrangement a]
splitRows' trk texs = map f $ splitRows trk texs where
  f snips = column [ Crop rect tex | (tex, rect) <- snips ]

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
