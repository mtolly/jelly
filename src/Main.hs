{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Jelly.Arrangement
import           Jelly.AudioPipe
import           Jelly.Jammit
import           Jelly.Prelude
import           Jelly.SDL

import           Control.Concurrent         (threadDelay)
import qualified Control.Lens as L
import           Control.Lens.Operators     ((.=), (%=), (+=))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.State  (StateT, evalStateT, get)
import           Data.Char                  (toLower)
import           Data.List                  (elemIndex, intercalate, nub,
                                             transpose)
import           Data.Maybe                 (fromJust, isJust)
import           Foreign                    (Word32, alloca, peek)
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Image      as Image
import           System.Environment         (getArgs, getProgName)
import           System.FilePath            ((<.>), (</>))

#ifndef LOCALRESOURCES
import           Paths_jelly                (getDataFileName)
#else
import           System.Environment.FindBin (getProgPath)

getDataFileName :: FilePath -> IO FilePath
getDataFileName fp = (</> fp) <$> getProgPath
#endif

-- | State that doesn't change once everything is loaded
data Static = Static
  { window_     :: SDL.Window
  , renderer_   :: SDL.Renderer
  , audioPipe_  :: AudioPipe
  , audioParts_ :: [Maybe Part]
  , sheetParts_ :: [(SheetPart, [Arrangement ()])]
  , beats_      :: [Beat]
  , getImage_   :: String -> SDL.Texture
  }

withLoad :: [FilePath] -> (Static -> IO a) -> IO a
withLoad songs act
  = withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
  $ withSDLImage [Image.InitPNG]
  $ withWindowAndRenderer "Jelly" sheetWidth 480 SDL.SDL_WINDOW_RESIZABLE
  $ \window rend -> do

  Just allInfo <- fmap sequence $ mapM loadInfo   songs
  Just allTrks <- fmap sequence $ mapM loadTracks songs
  Just bts     <- loadBeats $ head songs

  theTitle <- case nub $ map title allInfo of
    [x] -> pure x
    xs -> error $ "Different song titles: " ++ show xs
  _ <- case nub $ map artist allInfo of
    [x] -> pure x
    xs -> error $ "Different song artists: " ++ show xs
  let theInstruments = map instrument allInfo

  let fileTrks = do
        (song, inst, instTrks) <- zip3 songs theInstruments allTrks
        trk <- instTrks
        guard $ trackClass trk == "JMFileTrack"
        let Just apart = titleToAudioPart (fromJust $ trackTitle trk) inst
        pure (song, apart, trk)

  sheets <- fmap concat $ forM fileTrks $ \(song, apart, trk) -> case apart of
    Without _ -> pure []
    Only part -> do
      let findRows f = do
            pages <- f trk song
            Right texs <- sequence <$> mapM (Image.imgLoadTexture rend) pages
            pure $ splitRows' trk texs
      notes <- findRows findNotation
      tab   <- findRows findTab
      pure $ if null tab
        then [(Notation part, notes)]
        else [(Notation part, notes), (Tab part, tab)]
  audio <- forM fileTrks $ \(song, apart, trk) -> do
    Just aud <- findAudio trk song
    pure (apart, aud)

  let toggleImages = (++)
        — map (map toLower . drop 4 . show) ([minBound .. maxBound] :: [Part])
        — words "band guitar1tab guitar2tab basstab slow"
      simpleImages = words "play stop divider left right"
      allImages = simpleImages ++ toggleImages ++ map ("no_" ++) toggleImages
  images <- forM allImages $ \img -> do
    dataLoc <- getDataFileName $ "resources" </> img <.> "png"
    Image.imgLoadTexture rend dataLoc >>= \case
      Right tex -> pure (img, tex)
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
        pure f
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

data Command
  = ToggleSheet SheetPart
  | ToggleAudio (Maybe Part)
  | ToggleSlow
  | PlayPause
  | MoveLeft
  | MoveRight
  deriving (Eq, Ord, Show, Read)

data Volatile = Volatile
  { _stoppedPosn :: Double -- ^ seconds
  , _playSpeed   :: Double -- ^ ratio of playback secs to original secs
  , _audioGains  :: [Double]
  , _sheetShow   :: [Bool]
  , _startTicks  :: Maybe Word32 -- ^ sdl ticks, Nothing if stopped
  } deriving (Eq, Ord, Show, Read)

L.makeLenses ''Volatile
type App = StateT Volatile IO

isPlaying :: App Bool
isPlaying = fmap isJust $ L.use startTicks

currentPosn :: App Double
currentPosn = do
  sp <- L.use stoppedPosn
  L.use startTicks >>= \case
    Nothing -> pure sp
    Just tks -> do
      now <- SDL.getTicks
      let diffSeconds = fromIntegral (now - tks) / 1000
      pure $ sp + diffSeconds

main :: IO ()
main = do

  songs <- getArgs >>= \case
    []    -> getProgName >>= \pn -> error $ "Usage: "++pn++" dir1 [dir2 ...]"
    songs -> pure songs

  withLoad songs $ \static -> let
    rend = renderer_ static
    getImage = getImage_ static

    start :: App ()
    start = do
      liftIO $ playPause $ audioPipe_ static
      -- Mark the sdl ticks when we started audio
      SDL.getTicks >>= (startTicks .=) . Just

    stop :: App ()
    stop = do
      liftIO $ playPause $ audioPipe_ static
      currentPosn >>= (stoppedPosn .=)
      startTicks .= Nothing

    menu :: App (Arrangement Command)
    menu = get >>= \s -> pure $ row
      [ Label PlayPause $ Whole $ getImage $ case _startTicks s of
        Nothing -> "play"
        Just _  -> "stop"
      , Label MoveLeft $ Whole $ getImage "left"
      , Label MoveRight $ Whole $ getImage "right"
      , Label ToggleSlow $ Whole $ getImage $ case _playSpeed s of
        1 -> "no_slow"
        _ -> "slow"
      , column
        [ row $ do
          ((sheet, _), isShown) <- zip (sheetParts_ static) $ _sheetShow s
          let prefix = if isShown then "" else "no_"
              filename = case sheet of
                Notation p -> partToFile p
                Tab p -> partToFile p ++ "tab"
          pure $ Label (ToggleSheet sheet) $ Whole $ getImage $ prefix ++ filename
        , row $ do
          (apart, gain) <- zip (audioParts_ static) $ _audioGains s
          let prefix = if gain /= 0 then "" else "no_"
              filename = maybe "band" partToFile apart
          pure $ Label (ToggleAudio apart) $ Whole $ getImage $ prefix ++ filename
        ]
      ] where partToFile = map toLower . drop 4 . show

    draw :: App ()
    draw = do
      pn <- currentPosn
      s <- get
      let (rowN, frac) = rowNumber (beats_ static) pn
          sheetsToDraw = map snd $ filter fst
            $ zip (_sheetShow s) $ sheetParts_ static
          sheetStream :: Arrangement ()
          sheetStream = column $ case sheetsToDraw of
            [sheet] -> drop rowN $ snd sheet
            _ -> intercalate [Whole $ getImage "divider"] $
              transpose $ map (drop rowN . snd) sheetsToDraw
      thisMenu <- menu
      liftIO $ do
        systemHeight <- fmap sum $ forM sheetsToDraw $ \sheet ->
          fmap snd $ getDims $ head $ snd sheet
        zero $ SDL.setRenderDrawColor rend 0 0 0 255
        zero $ SDL.renderClear rend
        render rend (0, 0) $ column
          [ thisMenu
          , layers
            [ fmap undefined sheetStream
            , row
              [ space (floor $ frac * sheetWidth, 0)
              , Rectangle (2, systemHeight) (SDL.Color 255 0 0 255)
              ]
            ]
          ]
        SDL.renderPresent rend

    whileStopped :: App () -> App ()
    whileStopped f = isPlaying >>= \case
      True -> stop >> f >> liftIO (threadDelay 75000) >> start
      False -> f

    toggleVolume :: Int -> App ()
    toggleVolume i = do
      audioGains . L.ix i %= \case { 1 -> 0; _ -> 1 }
      gains' <- L.use audioGains
      liftIO $ setVolumes gains' $ audioPipe_ static

    toggleSheet :: Int -> App ()
    toggleSheet i = sheetShow . L.ix i %= not

    loop :: App ()
    loop = draw >> liftIO (threadDelay 16000) >> eloop

    updateSeek :: App ()
    updateSeek = do
      p <- L.use stoppedPosn
      s <- L.use playSpeed
      liftIO $ seekTo p s $ audioPipe_ static

    runCommand :: Command -> App ()
    runCommand cmd = case cmd of
      ToggleSheet sp -> let
        Just i = elemIndex sp $ map fst $ sheetParts_ static
        in toggleSheet i
      ToggleAudio aud -> let
        Just i = elemIndex aud $ audioParts_ static
        in toggleVolume i
      ToggleSlow -> whileStopped $ do
        playSpeed %= \case { 1 -> 1.25; _ -> 1 }
        updateSeek
      PlayPause -> isPlaying >>= \b -> if b then stop else start
      MoveLeft -> whileStopped $ do
        stoppedPosn %= \pn -> max 0 $ pn - 5
        updateSeek
      MoveRight -> whileStopped $ do
        stoppedPosn += 5
        updateSeek

    eloop :: App ()
    eloop = liftIO pollEvent >>= \case
      Just (SDL.QuitEvent {}) -> isPlaying >>= \b -> when b stop
      Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
        -- Let user adjust height, but make width to at least sheetWidth
        (width, height) <- liftIO $ alloca $ \pw -> alloca $ \ph -> do
          SDL.getWindowSize (window_ static) pw ph
          liftA2 (,) (peek pw) (peek ph)
        SDL.setWindowSize (window_ static) (max sheetWidth width) height
        eloop
      Just (KeyPress SDL.SDL_SCANCODE_SPACE) -> runCommand PlayPause >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_LEFT) -> runCommand MoveLeft >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_RIGHT) -> runCommand MoveRight >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_LSHIFT) -> runCommand ToggleSlow >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_Z) -> toggleVolume 0 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_X) -> toggleVolume 1 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_C) -> toggleVolume 2 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_V) -> toggleVolume 3 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_B) -> toggleVolume 4 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_N) -> toggleVolume 5 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_M) -> toggleVolume 6 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_A) -> toggleSheet 0 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_S) -> toggleSheet 1 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_D) -> toggleSheet 2 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_F) -> toggleSheet 3 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_G) -> toggleSheet 4 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_H) -> toggleSheet 5 >> eloop
      Just (KeyPress SDL.SDL_SCANCODE_J) -> toggleSheet 6 >> eloop
      Just (SDL.MouseButtonEvent
        { SDL.eventType = SDL.SDL_MOUSEBUTTONDOWN
        , SDL.mouseButtonEventButton = SDL.SDL_BUTTON_LEFT
        , SDL.mouseButtonEventX = mx
        , SDL.mouseButtonEventY = my
        }) -> menu >>= liftIO . findLabel (fromIntegral mx, fromIntegral my) >>= \case
          Just (act, _) -> runCommand act >> eloop
          Nothing       -> eloop
      Just _  -> eloop
      Nothing -> loop

    initialVolatile = Volatile
      { _stoppedPosn = 0
      , _playSpeed   = 1
      , _audioGains  = map (const 1) $ audioParts_ static
      , _sheetShow   = zipWith const (True : repeat False) $ sheetParts_ static
      , _startTicks  = Nothing
      }

    in evalStateT loop initialVolatile
