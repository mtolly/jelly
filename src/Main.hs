{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
module Main (main) where

import           Jelly.Arrangement
import           Jelly.AudioPipe
import           Jelly.Jammit
import           Jelly.Prelude
import           Jelly.SDL
import           Jelly.State

import           Control.Concurrent         (threadDelay)
import qualified Control.Lens as L
import           Control.Lens.Operators     ((.=), (%=), (+=), (^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.RWS    (ask, asks, get)
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

withLoad :: [FilePath] -> (Static -> IO a) -> IO a
withLoad songs act
  = withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO]
  $ withSDLImage [Image.InitPNG]
  $ withWindowAndRenderer "Jelly" sheetWidth 480 SDL.SDL_WINDOW_RESIZABLE
  $ \wind rend -> do

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
      { _window = wind
      , _renderer = rend
      , _audioPipe = pipe
      , _audioParts = labels
      , _sheetParts = sheets
      , _beats = bts
      , _interface = \s -> case lookup s images of
          Just img -> img
          Nothing  -> error $ "interface: couldn't find image " ++ s
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

isPlaying :: App Bool
isPlaying = fmap isJust $ L.use startTicks

currentPosn :: App Double
currentPosn = do
  sp <- L.use stoppedPosn
  L.use startTicks >>= \case
    Nothing -> pure sp
    Just tks -> do
      now <- SDL.getTicks
      speed <- L.use playSpeed
      let diffSeconds = fromIntegral (now - tks) / 1000
      pure $ sp + diffSeconds / speed

menu :: App (Arrangement Command)
menu = do
  volatile <- get
  static <- ask
  pure $ row
    [ Label PlayPause $ Whole $ static ^. interface $ case volatile ^. startTicks of
      Nothing -> "play"
      Just _  -> "stop"
    , Label MoveLeft $ Whole $ static ^. interface $ "left"
    , Label MoveRight $ Whole $ static ^. interface $ "right"
    , Label ToggleSlow $ Whole $ static ^. interface $ case volatile ^. playSpeed of
      1 -> "no_slow"
      _ -> "slow"
    , column
      [ row $ do
        ((sheet, _), isShown) <- zip (static ^. sheetParts) $ volatile ^. sheetShow
        let prefix = if isShown then "" else "no_"
            filename = case sheet of
              Notation p -> partToFile p
              Tab p -> partToFile p ++ "tab"
        pure $ Label (ToggleSheet sheet) $ Whole $ static ^. interface $ prefix ++ filename
      , row $ do
        (apart, gain) <- zip (static ^. audioParts) $ volatile ^. audioGains
        let prefix = if gain /= 0 then "" else "no_"
            filename = maybe "band" partToFile apart
        pure $ Label (ToggleAudio apart) $ Whole $ static ^. interface $ prefix ++ filename
      ]
    ] where partToFile = map toLower . drop 4 . show

draw :: App ()
draw = do
  pn <- currentPosn
  sheetBools <- L.use sheetShow
  static <- ask
  let (rowN, frac) = rowNumber (static ^. beats) pn
      sheetsToDraw = map snd $ filter fst $ zip sheetBools $ static ^. sheetParts
      sheetStream :: Arrangement ()
      sheetStream = column $ case sheetsToDraw of
        [sheet] -> drop rowN $ snd sheet
        _ -> intercalate [Whole $ (static ^. interface) "divider"] $
          transpose $ map (drop rowN . snd) sheetsToDraw
  thisMenu <- menu
  rend <- asks (^. renderer)
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

start :: App ()
start = do
  asks (^. audioPipe) >>= liftIO . playPause
  -- Mark the sdl ticks when we started audio
  SDL.getTicks >>= (startTicks .=) . Just

stop :: App ()
stop = do
  asks (^. audioPipe) >>= liftIO . playPause
  currentPosn >>= (stoppedPosn .=)
  startTicks .= Nothing

whileStopped :: App () -> App ()
whileStopped f = isPlaying >>= \case
  True -> stop >> f >> liftIO (threadDelay 75000) >> start
  False -> f

toggleVolume :: Int -> App ()
toggleVolume i = do
  audioGains . L.ix i %= \case { 1 -> 0; _ -> 1 }
  gains' <- L.use audioGains
  asks (^. audioPipe) >>= liftIO . setVolumes gains'

toggleSheet :: Int -> App ()
toggleSheet i = sheetShow . L.ix i %= not

updateSeek :: App ()
updateSeek = do
  p <- L.use stoppedPosn
  s <- L.use playSpeed
  asks (^. audioPipe) >>= liftIO . seekTo p s

runCommand :: Command -> App ()
runCommand cmd = case cmd of
  ToggleSheet sp -> do
    parts <- asks (^. sheetParts)
    let Just i = elemIndex sp $ map fst parts
    toggleSheet i
  ToggleAudio aud -> do
    parts <- asks (^. audioParts)
    let Just i = elemIndex aud parts
    toggleVolume i
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

limitTicks :: (MonadIO m) => Word32 -> (m () -> m ()) -> m ()
limitTicks n f = do
  tstart <- SDL.getTicks
  f $ do
    tend <- SDL.getTicks
    let waitMilli = max 0 $ tend - tstart
    liftIO $ threadDelay $ fromIntegral waitMilli * 1000
    limitTicks n f

eloop :: App () -> App ()
eloop next = liftIO pollEvent >>= \case
  Just (SDL.QuitEvent {}) -> isPlaying >>= \b -> when b stop
  Just (SDL.WindowEvent { SDL.windowEventEvent = SDL.SDL_WINDOWEVENT_RESIZED }) -> do
    -- Let user adjust height, but make width to at least sheetWidth
    wind <- asks (^. window)
    (width, height) <- liftIO $ alloca $ \pw -> alloca $ \ph -> do
      SDL.getWindowSize wind pw ph
      liftA2 (,) (peek pw) (peek ph)
    SDL.setWindowSize wind (max sheetWidth width) height
    eloop next
  Just (KeyPress SDL.SDL_SCANCODE_SPACE) -> runCommand PlayPause >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_LEFT) -> runCommand MoveLeft >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_RIGHT) -> runCommand MoveRight >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_LSHIFT) -> runCommand ToggleSlow >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_Z) -> toggleVolume 0 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_X) -> toggleVolume 1 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_C) -> toggleVolume 2 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_V) -> toggleVolume 3 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_B) -> toggleVolume 4 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_N) -> toggleVolume 5 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_M) -> toggleVolume 6 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_A) -> toggleSheet 0 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_S) -> toggleSheet 1 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_D) -> toggleSheet 2 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_F) -> toggleSheet 3 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_G) -> toggleSheet 4 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_H) -> toggleSheet 5 >> eloop next
  Just (KeyPress SDL.SDL_SCANCODE_J) -> toggleSheet 6 >> eloop next
  Just (SDL.MouseButtonEvent
    { SDL.eventType = SDL.SDL_MOUSEBUTTONDOWN
    , SDL.mouseButtonEventButton = SDL.SDL_BUTTON_LEFT
    , SDL.mouseButtonEventX = mx
    , SDL.mouseButtonEventY = my
    }) -> menu >>= liftIO . findLabel (fromIntegral mx, fromIntegral my) >>= \case
      Just (act, _) -> runCommand act >> eloop next
      Nothing       -> eloop next
  Just _  -> eloop next
  Nothing -> next

main :: IO ()
main = do
  songs <- getArgs >>= \case
    []    -> getProgName >>= \pn -> error $ "Usage: "++pn++" dir1 [dir2 ...]"
    songs -> pure songs
  withLoad songs $ \static -> let
    loop = limitTicks 20 $ \next -> draw >> eloop next
    in evalApp loop static $ initialVolatile static
