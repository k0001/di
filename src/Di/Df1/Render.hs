{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1.Render
 ( df1
 ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BBP
import Data.Function (fix)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word (Word8)
import Prelude hiding (log, filter, error)

import Di.Misc (renderIso8601)
import Di.Types
 (Log(Log),
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root),
  LogLineRenderer(LogLineRendererUtf8))

--------------------------------------------------------------------------------

-- | Render in the __df1__ format.
df1 :: LogLineRenderer
df1 = LogLineRendererUtf8 $ \case
  True -> renderLogColor
  False -> renderLog
{-# INLINE df1 #-}

-- This is rather ugly, but whatever.
renderLogColor :: Log -> BB.Builder
{-# INLINE renderLogColor #-}
renderLogColor = \(Log syst lvl path msg) ->
  let t = renderIso8601 syst <> space
      pDef = \fg -> renderPathColor fg fgBlue fgCyan path <> space
      pRed = renderPathColor fgBlack fgWhite fgWhite path <> space
      m = space <> escapeMessage msg <> reset
  in case lvl of
      Debug -> reset <> t <> pDef fgDefault <> fgDefault <> debug <> m
      Info -> reset <> t <> pDef fgDefault <> fgDefault <> info <> m
      Notice -> bgDefault <> fgGreen <> t <> pDef fgDefault <> fgGreen <> notice <> m
      Warning -> bgDefault <> fgYellow <> t <> pDef fgDefault <> fgYellow <> warning <> m
      Error -> bgDefault <> fgRed <> t <> pDef fgDefault <> fgRed <> error <> m
      Critical -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> critical <> fgBlack <> m
      Alert -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> alert <> fgBlack <> m
      Emergency -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> emergency <> fgBlack <> m

-- | Like 'renderLogColor', but without color.
renderLog :: Log -> BB.Builder
{-# INLINE renderLog #-}
renderLog = \(Log syst lvl path msg) ->
  renderIso8601 syst <> space <> renderPath path <> space <>
  level lvl <> space <> escapeMessage msg

-- | @'renderPathColor' a b c p@ renders @p@ using @a@ as the default color (for
-- things like whitespace or attribute values), @b@ as the color for path names,
-- and @c@ as the color for attribute keys.

-- This is rather ugly, but whatever.
renderPathColor :: BB.Builder -> BB.Builder -> BB.Builder -> Path -> BB.Builder
{-# INLINE renderPathColor #-}
renderPathColor defc pathc keyc = fix $ \f -> \case
  Attr k v p ->
    f p <> defc <> space <> keyc <> escapeMeta k <>
    defc <> equals <> escapeMeta v
  Push x p -> f p <> defc <> space <> pathc <> slash <> escapeMeta x
  Root x -> pathc <> slash <> escapeMeta x

-- | Like 'renderPathColor', but without color.
renderPath :: Path -> BB.Builder
{-# INLINE renderPath #-}
renderPath = fix $ \f -> \case
  Attr k v p -> f p <> space <> escapeMeta k <> equals <> escapeMeta v
  Push x p -> f p <> space <> slash <> escapeMeta x
  Root x -> slash <> escapeMeta x

escapeMessage :: TL.Text -> BB.Builder
{-# INLINE escapeMessage #-}
escapeMessage = TL.encodeUtf8BuilderEscaped $
   BBP.condB (== 10) (f2b (percent2word8Fixed (48, 97))) $  -- '\n' ->  "%0a"
   BBP.condB (== 13) (f2b (percent2word8Fixed (48,100))) $  -- '\r' ->  "%0d"
   BBP.condB (== 37) (f2b (percent2word8Fixed (50, 53))) $  -- '%'  ->  "%25"
   f2b BBP.word8

-- | Escape metadata such as path names, attribute keys or attribute values.
escapeMeta :: TL.Text -> BB.Builder
{-# INLINE escapeMeta #-}
escapeMeta = TL.encodeUtf8BuilderEscaped $
   BBP.condB (== 10) (f2b (percent2word8Fixed (48, 97))) $  -- '\n' ->  "%0a"
   BBP.condB (== 13) (f2b (percent2word8Fixed (48,100))) $  -- '\r' ->  "%0d"
   BBP.condB (== 32) (f2b (percent2word8Fixed (50, 48))) $  -- ' '  ->  "%20"
   BBP.condB (== 33) (f2b (percent2word8Fixed (50, 49))) $  -- '!'  ->  "%21"
   BBP.condB (== 34) (f2b (percent2word8Fixed (50, 50))) $  -- '"'  ->  "%22"
   BBP.condB (== 35) (f2b (percent2word8Fixed (50, 51))) $  -- '#'  ->  "%23"
   BBP.condB (== 36) (f2b (percent2word8Fixed (50, 52))) $  -- '$'  ->  "%24"
   BBP.condB (== 37) (f2b (percent2word8Fixed (50, 53))) $  -- '%'  ->  "%25"
   BBP.condB (== 38) (f2b (percent2word8Fixed (50, 54))) $  -- '&'  ->  "%26"
   BBP.condB (== 39) (f2b (percent2word8Fixed (50, 55))) $  -- '\'' ->  "%27"
   BBP.condB (== 40) (f2b (percent2word8Fixed (50, 56))) $  -- '('  ->  "%28"
   BBP.condB (== 41) (f2b (percent2word8Fixed (50, 57))) $  -- ')'  ->  "%29"
   BBP.condB (== 42) (f2b (percent2word8Fixed (50, 97))) $  -- '*'  ->  "%2a"
   BBP.condB (== 43) (f2b (percent2word8Fixed (50, 98))) $  -- '+'  ->  "%2b"
   BBP.condB (== 44) (f2b (percent2word8Fixed (50, 99))) $  -- ','  ->  "%2c"
   BBP.condB (== 47) (f2b (percent2word8Fixed (50,102))) $  -- '/'  ->  "%2f"
   BBP.condB (== 58) (f2b (percent2word8Fixed (51, 97))) $  -- ':'  ->  "%3a"
   BBP.condB (== 59) (f2b (percent2word8Fixed (51, 98))) $  -- ';'  ->  "%3b"
   BBP.condB (== 61) (f2b (percent2word8Fixed (51,100))) $  -- '='  ->  "%3d"
   BBP.condB (== 63) (f2b (percent2word8Fixed (51,102))) $  -- '?'  ->  "%3f"
   BBP.condB (== 64) (f2b (percent2word8Fixed (52, 48))) $  -- '@'  ->  "%40"
   BBP.condB (== 91) (f2b (percent2word8Fixed (53, 98))) $  -- '['  ->  "%5b"
   BBP.condB (== 92) (f2b (percent2word8Fixed (53, 99))) $  -- '\\' ->  "%5c"
   BBP.condB (== 93) (f2b (percent2word8Fixed (53,100))) $  -- ']'  ->  "%5d"
   f2b BBP.word8

--------------------------------------------------------------------------------
-- Some hardcoded stuff we use time and time again

debug :: BB.Builder
debug = BB.string7 "DEBUG"
{-# INLINE debug #-}

info :: BB.Builder
info = BB.string7 "INFO"
{-# INLINE info #-}

notice :: BB.Builder
notice = BB.string7 "NOTICE"
{-# INLINE notice #-}

warning :: BB.Builder
warning = BB.string7 "WARNING"
{-# INLINE warning #-}

error :: BB.Builder
error = BB.string7 "ERROR"
{-# INLINE error #-}

critical :: BB.Builder
critical = BB.string7 "CRITICAL"
{-# INLINE critical #-}

alert :: BB.Builder
alert = BB.string7 "ALERT"
{-# INLINE alert #-}

emergency :: BB.Builder
emergency = BB.string7 "EMERGENCY"
{-# INLINE emergency #-}

level :: Level -> BB.Builder
{-# INLINE level #-}
level = \case
  { Debug -> debug; Info -> info;
    Notice -> notice; Warning -> warning;
    Error -> error; Critical -> critical;
    Alert -> alert; Emergency -> emergency }

space :: BB.Builder
space = BB.char7 ' '
{-# INLINE space #-}

slash :: BB.Builder
slash = BB.char7 '/'
{-# INLINE slash #-}

equals :: BB.Builder
equals = BB.char7 '='
{-# INLINE equals #-}

--------------------------------------------------------------------------------
-- ANSI escape codes

-- | Reset all
reset :: BB.Builder
reset = BB.string7 "\x1b[0m"
{-# INLINE reset #-}

-- | Default foreground
fgDefault :: BB.Builder
fgDefault = BB.string7 "\x1b[39m"
{-# INLINE fgDefault #-}

-- | Reset background
bgDefault :: BB.Builder
bgDefault = BB.string7 "\x1b[49m"
{-# INLINE bgDefault #-}

-- | green foreground
fgGreen :: BB.Builder
fgGreen = BB.string7 "\x1b[32m"
{-# INLINE fgGreen #-}

-- | green foreground
fgRed :: BB.Builder
fgRed = BB.string7 "\x1b[31m"
{-# INLINE fgRed #-}

-- | Yellow foreground
fgYellow :: BB.Builder
fgYellow = BB.string7 "\x1b[33m"
{-# INLINE fgYellow #-}

-- | Cyan foreground
fgCyan :: BB.Builder
fgCyan = BB.string7 "\x1b[36m"
{-# INLINE fgCyan #-}

-- | Blue foreground
fgBlue :: BB.Builder
fgBlue = BB.string7 "\x1b[34m"
{-# INLINE fgBlue #-}

-- | Black foreground
fgBlack :: BB.Builder
fgBlack = BB.string7 "\x1b[30m"
{-# INLINE fgBlack #-}

-- | White foreground
fgWhite :: BB.Builder
fgWhite = BB.string7 "\x1b[37m"
{-# INLINE fgWhite #-}

-- | Red background
bgRed :: BB.Builder
bgRed = BB.string7 "\x1b[41m"
{-# INLINE bgRed #-}

-- | Render @'%'@ followed by the two given bytes
percent2word8Fixed :: (Word8, Word8) -> BBP.FixedPrim Word8
percent2word8Fixed x =
  const (37, x) BBP.>$< BBP.word8 BBP.>*< BBP.word8 BBP.>*< BBP.word8
{-# INLINE percent2word8Fixed #-}

-- | @'f2b' == 'BBP.liftFixedToBounded'@
f2b :: BBP.FixedPrim a -> BBP.BoundedPrim a
f2b = BBP.liftFixedToBounded
{-# INLINE f2b #-}



