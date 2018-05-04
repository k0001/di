{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1.Render
 ( renderLog
 , renderLogColor
 , removeAnsiEscapes
 ) where

import Control.Applicative ((<|>), many)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BBP
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)
import Data.Functor (($>))
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word8, Word32)
import Prelude hiding (log, filter, error)
import qualified Prelude

import Di.Types
 (Log(Log), Message, unMessage,
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root), Segment, unSegment, Key, unKey, Value, unValue)

--------------------------------------------------------------------------------

-- This is rather ugly, but whatever.
renderLogColor :: Log -> BB.Builder
renderLogColor = \(Log syst lvl path msg) ->
 let t = renderIso8601 syst <> space
     pDef = \fg -> renderPathColor fg fgBlue fgCyan path
     pRed = renderPathColor fgBlack fgWhite fgWhite path
     m = space <> renderMessage msg <> reset
 in case lvl of
     Debug -> reset <> t <> pDef fgDefault <> fgDefault <> debug <> m
     Info -> reset <> t <> pDef fgDefault <> fgDefault <> info <> m
     Notice ->
       bgDefault <> fgGreen <> t <> pDef fgDefault <> fgGreen <> notice <> m
     Warning ->
       bgDefault <> fgYellow <> t <> pDef fgDefault <> fgYellow <> warning <> m
     Error ->
       bgDefault <> fgRed <> t <> pDef fgDefault <> fgRed <> error <> m
     Critical ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> critical <> fgBlack <> m
     Alert ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> alert <> fgBlack <> m
     Emergency ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> emergency <> fgBlack <> m

-- | Like 'renderLogColor', but without color.
renderLog :: Log -> BB.Builder
renderLog = \(Log syst lvl path msg) ->
  renderIso8601 syst <> space <> renderPath path <>
  level lvl <> space <> renderMessage msg

-- | @'renderPathColor' a b c p@ renders @p@ using @a@ as the default color (for
-- things like whitespace or attribute values), @b@ as the color for path names,
-- and @c@ as the color for attribute keys. This adds a trailing whitespace if
-- necessary.
renderPathColor :: BB.Builder -> BB.Builder -> BB.Builder -> Path -> BB.Builder
{-# INLINE renderPathColor #-}
renderPathColor defc pathc keyc = fix $ \f -> \case
  Attr k v p ->
    f p <> defc <> keyc <> renderKey k <>
    defc <> equals <> renderValue v <> space
  Push s p -> f p <> defc <> pathc <> slash <> renderSegment s <> space
  Root -> mempty

-- | Like 'renderPathColor', but without color.
renderPath :: Path -> BB.Builder
{-# INLINE renderPath #-}
renderPath = fix $ \f -> \case
  Attr k v p -> f p <>renderKey k <> equals <> renderValue v <> space
  Push s p -> f p <> slash <> renderSegment s <> space
  Root -> mempty

renderMessage :: Message -> BB.Builder
{-# INLINE renderMessage #-}
renderMessage = escapeMessage . unMessage

escapeMessage :: TL.Text -> BB.Builder
{-# INLINE escapeMessage #-}
escapeMessage = TL.encodeUtf8BuilderEscaped
  $ BBP.condB (== 37) word8HexPercent  -- '%'
  $ BBP.condB (<= 31) word8HexPercent  -- control characters
  $ BBP.liftFixedToBounded BBP.word8

renderSegment :: Segment -> BB.Builder
{-# INLINE renderSegment #-}
renderSegment = escapeMetaL . TL.fromStrict . unSegment

renderKey :: Key -> BB.Builder
{-# INLINE renderKey #-}
renderKey = escapeMetaL . TL.fromStrict . unKey

renderValue :: Value -> BB.Builder
{-# INLINE renderValue #-}
renderValue = escapeMetaL . unValue

-- | Escape metadata such as path segments, attribute keys or attribute values.
escapeMetaL :: TL.Text -> BB.Builder
{-# INLINE escapeMetaL #-}
escapeMetaL = TL.encodeUtf8BuilderEscaped
   $ BBP.condB
        (\w -> (w <= 47)               -- Control and separator-like
            || (w >= 58 && w <= 64)    -- Delimiter-like
            || (w >= 91 && w <= 96)    -- Delimiter-like
            || (w >= 123 && w <= 127)) -- Delimiter-like and DEL
        word8HexPercent
   $ BBP.liftFixedToBounded BBP.word8

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

-- | Render @'%'@ followed by the given 'Word8' rendered as two hexadecimal
-- nibbles.
word8HexPercent :: BBP.BoundedPrim Word8
word8HexPercent = BBP.liftFixedToBounded
  ((\x -> (37, x)) BBP.>$< BBP.word8 BBP.>*< BBP.word8HexFixed)
{-# INLINE word8HexPercent #-}

--------------------------------------------------------------------------------

-- | Renders /YYYY-MM-DDThh:mm:ss.sssssssssZ/ (nanosecond precision).
--
-- The rendered string is a 30 characters long, and it's ASCII-encoded.
renderIso8601 :: Time.SystemTime -> BB.Builder
{-# INLINE renderIso8601 #-}
renderIso8601 = \syst ->
  let Time.UTCTime tday tdaytime = Time.systemToUTCTime syst
      (year, month, day) = Time.toGregorian tday
      Time.TimeOfDay hour min' sec = Time.timeToTimeOfDay tdaytime
  in -- Notice that 'TB.decimal' RULES dispatch to faster code for smaller
     -- types (e.g., 'Word8' is faster to render than 'Int'), so we make
     -- seemingly redundant 'fromIntegral' conversions here to that effect.
     BB.int16Dec (fromIntegral year) <> "-" <>
     word8Dec_pad10 (fromIntegral month) <> "-" <>
     word8Dec_pad10 (fromIntegral day) <> "T" <>
     word8Dec_pad10 (fromIntegral hour) <> ":" <>
     word8Dec_pad10 (fromIntegral min') <> ":" <>
     word8Dec_pad10 (truncate sec) <> "." <>
     word32Dec_pad100000000 (Time.systemNanoseconds syst) <> "Z"

word8Dec_pad10 :: Word8 -> BB.Builder
{-# INLINE word8Dec_pad10 #-}
word8Dec_pad10 x =
  let !y = BB.word8Dec x
  in if x < 10 then (_zero1 <> y) else y

word32Dec_pad100000000 :: Word32 -> BB.Builder
{-# INLINE word32Dec_pad100000000 #-}
word32Dec_pad100000000 x =
  let !y = BB.word32Dec x
  in if | x < 10 -> _zero8 <> y
        | x < 100 -> _zero7 <> y
        | x < 1000 -> _zero6 <> y
        | x < 10000 -> _zero5 <> y
        | x < 100000 -> _zero4 <> y
        | x < 1000000 -> _zero3 <> y
        | x < 10000000 -> _zero2 <> y
        | x < 100000000 -> _zero1 <> y
        | otherwise -> y

_zero1, _zero2, _zero3, _zero4, _zero5, _zero6, _zero7, _zero8 :: BB.Builder
_zero1 = BB.string7 "0"
_zero2 = BB.string7 "00"
_zero3 = BB.string7 "000"
_zero4 = BB.string7 "0000"
_zero5 = BB.string7 "00000"
_zero6 = BB.string7 "000000"
_zero7 = BB.string7 "0000000"
_zero8 = BB.string7 "00000000"

--------------------------------------------------------------------------------

-- | Remove ANSI escapes. This is not complete, but it covers the ANSI codes we
-- use. In particular:
--
-- @
-- forall x.
--    'BB.toByteString' . 'renderLog'
--        == 'removeAnsiEscapes' . 'BB.toByteString' . 'renderLogColor'
-- @
removeAnsiEscapes :: BL.ByteString -> BL.ByteString
removeAnsiEscapes b0 = do
    case ABL.eitherResult (ABL.parse p b0) of
       Right b1 -> b1
       Left e -> Prelude.error ("removeAnsiEscapes: unexpected " ++ e)
  where
    p :: ABL.Parser BL.ByteString
    p = fmap BL.fromChunks $ many $
          (ABL.takeWhile1 (/= 27)) <|>
          (pAnsiEscape $> "") <|>
          (ABL.satisfy (== 27) $> "")
    pAnsiEscape :: ABL.Parser ()
    pAnsiEscape = (ABL.<?> "pAnsiEscape") $ do
      _ <- ABL.satisfy (== 27)  -- '\ESC'
      _ <- ABL.satisfy (== 91)  -- '['
      _ <- ABL.takeWhile (\w -> w == 59 || (w >= 48 && w <= 57)) -- ';' '0'-'9'
      _ <- ABL.satisfy (== 109) -- 'm'
      pure ()

