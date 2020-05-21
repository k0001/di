{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Render
 ( log
 , logColor
 , key
 , message
 , iso8601
 , segment
 , value
 ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BBP
import Data.Function (fix)
import Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word8, Word32)
import Prelude hiding (log, filter, error)

import Df1.Types
 (Log(log_time, log_level, log_path, log_message),
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push),
  Segment, unSegment,
  Key, unKey,
  Value, unValue,
  Message, unMessage)

--------------------------------------------------------------------------------

-- | Like 'log', but with ASCII colors.
logColor :: Log -> BB.Builder
{-# INLINABLE logColor #-}
logColor = \log_ ->
 let t = iso8601 (log_time log_) <> space
     pDef = \fg -> renderPathColor fg fgBlue fgCyan (log_path log_)
     pRed = renderPathColor fgBlack fgWhite fgCyan (log_path log_)
     m = space <> message (log_message log_) <> reset
 in case log_level log_ of
     Debug -> reset <> t <> pDef fgDefault <> fgDefault <> debug <> m
     Info -> reset <> t <> pDef fgDefault <> fgDefault <> info <> m
     Notice ->
       reset <> t <> pDef fgDefault <> fgGreen <> notice <> fgDefault <> m
     Warning ->
       reset <> t <> pDef fgDefault <> fgYellow <> warning <> fgDefault <> m
     Error ->
       bgWhite <> fgBlack <> t <> pDef fgBlack <> fgRed <> error <> fgBlack <> m
     Critical ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> critical <> fgBlack <> m
     Alert ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> alert <> fgBlack <> m
     Emergency ->
       bgRed <> fgBlack <> t <> pRed <> fgWhite <> emergency <> fgBlack <> m

-- | Renders a 'Log' on its own line. Doesn't include a trailing newline character.
--
-- For example:
--
-- @
-- 2019-11-15T18:05:54.949470902Z NOTICE Welcome to my program!
-- 2019-11-15T18:05:54.949623731Z \/initialization NOTICE Starting web server
-- 2019-11-15T18:05:54.949630205Z \/initialization ALERT Disk is almost full!!!
-- 2019-11-15T18:05:54.949640299Z \/server port=80 INFO Listening for new clients
-- 2019-11-15T18:05:54.949652133Z \/server port=80 \/handler client-address=10.0.0.8 INFO Connection established
-- 2019-11-15T18:05:54.949664482Z \/server port=80 \/handler client-address=10.0.0.8 WARNING user error (Oops!)
-- @ 
log :: Log -> BB.Builder
{-# INLINABLE log #-}
log = \x ->
  iso8601 (log_time x) <> space <>
  renderPath (log_path x) <>
  level (log_level x) <> space <>
  message (log_message x)

-- | @'renderPathColor' a b c p@ renders @p@ using @a@ as the default color (for
-- things like whitespace or attribute values), @b@ as the color for path names,
-- and @c@ as the color for attribute keys. This adds a trailing whitespace if
-- necessary.
renderPathColor
  :: BB.Builder -> BB.Builder -> BB.Builder -> Seq.Seq Path -> BB.Builder
{-# INLINE renderPathColor #-}
renderPathColor defc pathc keyc = fix $ \f -> \case
  ps Seq.:|> Attr k v ->
    f ps <> defc <> keyc <> key k <>
    defc <> equals <> value v <> space
  ps Seq.:|> Push s -> f ps <> defc <> pathc <> slash <> segment s <> space
  Seq.Empty -> mempty

-- | Like 'renderPathColor', but without color.
renderPath :: Seq.Seq Path -> BB.Builder
{-# INLINE renderPath #-}
renderPath = fix $ \f -> \case
  ps Seq.:|> Attr k v -> f ps <> key k <> equals <> value v <> space
  ps Seq.:|> Push s -> f ps <> slash <> segment s <> space
  Seq.Empty -> mempty

-- | Escaping rules for 'Segment':
--
-- * A \'%\' anywhere is always percent-escaped (\"%25\")
--
-- * An ASCII-7 control character anywhere is always percent-escaped. 
--
-- The output is encoded as UTF-8.
message :: Message -> BB.Builder
{-# INLINE message #-}
message x = eall (unMessage x)
  where
    {-# INLINE eall #-}
    eall = TL.encodeUtf8BuilderEscaped
      $ BBP.condB (== 37) word8HexPercent  -- '%'
      $ BBP.condB isControl7 word8HexPercent
      $ BBP.liftFixedToBounded BBP.word8

-- | Escaping rules for 'Segment':
--
-- * An ASCII-7 punctuation character as first character is always percent-escaped.
--
-- * An ASCII-7 punctuation character anywhere else is always percent-escaped, unless it is
--   \'-\' or \'_\'.
--
-- * An ASCII-7 control character anywhere is always percent-escaped. 
--
-- The output is encoded as UTF-8.
segment :: Segment -> BB.Builder
{-# INLINE segment #-}
segment x = case TL.uncons (unSegment x) of
    Nothing -> mempty
    Just (hd,tl) -> ehead (T.singleton hd) <> etail tl
  where
    {-# INLINE ehead #-}
    ehead = T.encodeUtf8BuilderEscaped
      $ BBP.condB isPunctuation7 word8HexPercent
      $ BBP.condB isControl7 word8HexPercent
      $ BBP.liftFixedToBounded BBP.word8
    {-# INLINE etail #-}
    etail = TL.encodeUtf8BuilderEscaped
      $ BBP.condB (\w -> w == 0x2d    -- '-'
                      || w == 0x5f)   -- '_'
                  (BBP.liftFixedToBounded BBP.word8)
      $ BBP.condB isPunctuation7 word8HexPercent
      $ BBP.condB isControl7 word8HexPercent
      $ BBP.liftFixedToBounded BBP.word8

-- | Escaping rules for 'Key':
--
-- * An ASCII-7 control character is always percent-escaped.
--
-- * An ASCII-7 punctuation character is always percent-escaped.
--
-- * An ASCII-7 punctuation character anywhere else is always percent-escaped, unless it is
--   \'-\' or \'_\'.
--
-- The output is encoded as UTF-8.
key :: Key -> BB.Builder
{-# INLINE key #-}
key x = case TL.uncons (unKey x) of
    Nothing -> mempty
    Just (hd,tl) -> ehead (T.singleton hd) <> etail tl
  where
    {-# INLINE ehead #-}
    ehead = T.encodeUtf8BuilderEscaped
      $ BBP.condB isPunctuation7 word8HexPercent
      $ BBP.condB isControl7 word8HexPercent
      $ BBP.liftFixedToBounded BBP.word8
    {-# INLINE etail #-}
    etail = TL.encodeUtf8BuilderEscaped
      $ BBP.condB (\w -> w == 0x2d    -- '-'
                      || w == 0x5f)   -- '_'
                  (BBP.liftFixedToBounded BBP.word8)
      $ BBP.condB isPunctuation7 word8HexPercent
      $ BBP.condB isControl7 word8HexPercent
      $ BBP.liftFixedToBounded BBP.word8

-- | Escaping rules for 'Value':
--
-- * A \' \' anywhere is always percent-escaped (\"%20\").
--
-- * A \'%\' anywhere is always percent-escaped (\"%25\")"
--
-- * A \'=\' anywhere is always percent-escaped (\"%3d\").
--
-- * An ASCII-7 control character anywhere is always percent-escaped.
--
-- The output is encoded as UTF-8.
value :: Value -> BB.Builder
{-# INLINE value #-}
value x = eall (unValue x)
  where
    {-# INLINE eall #-}
    eall = TL.encodeUtf8BuilderEscaped
      $ BBP.condB (== 0x20) word8HexPercent
      $ BBP.condB (== 0x25) word8HexPercent
      $ BBP.condB (== 0x3d) word8HexPercent
      $ BBP.condB isControl7 word8HexPercent
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

-- -- | Reset background
-- bgDefault :: BB.Builder
-- bgDefault = BB.string7 "\x1b[49m"
-- {-# INLINE bgDefault #-}

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

-- | Red background
bgWhite :: BB.Builder
bgWhite = BB.string7 "\x1b[47m"
{-# INLINE bgWhite #-}

-- | Render @'%'@ followed by the given 'Word8' rendered as two hexadecimal
-- nibbles.
word8HexPercent :: BBP.BoundedPrim Word8
word8HexPercent = BBP.liftFixedToBounded
  ((\x -> (37, x)) BBP.>$< BBP.word8 BBP.>*< BBP.word8HexFixed)
{-# INLINE word8HexPercent #-}

--------------------------------------------------------------------------------

-- | Renders /YYYY-MM-DDThh:mm:ss.sssssssssZ/ (nanosecond precision).
--
-- The rendered string is 30 characters long, and it's encoded as ASCII/UTF-8.
iso8601 :: Time.SystemTime -> BB.Builder
{-# INLINE iso8601 #-}
iso8601 = \syst ->
  let Time.UTCTime tday tdaytime = Time.systemToUTCTime syst
      (year, month, day) = Time.toGregorian tday
      Time.TimeOfDay hour min' sec = Time.timeToTimeOfDay tdaytime
  in -- Notice that 'TB.decimal' RULES dispatch to faster code for smaller
     -- types (e.g., 'Word8' is faster to render than 'Int'), so we make
     -- seemingly redundant 'fromIntegral' conversions here to that effect.
     BB.int16Dec (fromIntegral year) <> BB.char7 '-' <>
     word8Dec_pad10 (fromIntegral month) <> BB.char7 '-' <>
     word8Dec_pad10 (fromIntegral day) <> BB.char7 'T' <>
     word8Dec_pad10 (fromIntegral hour) <> BB.char7 ':' <>
     word8Dec_pad10 (fromIntegral min') <> BB.char7 ':' <>
     word8Dec_pad10 (truncate sec) <> BB.char7 '.' <>
     word32Dec_pad100000000 (Time.systemNanoseconds syst) <> BB.char7 'Z'

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
{-# INLINE _zero1 #-}
{-# INLINE _zero2 #-}
{-# INLINE _zero3 #-}
{-# INLINE _zero4 #-}
{-# INLINE _zero5 #-}
{-# INLINE _zero6 #-}
{-# INLINE _zero7 #-}
{-# INLINE _zero8 #-}

-- | 'True' for all ASCII-7 punctuation characters.
isPunctuation7 :: Word8 -> Bool
{-# INLINE isPunctuation7 #-}
isPunctuation7 w =
  (w >= 32 && w <= 47)   ||
  (w >= 58 && w <= 64)   ||
  (w >= 91 && w <= 96)   ||
  (w >= 123 && w <= 126)

-- | 'True' for ASCII-7 control characters.
isControl7 :: Word8 -> Bool
{-# INLINE isControl7 #-}
isControl7 w = (w <= 31) || (w == 127)
