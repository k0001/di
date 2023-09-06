{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Parse
 ( log
 ) where

import Control.Applicative ((<|>), many, empty)
import Data.Bits (shiftL)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)
import Data.Functor (($>))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word8, Word16, Word32)
import Prelude hiding (log)

import Df1.Render () -- To make sure module instances are available here too.

import Df1.Types
 (Log(Log, log_time, log_level, log_path, log_message),
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push),
  Segment, segment,
  Key, key,
  Value, value,
  Message, message)

--------------------------------------------------------------------------------

-- | If sucessful, parsing will stop after the first CR or LF newline marker if
-- any, otherwise it will consume all input.
log :: AB.Parser Log
log = (AB.<?> "log") $ do
  t <- AB.skipWhile (== 32) *> pIso8601 -- :space:
  p <- AB.skipWhile (== 32) *> pPath
  l <- AB.skipWhile (== 32) *> pLevel
  m <- AB.skip (== 32) *> pMessage
  pure (Log { log_time = Time.utcToSystemTime t
            , log_level = l, log_path = p, log_message = m })

pIso8601 :: AB.Parser Time.UTCTime
pIso8601 = (AB.<?> "pIso8601") $ do
  year <- (pNum4Digits AB.<?> "year") <* (AB.skip (== 45) AB.<?> "-")
  month <- (pNum2Digits AB.<?> "month") <* (AB.skip (== 45) AB.<?> "-")
  day <- (pNum2Digits AB.<?> "day") <* (AB.skip (== 84) AB.<?> "T")
  Just tday <- pure (Time.fromGregorianValid
     (fromIntegral year) (fromIntegral month) (fromIntegral day))
  hour <- (pNum2Digits AB.<?> "hour") <* (AB.skip (== 58) AB.<?> ":")
  min' <- (pNum2Digits AB.<?> "minute") <* (AB.skip (== 58) AB.<?> ":")
  sec <- (pNum2Digits AB.<?> "second") <* (AB.skip (== 46) AB.<?> ".")
  nsec <- (pNum9Digits AB.<?> "nanosecond") <* (AB.skip (== 90) AB.<?> "Z")
  Just ttod <- pure (Time.makeTimeOfDayValid
     (fromIntegral hour) (fromIntegral min')
     (fromIntegral sec + (fromIntegral nsec / 1000000000)))
  pure (Time.UTCTime tday (Time.timeOfDayToTime ttod))

pNum1Digit :: AB.Parser Word8
pNum1Digit = AB.satisfyWith (subtract 48) (< 10) AB.<?> "pNum1Digit"

pNum2Digits :: AB.Parser Word8
pNum2Digits = (AB.<?> "pNum2Digits") $ do
  (+) <$> fmap (* 10) pNum1Digit <*> pNum1Digit

pNum4Digits :: AB.Parser Word16
pNum4Digits = (AB.<?> "pNum4Digits") $ do
  (\a b c d -> a + b + c + d)
     <$> fmap ((* 1000) . fromIntegral) pNum1Digit
     <*> fmap ((* 100) . fromIntegral) pNum1Digit
     <*> fmap ((* 10) . fromIntegral) pNum1Digit
          <*> fmap fromIntegral pNum1Digit

pNum9Digits :: AB.Parser Word32
pNum9Digits = (AB.<?> "pNum9Digits") $ do
  (\a b c d e f g h i -> a + b + c + d + e + f + g + h + i)
     <$> fmap ((* 100000000) . fromIntegral) pNum1Digit
     <*> fmap ((* 10000000) . fromIntegral) pNum1Digit
     <*> fmap ((* 1000000) . fromIntegral) pNum1Digit
     <*> fmap ((* 100000) . fromIntegral) pNum1Digit
     <*> fmap ((* 10000) . fromIntegral) pNum1Digit
     <*> fmap ((* 1000) . fromIntegral) pNum1Digit
     <*> fmap ((* 100) . fromIntegral) pNum1Digit
     <*> fmap ((* 10) . fromIntegral) pNum1Digit
     <*> fmap fromIntegral pNum1Digit

pLevel :: AB.Parser Level
pLevel = (AB.<?> "pLevel")
  -- In decreasing frequency we expect logs to happen.
  -- We expect 'Debug' to mostly be muted, so 'Info' is prefered.
  (AB.string "INFO"      $> Info)     <|>
  (AB.string "DEBUG"     $> Debug)    <|>
  (AB.string "NOTICE"    $> Notice)   <|>
  (AB.string "WARNING"   $> Warning)  <|>
  (AB.string "ERROR"     $> Error)    <|>
  (AB.string "CRITICAL"  $> Critical) <|>
  (AB.string "ALERT"     $> Alert)    <|>
  (AB.string "EMERGENCY" $> Emergency)

pPath :: AB.Parser (Seq.Seq Path)
pPath = (AB.<?> "pPath") $ do
    fix (\k ps -> ((pPush <|> pAttr) >>= \p -> k (ps Seq.|> p)) <|> pure ps)
        mempty
  where
    pPush :: AB.Parser Path
    pPush = (AB.<?> "pPush") $ do
      seg <- pSegment <* AB.skipWhile (== 32)
      pure (Push seg)
    pAttr :: AB.Parser Path
    pAttr = do
      k <- pKey <* AB.skip (== 61)
      v <- pValue <* AB.skipWhile (== 32)
      pure (Attr k v)

pSegment :: AB.Parser Segment
pSegment = (AB.<?> "pSegment") $ do
  AB.skip (== 47) AB.<?> "/"
  bl <- pUtf8LtoL =<< pDecodePercents =<< AB.takeWhile (/= 32) -- :space:
  pure (segment bl)

pKey :: AB.Parser Key
pKey = (AB.<?> "pKey") $ do
  bl <- pUtf8LtoL =<< pDecodePercents
          =<< AB.takeWhile (\w -> w /= 61 && w /= 32) -- '=' or :space:
  pure (key bl)

pValue :: AB.Parser Value
pValue = (AB.<?> "pValue") $ do
  bl <- pUtf8LtoL =<< pDecodePercents =<< AB.takeWhile (/= 32) -- :space:
  pure (value bl)

pMessage :: AB.Parser Message
pMessage = (AB.<?> "pMessage") $ do
  b <- AB.takeWhile (\w -> w /= 10 && w /= 13) -- CR and LF
  tl <- pUtf8LtoL =<< pDecodePercents b
  pure (message tl)

pUtf8LtoL :: BL.ByteString -> AB.Parser TL.Text
pUtf8LtoL = \bl -> case TL.decodeUtf8' bl of
   Right x -> pure x
   Left e -> fail (show e) AB.<?> "pUtf8LtoL"

-- | Parse @\"%FF\"@. Always consumes 3 bytes from the input, if successful.
pNumPercent :: AB.Parser Word8
pNumPercent = (AB.<?> "pNum2Nibbles") $ do
   AB.skip (== 37) -- percent
   wh <- pHexDigit
   wl <- pHexDigit
   pure (shiftL wh 4 + wl)

pHexDigit :: AB.Parser Word8
pHexDigit = AB.satisfyWith
  (\case w | w >= 48 && w <=  57 -> w - 48
           | w >= 65 && w <=  70 -> w - 55
           | w >= 97 && w <= 102 -> w - 87
           | otherwise -> 99)
  (\w -> w /= 99)

-- | Like 'pDecodePercentsL' but takes strict bytes.
pDecodePercents :: B.ByteString -> AB.Parser BL.ByteString
pDecodePercents = pDecodePercentsL . BL.fromStrict

-- | Decodes all 'pNumPercent' occurences from the given input.
--
-- TODO: Make faster and more space efficient.
pDecodePercentsL :: BL.ByteString -> AB.Parser BL.ByteString
pDecodePercentsL = \bl ->
    either fail pure (ABL.eitherResult (ABL.parse p bl))
  where
    p :: AB.Parser BL.ByteString
    p = AB.atEnd >>= \case
          True -> pure mempty
          False -> fix $ \k -> do
             b <- AB.peekWord8 >>= \case
                Nothing -> empty
                Just 37 -> fmap B.singleton pNumPercent
                Just _  -> AB.takeWhile1 (\w -> w /= 37)
             bls <- many k <* AB.endOfInput
             pure (mconcat (BL.fromStrict b : bls))

