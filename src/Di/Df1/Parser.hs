{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1.Parser
 {- ( parseLog
 ) -} where

import Control.Applicative ((<|>), many, empty)
import Control.Monad (void)
import Data.Bits (shiftL)
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

import Di.Types as Di
 (Log(Log), Message, message,
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root), Segment, segment, Key, key, Value, value)

--------------------------------------------------------------------------------

parseLog :: AB.Parser Di.Log
parseLog = (AB.<?> "parseLog") $ do
  t <- AB.skipWhile (== 32) *> pIso8601
  p <- AB.skipWhile (== 32) *> pPath
  l <- AB.skipWhile (== 32) *> pLevel
  m <- AB.skipWhile (== 32) *> pMessage
  -- Make sure we reached the end or a newline.
  -- void AB.atEnd <|> void (AB.word8 10) <|> void (AB.word8 13 >> AB.word8 10)
  pure (Log (Time.utcToSystemTime t) l p m)

pIso8601 :: AB.Parser Time.UTCTime
{-# INLINE pIso8601 #-}
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
{-# INLINE pNum1Digit #-}
pNum1Digit = AB.satisfyWith (subtract 48) (< 10) AB.<?> "pNum1Digit"

pNum2Digits :: AB.Parser Word8
{-# INLINE pNum2Digits #-}
pNum2Digits = (AB.<?> "pNum2Digits") $ do
  (+) <$> fmap (* 10) pNum1Digit <*> pNum1Digit

pNum4Digits :: AB.Parser Word16
{-# INLINE pNum4Digits #-}
pNum4Digits = (AB.<?> "pNum4Digits") $ do
  (\a b c d -> a + b + c + d)
     <$> fmap ((* 1000) . fromIntegral) pNum1Digit
     <*> fmap ((* 100) . fromIntegral) pNum1Digit
     <*> fmap ((* 10) . fromIntegral) pNum1Digit
          <*> fmap fromIntegral pNum1Digit

pNum9Digits :: AB.Parser Word32
{-# INLINE pNum9Digits #-}
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

pLevel :: AB.Parser Di.Level
{-# INLINE pLevel #-}
pLevel = (AB.<?> "pLevel")
  -- In decreasing frequency we expect logs to happen.
  -- We expect 'Debug' to mostly be muted, so 'Info' is prefered.
  (AB.string "INFO"      $> Di.Info)     <|>
  (AB.string "DEBUG"     $> Di.Debug)    <|>
  (AB.string "NOTICE"    $> Di.Notice)   <|>
  (AB.string "WARNING"   $> Di.Warning)  <|>
  (AB.string "ERROR"     $> Di.Error)    <|>
  (AB.string "CRITICAL"  $> Di.Critical) <|>
  (AB.string "ALERT"     $> Di.Alert)    <|>
  (AB.string "EMERGENCY" $> Di.Emergency)

pPath :: AB.Parser Di.Path
{-# INLINE pPath #-}
pPath = (AB.<?> "pPath") $ do
    fix (\k path -> ((pPush path <|> pAttr path) >>= k) <|> pure path) Root
  where
    {-# INLINE pPush #-}
    pPush :: Di.Path -> AB.Parser Di.Path
    pPush path = (AB.<?> "pPush") $ do
      seg <- pSegment <* AB.skipWhile (== 32)
      pure (Di.Push seg path)
    {-# INLINE pAttr #-}
    pAttr :: Di.Path -> AB.Parser Di.Path
    pAttr path = do
      key <- pKey <* AB.skip (== 61)
      value <- pValue <* AB.skipWhile (== 32)
      pure (Di.Attr key value path)

pSegment :: AB.Parser Di.Segment
pSegment = (AB.<?> "pSegment") $ do
  AB.skip (== 47) AB.<?> "/"
  bl <- pUtf8LtoL =<< pDecodePercents =<< AB.takeWhile (/= 32) -- :space:
  pure (Di.segment (TL.toStrict bl))

pKey :: AB.Parser Di.Key
pKey = (AB.<?> "pKey") $ do

  bl <- pUtf8LtoL =<< pDecodePercents
          =<< AB.takeWhile (\w -> w /= 61 && w /= 32) -- '=' or :space:
  pure (Di.key (TL.toStrict bl))

pValue :: AB.Parser Di.Value
pValue = (AB.<?> "pValue") $ do
  bl <- pUtf8LtoL =<< pDecodePercents =<< AB.takeWhile (/= 32) -- :space:
  pure (Di.value bl)

pMessage :: AB.Parser Di.Message
{-# INLINE pMessage #-}
pMessage = (AB.<?> "pMessage") $ do
  tl <- pUtf8LtoL =<< pDecodePercentsL =<< AB.takeLazyByteString
  pure (Di.message tl)

pUtf8LtoL :: BL.ByteString -> AB.Parser TL.Text
{-# INLINE pUtf8LtoL #-}
pUtf8LtoL = \bl -> case TL.decodeUtf8' bl of
   Right x -> pure x
   Left e -> fail (show e) AB.<?> "pUtf8LtoL"

-- | Parse @\"%FF\"@. Always consumes 3 bytes from the input, if successful.
pNumPercent :: AB.Parser Word8
{-# INLINE pNumPercent #-}
pNumPercent = (AB.<?> "pNum2Nibbles") $ do
   AB.skip (== 37) -- percent
   wh <- pHexDigit
   wl <- pHexDigit
   pure (shiftL wh 4 + wl)

pHexDigit :: AB.Parser Word8
{-# INLINE pHexDigit #-}
pHexDigit = AB.satisfyWith
  (\case w | w >= 48 && w <=  57 -> w - 48
           | w >= 65 && w <=  70 -> w - 55
           | w >= 97 && w <= 102 -> w - 87
           | otherwise -> 99)
  (\w -> w /= 99)

-- | Like 'pDecodePercentsL' but takes strict bytes.
pDecodePercents :: B.ByteString -> AB.Parser BL.ByteString
{-# INLINE pDecodePercents #-}
pDecodePercents = pDecodePercentsL . BL.fromStrict

-- | Decodes all 'pNumPercent' occurences from the given input.
--
-- TODO: Make faster and more space efficient.
pDecodePercentsL :: BL.ByteString -> AB.Parser BL.ByteString
{-# INLINE pDecodePercentsL #-}
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

-- | Remove ANSI escapes. This is not complete, but it covers the ANSI codes we
-- use.
removeAnsiEscapes :: B.ByteString -> B.ByteString
removeAnsiEscapes b0 = do
    case AB.parseOnly p b0 of
       Right b1 -> b1
       Left e -> error ("removeAnsiEscapes: unexpected " ++ e)
  where
    p :: AB.Parser B.ByteString
    p = fmap B.concat $ many $
          (AB.takeWhile1 (/= 27)) <|>
          (pAnsiEscape $> "") <|>
          (AB.satisfy (== 27) $> "")
    pAnsiEscape :: AB.Parser ()
    pAnsiEscape = (AB.<?> "pAnsiEscape") $ do
      AB.satisfy (== 27) AB.<?> "a" -- '\ESC'
      AB.satisfy (== 91) AB.<?> "b" -- '['
      AB.takeWhile (\w -> w == 59 || (w >= 48 && w <= 57)) -- ';' '0'-'9'
      AB.satisfy (== 109) AB.<?> "d" -- 'm'
      pure ()

