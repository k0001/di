{-# LANGUAGE OverloadedStrings #-}

module Di.Df1.Parser

 where

import Control.Applicative ((<|>))
import Control.Monad.Fix (mfix)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)
import Data.Functor (($>))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import Data.Word (Word8, Word16, Word32)

import Di.Misc (iterateM)
import Di.Types as Di
 (Log(Log),
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root))

--------------------------------------------------------------------------------

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
pLevel = (AB.<?> "pLevel") $
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
pPath = (AB.<?> "pLevel") $ do
    pRoot >>= fix (\k a -> ((pAttr a <|> pPush a) >>= k) <|> pure a)
  where
    pRoot :: AB.Parser Di.Path
    pRoot = (AB.<?> "pRoot") $ do
      AB.skip (== 47) AB.<?> "/"
      key <- pUtf8L =<< (AB.takeWhile1 (/= 32) AB.<?> "key")
      pure (Di.Root key)
    pPush :: Di.Path -> AB.Parser Di.Path
    pPush path = (AB.<?> "pPush") $ do
      AB.skip (== 32) AB.<?> "' '"
      AB.skip (== 47) AB.<?> "/"
      key <- pUtf8L =<< (AB.takeWhile1 (/= 32) AB.<?> "key")
      pure (Di.Push key path)
    pAttr :: Di.Path -> AB.Parser Di.Path
    pAttr path = do
      AB.skip (== 32) AB.<?> "' '"
      key <- pUtf8L =<< (AB.takeWhile1 (/= 61) AB.<?> "key")
      AB.skip (== 61) AB.<?> "="
      val <- pUtf8L =<< (AB.takeWhile1 (/= 32) AB.<?> "value")
      pure (Di.Attr key val path)
    {-# INLINE pRoot #-}
    {-# INLINE pPush #-}
    {-# INLINE pAttr #-}

pUtf8L :: B.ByteString -> AB.Parser TL.Text
{-# INLINE pUtf8L #-}
pUtf8L = \b -> case TL.decodeUtf8' (BL.fromStrict b) of
   Right x -> pure x
   Left e -> fail (show e) AB.<?> "pUtf8L"

