{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Assorted utility functions.
module Di.Misc
 ( renderIso8601
 , catchSync
 ) where

import qualified Control.Exception as Ex
import Data.Monoid ((<>))
import Data.Int (Int16)
import Data.Word (Word8, Word32)
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

--------------------------------------------------------------------------------

-- | Renders /YYYY-MM-DDThh:mm:ss.sssssssssZ/ (nanosecond precision).
--
-- The rendered string is always 30 characters long.
renderIso8601 :: Time.SystemTime -> TB.Builder
{-# INLINABLE renderIso8601 #-}
renderIso8601 = \syst ->
  let Time.UTCTime tday tdaytime = Time.systemToUTCTime syst
      (year, month, day) = Time.toGregorian tday
      Time.TimeOfDay hour min sec = Time.timeToTimeOfDay tdaytime
  in -- Notice that 'TB.decimal' RULES dispatch to faster code for smaller
     -- types (e.g., 'Word8' is faster to render than 'Int'), so we make
     -- seemingly redundant 'fromIntegral' conversions here to that effect.
     TB.decimal (fromIntegral year :: Int16) <> "-" <>
     pad10 (fromIntegral month :: Word8) <> "-" <>
     pad10 (fromIntegral day :: Word8) <> "T" <>
     pad10 (fromIntegral hour :: Word8) <> ":" <>
     pad10 (fromIntegral min :: Word8) <> ":" <>
     pad10 (truncate sec :: Word8) <> "." <>
     pad100000000 (Time.systemNanoseconds syst :: Word32) <> "Z"

pad10 :: Integral a => a -> TB.Builder
pad10 x
  | x < 10 = TB.singleton '0' <> TB.decimal x
  | otherwise = TB.decimal x
{-# INLINE pad10 #-}

pad100000000 :: Integral a => a -> TB.Builder
{-# INLINE pad100000000 #-}
pad100000000 x
  | x < 10 = TB.fromText  "00000000" <> y
  | x < 100 = TB.fromText  "0000000" <> y
  | x < 1000 = TB.fromText  "000000" <> y
  | x < 10000 = TB.fromText  "00000" <> y
  | x < 100000 = TB.fromText  "0000" <> y
  | x < 1000000 = TB.fromText  "000" <> y
  | x < 10000000 = TB.fromText  "00" <> y
  | x < 100000000 = TB.singleton '0' <> y
  | otherwise = y
  where y = TB.decimal x

--------------------------------------------------------------------------------

-- | @'catchSync' m f@ runs @m@, and in case of /synchronous/ exceptions, calls
-- @f@ with said exception as argument. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'catchSync'. Notice that
-- neither synchronous nor asynchronous exceptions thrown by @f@ are handled.
catchSync :: IO a -> (Ex.SomeException -> IO a) -> IO a
{-# INLINABLE catchSync #-}
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwIO (ae :: Ex.AsyncException)
   Nothing -> f se

