{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Assorted utility functions.
module Di.Misc
 ( renderIso8601
 , catchSync
 , muteSync
 , mute
 , getSystemTimeSTM
 ) where

import Control.Concurrent.STM (STM)
import qualified Control.Exception as Ex
import Data.Monoid ((<>))
import Data.Int (Int16)
import Data.Word (Word8, Word32)
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.ByteString.Builder as BB
import GHC.Conc (unsafeIOToSTM)

--------------------------------------------------------------------------------

-- | Renders /YYYY-MM-DDThh:mm:ss.sssssssssZ/ (nanosecond precision).
--
-- The rendered string is a 30 characters long, and it's ASCII-encoded.
renderIso8601 :: Time.SystemTime -> BB.Builder
{-# INLINABLE renderIso8601 #-}
renderIso8601 = \syst ->
  let Time.UTCTime tday tdaytime = Time.systemToUTCTime syst
      (year, month, day) = Time.toGregorian tday
      Time.TimeOfDay hour min sec = Time.timeToTimeOfDay tdaytime
  in -- Notice that 'TB.decimal' RULES dispatch to faster code for smaller
     -- types (e.g., 'Word8' is faster to render than 'Int'), so we make
     -- seemingly redundant 'fromIntegral' conversions here to that effect.
     BB.int16Dec (fromIntegral year) <> "-" <>
     word8Dec_pad10 (fromIntegral month) <> "-" <>
     word8Dec_pad10 (fromIntegral day) <> "T" <>
     word8Dec_pad10 (fromIntegral hour) <> ":" <>
     word8Dec_pad10 (fromIntegral min) <> ":" <>
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

getSystemTimeSTM :: STM Time.SystemTime
{-# INLINE getSystemTimeSTM #-}
getSystemTimeSTM = unsafeIOToSTM Time.getSystemTime

--------------------------------------------------------------------------------

-- | @'catchSync' m f@ runs @m@, and in case of /synchronous/ exceptions, calls
-- @f@ with said exception as argument. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'catchSync'. Notice that
-- neither synchronous nor asynchronous exceptions thrown by @f@ are handled.
catchSync :: IO a -> (Ex.SomeException -> IO a) -> IO a
{-# INLINE catchSync #-}
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwIO (ae :: Ex.AsyncException)
   Nothing -> f se

-- | @'muteSync' m@ runs @m@, and in case of /synchronous/ exceptions, it
-- ignores them and returns @()@. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'muteSync'.
muteSync :: IO () -> IO ()
{-# INLINE muteSync #-}
muteSync m = catchSync m (\_ -> pure ())

-- | @'mute' m@ runs @m@, and in case of either /synchronous/ or /asynchronous/
-- exceptions, it ignores them and returns @()@. Most of the times this is not
-- what you want.
mute :: IO () -> IO ()
{-# INLINE mute #-}
mute m = Ex.catch m (\(_ :: Ex.SomeException) -> pure ())
