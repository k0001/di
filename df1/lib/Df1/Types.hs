{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Df1.Types
 ( Log(Log, log_time, log_level, log_path, log_message)
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push), ToPath(path)
 , Segment, unSegment, ToSegment(segment)
 , Key, unKey, ToKey(key)
 , Value, unValue, ToValue(value)
 , Message, unMessage, ToMessage(message)
 ) where

import Control.Exception (SomeException)
import qualified Data.Fixed as Fixed
import Data.Foldable (toList)
import Data.Semigroup (Semigroup((<>)))
import Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.String (IsString(fromString))
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Format.ISO8601 as Time

--------------------------------------------------------------------------------

data Log = Log
  { log_time :: !Time.SystemTime
    -- ^ First known timestamp when the log was generated.
    --
    -- We use 'Time.SystemTime' rather than 'Time.UTCTime' because it is
    -- cheaper to obtain and to render. You can use
    -- 'Data.Time.Clock.System.systemToUTCTime' to convert it if necessary.
  , log_level :: !Level
    -- ^ Importance level of the logged message.
  , log_path :: !(Seq.Seq Path)
    -- ^ 'Path' where the logged message was created from.
    --
    -- The leftmost 'Path' is the closest to the root. The rightmost 'Path' is
    -- the one closest to where the log was generated.
    --
    -- An 'Seq.empty' 'Seq.Seq' is acceptable, conveying the idea of the “root
    -- path”.
  , log_message :: !Message
    -- ^ Human-readable message itself.
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | A message text.
--
-- If you have the @OverloadedStrings@ GHC extension enabled, you can build a
-- 'Message' using a string literal:
--
-- @
-- \"foo\" :: 'Message'
-- @
--
-- Otherwise, you can use 'fromString' or 'message'.
--
-- Notice that @\"\" :: 'Message'@ is acceptable, and will be correctly rendered
-- and parsed back.
newtype Message = Message TL.Text
  deriving (Eq, Show)

unMessage :: Message -> TL.Text
unMessage = \(Message x) -> x
{-# INLINE unMessage #-}

instance IsString Message where
  fromString = message
  {-# INLINE fromString #-}

instance Semigroup Message where
  (<>) (Message a) (Message b) = Message (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Message where
  mempty = Message mempty
  {-# INLINE mempty #-}
  mappend (Message a) (Message b) = Message (mappend a b)
  {-# INLINE mappend #-}

-- | Convert an arbitrary type to a 'Message'.
--
-- You are encouraged to create custom 'ToMessage' instances for your types
-- making sure you avoid rendering sensitive details such as passwords, so that
-- they don't accidentally end up in logs.
--
-- Any characters that need to be escaped for rendering will be automatically
-- escaped at rendering time. You don't need to escape them here.
class ToMessage a where
  message :: a -> Message

-- | Identity.
instance ToMessage Message where
  message = id
  {-# INLINE message #-}

-- |
-- @
-- x :: 'TL.Text' == 'unMessage' ('message' x)
-- @
instance ToMessage TL.Text where
  message = Message
  {-# INLINE message #-}

-- |
-- @
-- x :: 'T.Text' == 'TL.toStrict' ('unMessage' ('message' x))
-- @
instance ToMessage T.Text where
  message = Message . TL.fromStrict
  {-# INLINE message #-}

-- |
-- @
-- x :: 'String' == 'TL.unpack' ('unMessage' ('message' x))
-- @
instance ToMessage String where
  message = Message . TL.pack
  {-# INLINE message #-}

instance ToMessage SomeException where
  message = message . show
  {-# INLINE message #-}

--------------------------------------------------------------------------------

-- | Importance of the logged message.
--
-- These levels, listed in increasing order of importance, correspond to the
-- levels used by [syslog(3)](https://linux.die.net/man/3/syslog).
data Level
  = Debug
  -- ^ Message intended to be useful only when deliberately debugging a program.
  | Info
  -- ^ Informational message.
  | Notice
  -- ^ A condition that is not an error, but should possibly be handled
  -- specially.
  | Warning
  -- ^ A warning condition, such as an exception being gracefully handled or
  -- some missing configuration setting being assigned a default value.
  | Error
  -- ^ Error condition, such as an unhandled exception.
  | Critical
  -- ^ Critical condition that could result in system failure, such as a disk
  -- running out of space.
  | Alert
  -- ^ A condition that should be corrected immediately, such as a corrupted
  -- database.
  | Emergency
  -- ^ System is unusable.
  deriving (Eq, Show, Bounded, Enum)

-- | Order of importance. For example, 'Emergency' is more important than
-- 'Debug':
--
-- @
-- 'Emergency' > 'Debug'  ==  'True'
-- @
deriving instance Ord Level

--------------------------------------------------------------------------------

-- | A path segment.
--
-- If you have the @OverloadedStrings@ GHC extension enabled, you can build a
-- 'Segment' using a string literal:
--
-- @
-- \"foo\" :: 'Segment'
-- @
--
-- Otherwise, you can use 'fromString' or 'segment'.
--
-- Notice that @\"\" :: 'Segment'@ is acceptable, and will be correctly rendered
-- and parsed back.
newtype Segment = Segment TL.Text
  deriving (Eq, Show)

unSegment :: Segment -> TL.Text
unSegment = \(Segment x) -> x
{-# INLINE unSegment #-}

instance IsString Segment where
  fromString = segment
  {-# INLINE fromString #-}

instance Semigroup Segment where
  (<>) (Segment a) (Segment b) = Segment (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Segment where
  mempty = Segment mempty
  {-# INLINE mempty #-}
  mappend (Segment a) (Segment b) = Segment (mappend a b)
  {-# INLINE mappend #-}

-- | Convert an arbitrary type to a 'Segment'.
--
-- You are encouraged to create custom 'ToSegment' instances for your types
-- making sure you avoid rendering sensitive details such as passwords, so that
-- they don't accidentally end up in logs.
--
-- Any characters that need to be escaped for rendering will be automatically
-- escaped at rendering time. You don't need to escape them here.
class ToSegment a where
  segment :: a -> Segment

-- | Identity.
instance ToSegment Segment where
  segment = id
  {-# INLINE segment #-}

-- |
-- @
-- x :: 'TL.Text' == 'unSegment' ('segment' x)
-- @
instance ToSegment TL.Text where
  segment = Segment
  {-# INLINE segment #-}

-- |
-- @
-- x :: 'T.Text' == 'TL.toStrict' ('unSegment' ('segment' x))
-- @
instance ToSegment T.Text where
  segment = Segment . TL.fromStrict
  {-# INLINE segment #-}

-- |
-- @
-- x :: 'String' == 'TL.unpack' ('unSegment' ('segment' x))
-- @
instance ToSegment String where
  segment = Segment . TL.pack
  {-# INLINE segment #-}

--------------------------------------------------------------------------------

-- | An attribute key (see 'Attr').
--
-- If you have the @OverloadedStrings@ GHC extension enabled, you can build a
-- 'Key' using a string literal:
--
-- @
-- \"foo\" :: 'Key'
-- @
--
-- Otherwise, you can use 'fromString' or 'key'.
--
-- Notice that @\"\" :: 'Key'@ is acceptable, and will be correctly rendered and
-- parsed back.
newtype Key = Key TL.Text
  deriving (Eq, Show)

unKey :: Key -> TL.Text
unKey = \(Key x) -> x
{-# INLINE unKey #-}

instance IsString Key where
  fromString = key
  {-# INLINE fromString #-}

instance Semigroup Key where
  (<>) (Key a) (Key b) = Key (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Key where
  mempty = Key mempty
  {-# INLINE mempty #-}
  mappend (Key a) (Key b) = Key (mappend a b)
  {-# INLINE mappend #-}

-- | Convert an arbitrary type to a 'Key'.
--
-- You are encouraged to create custom 'ToKey' instances for your types
-- making sure you avoid rendering sensitive details such as passwords, so that
-- they don't accidentally end up in logs.
--
-- Any characters that need to be escaped for rendering will be automatically
-- escaped at rendering time. You don't need to escape them here.
class ToKey a where
  key :: a -> Key

-- | Identity.
instance ToKey Key where
  key = id
  {-# INLINE key #-}

-- |
-- @
-- x :: 'TL.Text' == 'unKey' ('key' x)
-- @
instance ToKey TL.Text where
  key = Key
  {-# INLINE key #-}

-- |
-- @
-- x :: 'T.Text' == 'TL.toStrict' ('unKey' ('key' x))
-- @
instance ToKey T.Text where
  key = Key . TL.fromStrict
  {-# INLINE key #-}

-- |
-- @
-- x :: 'String' == 'TL.unpack' ('unKey' ('key' x))
-- @
instance ToKey String where
  key = Key . TL.pack
  {-# INLINE key #-}

--------------------------------------------------------------------------------

-- | An attribute value (see 'Attr').
--
-- If you have the @OverloadedStrings@ GHC extension enabled, you can build a
-- 'Value' using a string literal:
--
-- @
-- \"foo\" :: 'Value'
-- @
--
-- Otherwise, you can use 'fromString' or 'value'.
--
-- Notice that @\"\" :: 'Value'@ is acceptable, and will be correctly rendered
-- and parsed back.
newtype Value = Value TL.Text
  deriving (Eq, Show)

unValue :: Value -> TL.Text
unValue = \(Value x) -> x
{-# INLINE unValue #-}

instance IsString Value where
  fromString = value
  {-# INLINE fromString #-}

instance Semigroup Value where
  (<>) (Value a) (Value b) = Value (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Value where
  mempty = Value mempty
  {-# INLINE mempty #-}
  mappend (Value a) (Value b) = Value (mappend a b)
  {-# INLINE mappend #-}

-- | Convert an arbitrary type to a 'Value'.
--
-- You are encouraged to create custom 'ToValue' instances for your types
-- making sure you avoid rendering sensitive details such as passwords, so that
-- they don't accidentally end up in logs.
--
-- Any characters that need to be escaped for rendering will be automatically
-- escaped at rendering time. You don't need to escape them here.
class ToValue a where
  value :: a -> Value

-- | Identity.
instance ToValue Value where
  value = id
  {-# INLINE value #-}

-- |
-- @
-- x :: 'TL.Text' == 'unValue' ('value' x)
-- @
instance ToValue TL.Text where
  value = Value
  {-# INLINE value #-}

-- |
-- @
-- x :: 'T.Text' == 'TL.toStrict' ('unValue' ('value' x))
-- @
instance ToValue T.Text where
  value = Value . TL.fromStrict
  {-# INLINE value #-}

-- |
-- @
-- x :: 'String' == 'TL.unpack' ('unValue' ('value' x))
-- @
instance ToValue String where
  value = Value . TL.pack
  {-# INLINE value #-}

instance ToValue SomeException where
  value = value . show
  {-# INLINE value #-}

instance ToValue Bool where
  value = \b -> if b then Value "true" else Value "false"
  {-# INLINE value #-}

instance ToValue Int where
  value = value . show
  {-# INLINE value #-}
instance ToValue Int8 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Int16 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Int32 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Int64 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Word where
  value = value . show
  {-# INLINE value #-}
instance ToValue Word8 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Word16 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Word32 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Word64 where
  value = value . show
  {-# INLINE value #-}
instance ToValue Integer where
  value = value . show
  {-# INLINE value #-}
instance ToValue Natural where
  value = value . show
  {-# INLINE value #-}
instance ToValue Float where
  value = value . show
  {-# INLINE value #-}
instance ToValue Double where
  value = value . show
  {-# INLINE value #-}
-- | Chops trailing zeros.
instance Fixed.HasResolution a => ToValue (Fixed.Fixed a) where
  value = value . Fixed.showFixed True
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.CalendarDiffDays where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.CalendarDiffTime where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.Day where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.TimeZone where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.TimeOfDay where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.LocalTime where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | See 'Time.ISO8601'.
instance ToValue Time.ZonedTime where
  value = value . Time.iso8601Show
  {-# INLINE value #-}
-- | @123456s@
instance ToValue Time.NominalDiffTime where
  value = value . show
  {-# INLINE value #-}
-- | @123456s@
instance ToValue Time.DiffTime where
  value = value . show
  {-# INLINE value #-}
-- | Lowercase @monday@, @tuesday@, etc.
instance ToValue Time.DayOfWeek where
   value = \x -> case x of
     Time.Monday    -> "monday"
     Time.Tuesday   -> "tuesday"
     Time.Wednesday -> "wednesday"
     Time.Thursday  -> "thursday"
     Time.Friday    -> "friday"
     Time.Saturday  -> "saturday"
     Time.Sunday    -> "sunday"

--------------------------------------------------------------------------------

-- | 'Path' represents the hierarchical structure of logged messages.
--
-- For example, consider a /df1/ log line as like the following:
--
-- @
-- 1999-12-20T07:11:39.230553031Z \/foo x=a y=b \/bar \/qux z=c z=d WARNING Something
-- @
--
-- For that line, the 'log_path' attribute of the 'Log' datatype will contain
-- the following:
--
-- @
-- [ 'Push' ('segment' \"foo\")
-- , 'Attr' ('key' \"x\") ('value' \"a\")
-- , 'Attr' ('key' \"y\") ('value' \"b\")
-- , 'Push' ('segment' \"bar\")
-- , 'Push' ('segment' \"qux\")
-- , 'Attr' ('key' \"z\") ('value' \"c\")
-- , 'Attr' ('key' \"z\") ('value' \"d\")
-- ] :: 'Seq.Seq' 'Path'
-- @
--
-- Please notice that @[] :: 'Seq.Seq' 'Path'@ is a valid path insofar as /df1/
-- is concerned, and that 'Attr' and 'Push' can be juxtapositioned in any order.
data Path
  = Push !Segment
  | Attr !Key !Value
  deriving (Eq, Show)

-- | Convert an arbitrary type to a 'Seq'uence of 'Path's.
--
-- You are encouraged to create custom 'ToPath' instances for your types
-- making sure you avoid rendering sensitive details such as passwords, so that
-- they don't accidentally end up in logs.
--
-- Any characters that need to be escaped for rendering will be automatically
-- escaped at rendering time. You don't need to escape them here.
class ToPath a where
  -- | The leftmost 'Path' is the closest to the root. The rightmost 'Path' is
  -- the one closest to where the log was generated.
  --
  -- See the documentation for 'Path'.
  path :: a -> Seq.Seq Path

-- | Identity.
instance ToPath (Seq.Seq Path) where
  path = id
  {-# INLINE path #-}

-- |
-- @
-- 'path' = 'Seq.fromList' . 'toList'
-- @
instance {-# OVERLAPPABLE #-} Foldable f => ToPath (f Path) where
  path = Seq.fromList . toList
  {-# INLINE path #-}
