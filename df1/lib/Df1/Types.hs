{-# LANGUAGE StandaloneDeriving #-}

module Df1.Types
 ( Log(Log, log_time, log_level, log_path, log_message)
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push)
 , Segment, unSegment, segment
 , Key, unKey, key
 , Value, unValue, value
 , Message, unMessage, message
 ) where

import Data.Semigroup (Semigroup((<>)))
import Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.String (IsString(fromString))
import qualified Data.Time.Clock.System as Time

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
-- Please keep in mind that 'Message' will always strip surrounding whitespace.
-- That is:
--
-- @
-- \"x\" :: 'Message'  ==  \" x\"  == \"x \" == \" x \"
-- @
newtype Message = Message TL.Text
  deriving (Eq, Show)

message :: TL.Text -> Message
message = Message
{-# INLINE message #-}

unMessage :: Message -> TL.Text
unMessage = \(Message x) -> x
{-# INLINE unMessage #-}

instance IsString Message where
  fromString = message . TL.pack
  {-# INLINE fromString #-}

instance Semigroup Message where
  (<>) (Message a) (Message b) = Message (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Message where
  mempty = Message mempty
  {-# INLINE mempty #-}
  mappend (Message a) (Message b) = Message (mappend a b)
  {-# INLINE mappend #-}

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
-- Otherwise, you can use 'fromString' or the 'Segment' constructor directly.
newtype Segment = Segment T.Text
  deriving (Eq, Show)

segment :: T.Text -> Segment
segment = Segment . T.dropAround (== ' ')
{-# INLINE segment #-}

unSegment :: Segment -> T.Text
unSegment = \(Segment x) -> x
{-# INLINE unSegment #-}

instance IsString Segment where
  fromString = segment . T.pack
  {-# INLINE fromString #-}

instance Semigroup Segment where
  (<>) (Segment a) (Segment b) = Segment (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Segment where
  mempty = Segment mempty
  {-# INLINE mempty #-}
  mappend (Segment a) (Segment b) = Segment (mappend a b)
  {-# INLINE mappend #-}

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
-- Otherwise, you can use 'fromString' or the 'key' function.
--
-- Please keep in mind that 'Key' will always strip surrounding whitespace.
-- That is:
--
-- @
-- \"x\" :: 'Key'  ==  \" x\"  == \"x \" == \" x \"
-- @
newtype Key = Key T.Text
  deriving (Eq, Show)

key :: T.Text -> Key
key = Key
{-# INLINE key #-}

unKey :: Key -> T.Text
unKey = \(Key x) -> x
{-# INLINE unKey #-}

instance IsString Key where
  fromString = key . T.pack
  {-# INLINE fromString #-}

instance Semigroup Key where
  (<>) (Key a) (Key b) = Key (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Key where
  mempty = Key mempty
  {-# INLINE mempty #-}
  mappend (Key a) (Key b) = Key (mappend a b)
  {-# INLINE mappend #-}

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
-- Otherwise, you can use 'fromString' or the 'value' function.
--
-- Please keep in mind that 'value' will always strip surrounding whitespace.
-- That is:
--
-- @
-- \"x\" :: 'Value'  ==  \" x\"  == \"x \" == \" x \"
-- @
newtype Value = Value TL.Text
  deriving (Eq, Show)

unValue :: Value -> TL.Text
unValue = \(Value x) -> x
{-# INLINE unValue #-}

value :: TL.Text -> Value
value = Value
{-# INLINE value #-}

instance IsString Value where
  fromString = value . TL.pack
  {-# INLINE fromString #-}

instance Semigroup Value where
  (<>) (Value a) (Value b) = Value (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Value where
  mempty = Value mempty
  {-# INLINE mempty #-}
  mappend (Value a) (Value b) = Value (mappend a b)
  {-# INLINE mappend #-}

--------------------------------------------------------------------------------

-- | 'Path' represents the hierarchical structure of logged messages.
--
-- For example, consider a /df1/ log line as like the following:
--
-- @
-- 1999-12-20T07:11:39.230553031Z \/foo x=a y=b \/qux z=c z=d WARNING Something
-- @
--
-- For that line, the 'log_path' attribute of the 'Log' datatype will contain
-- the following:
--
-- @
-- [ 'Push' ('segment' \"foo\")
-- , 'Attr' ('key' \"x\") ('value' \"a\")
-- , 'Attr' ('key' \"y\") ('value' \"b\")
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

