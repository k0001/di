{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Di.Types
 ( Log(Log, logTime, logLevel, logPath, logMessage)
 , Message, message, unMessage
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push, Root)
 , Segment, segment, unSegment
 , Key, key, unKey
 , Value, value, unValue
 , Di(Di, diMax, diPath, diLogs)
 , LogLineParser(LogLineParserUtf8)
 , LogLineRenderer(LogLineRendererUtf8)
 , LogBlobRenderer(LogBlobRenderer)
 ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Semigroup (Semigroup)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.String (IsString(fromString))
import qualified Data.Time.Clock.System as Time
import Control.Concurrent.STM (TQueue)
import qualified Pipes.Parse as Pp

--------------------------------------------------------------------------------

data Log = Log
  { logTime :: !Time.SystemTime
  , logLevel :: !Level
  , logPath :: !Path
  , logMessage :: !Message
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
message = Message . TL.dropAround (== ' ')
{-# INLINE message #-}

unMessage :: Message -> TL.Text
unMessage = \(Message x) -> x
{-# INLINE unMessage #-}

instance IsString Message where
  fromString = message . TL.pack
  {-# INLINE fromString #-}

instance Semigroup Message
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

instance Semigroup Segment
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
key = Key . T.dropAround (== ' ')
{-# INLINE key #-}

unKey :: Key -> T.Text
unKey = \(Key x) -> x
{-# INLINE unKey #-}

instance IsString Key where
  fromString = key . T.pack
  {-# INLINE fromString #-}

instance Semigroup Key
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
value = Value . TL.dropAround (== ' ')
{-# INLINE value #-}

instance IsString Value where
  fromString = value . TL.pack
  {-# INLINE fromString #-}

instance Semigroup Value
instance Monoid Value where
  mempty = Value mempty
  {-# INLINE mempty #-}
  mappend (Value a) (Value b) = Value (mappend a b)
  {-# INLINE mappend #-}

--------------------------------------------------------------------------------

-- We keep the strings as lazy 'TL.Text', even if short, so that we avoid
-- calling 'TL.fromStrict' time and time again when rendering this 'Path'.
data Path
  = Root
  | Push !Segment !Path
  | Attr !Key !Value !Path
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | How to parse a 'Log' from a line of text.
data LogLineParser
  = LogLineParserUtf8
      !(forall m. Monad m => Pp.Parser B.ByteString m (Either String Log))
  -- ^ Parse a 'Log' from some UTF-8 bytes. The parser can asume the given input
  -- is non-empty, and contains an entire line without trailing @\"\\r\\n\"@
  -- nor @\"\\n\"@.
  --
  -- If the parser doesn't consume the entire input, then those leftovers are
  -- discarded.
  --
  -- If parsing fails, the returned 'String' should give a hint of why.

--------------------------------------------------------------------------------

-- | How to render a 'Log' as a line of text.
data LogLineRenderer
  = LogLineRendererUtf8 !(Bool -> Log -> BB.Builder)
  -- ^ The returned bytes must not contain a leading nor trailing newline.
  --
  -- The 'Bool' tells whether we are trying to write these bytes to a terminal
  -- that supports ANSI colors.

-- | How to render a 'Log' as a binary blob.
data LogBlobRenderer = LogBlobRenderer !(Log -> BB.Builder)

--------------------------------------------------------------------------------

data Di = Di
  { diMax :: !Level
    -- ^ Whether a particular message @level@ should be logged or not.
  , diPath :: !Path
    -- ^ Current path.
  , diLogs :: !(TQueue Log)
    -- ^ Work queue keeping 'Log's that need to be commited using '_diLog'.
  }

