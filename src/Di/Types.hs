{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Di.Types
 ( Log(Log, logTime, logLevel, logPath, logMessage)
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push, Root)
 , pathRoot
 , Di(Di, diMax, diPath, diLogs)
 , LogLineRenderer(LogLineRendererUtf8)
 , LogBlobRenderer(LogBlobRenderer)
 , RenderMode(RenderModeBlob, RenderModeUtf8)
 ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Builder as BB
import qualified Data.Time.Clock.System as Time
import Control.Concurrent.STM (TQueue)

--------------------------------------------------------------------------------

data Log = Log
  { logTime :: !Time.SystemTime
  , logLevel :: !Level
  , logPath :: !Path
  , logMessage :: !TL.Text
  } deriving (Eq, Show)

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

-- We keep the strings as lazy 'TL.Text', even if short, so that we avoid
-- calling 'TL.fromStrict' time and time again when rendering this 'Path'.
data Path
  = Root !TL.Text
  | Push !TL.Text !Path
  | Attr !TL.Text !TL.Text !Path
  deriving (Eq, Show)

-- | Finds the 'Root' of a 'Path'.
pathRoot :: Path -> Path
pathRoot (Attr _ _ p) = pathRoot p
pathRoot (Push _ p) = pathRoot p
pathRoot (Root x) = Root x

--------------------------------------------------------------------------------

data RenderMode
  = RenderModeUtf8  -- ^ UTF-8 encoded text.
  | RenderModeBlob  -- ^ A blob of binary data.
  deriving (Show)

--------------------------------------------------------------------------------

-- | How to render a 'Log' as a line of text.
data LogLineRenderer
  = LogLineRendererUtf8 !(Bool -> Log -> BB.Builder)
  -- ^ The returned bytes must not contain a trailing newline.
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

