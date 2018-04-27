{-# LANGUAGE StandaloneDeriving #-}

module Di.Types
 ( Log(Log)
 , logTime, logLevel, logPath, logMessage
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push, Root)
 , Di(Di)
 , diMax, diPath, diLogs
 , Writer(Writer, unWriter)
 , LogRenderer(TextLogRenderer, BytesLogRenderer)
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
  deriving (Eq, Show)

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

--------------------------------------------------------------------------------

-- | A 'Writer' describes how a 'Log' is fully written (i.e., commited) to the
-- outside world.
newtype Writer = Writer { unWriter :: IO (Log -> IO ()) }
  -- ^ The outer 'IO' is run once by 'mkDi' to initialize anything that needs
  -- to be initialized in order for the actual writing function @'Log' -> 'IO'
  -- ()@ to work properly.

--------------------------------------------------------------------------------

-- | A 'LogRenderer' describes how to render a 'Log' as a blob of text or bytes.
data LogRenderer
  = TextLogRenderer !(Bool -> Log -> TB.Builder)
  -- ^ Render a 'Log' as text. The returned 'TB.Builder' shouldn't include a
  -- trailing newline. The given 'Bool' tells whether ANSI terminal colors are
  -- supported.
  | BytesLogRenderer !(Bool -> Log -> BB.Builder)
  -- ^ Render a 'Log' as bytes. The returned 'BB.Builder' shouldn't include a
  -- trailing newline. The given 'Bool' tells whether ANSI terminal colors are
  -- supported.

--------------------------------------------------------------------------------

data Di = Di
  { diMax :: !Level
    -- ^ Whether a particular message @level@ should be logged or not.
  , diPath :: !Path
    -- ^ Current path.
  , diLogs :: !(TQueue Log)
    -- ^ Work queue keeping 'Log's that need to be commited using '_diLog'.
  }

