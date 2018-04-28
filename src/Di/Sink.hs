{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Sink
 ( sinkFallback
 , stderr
 , handle
 ) where

import qualified Control.Exception as Ex
import qualified Data.ByteString.Builder as BB
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Time.Clock.System as Time
import qualified System.IO as IO

#ifdef VERSION_unix
import qualified System.Posix.Terminal
import qualified System.Posix.IO
#endif

import Di.Types
  (Log(Log,  logTime, logLevel, logPath, logMessage),
   SinkInit(SinkInit), Sink(Sink, sinkRun, sinkClose),
   LogRenderer(TextLogRenderer, BytesLogRenderer),
   Level(Error),
   Path(Attr), pathRoot)
import Di.Misc (catchSync)

--------------------------------------------------------------------------------

-- | Wraps a desired 'Sink' so that if its 'sinkClose' throws a synchronous
-- exception, a fallback 'Sink' will attempt to log the same log message
-- afterwards.
--
-- Notice that exceptions from the fallback writer itself are not handled.
--
-- 'sinkClose' will close the desired 'Sink' first, and then close the fallback
-- writer even in case of exceptions.
sinkFallback
  :: Sink  -- ^ Desired sink.
  -> Sink  -- ^ Fallback sink.
  -> Sink
sinkFallback (Sink rD cD) (Sink rF cF) = Sink
    { sinkRun = \log' -> do
        catchSync (rD log') $ \se -> do
           syst <- Ex.onException Time.getSystemTime (rF log')
           Ex.finally (rF (fallbackLog syst se log')) (rF log')
    , sinkClose = Ex.finally cD cF
    }
  where
    fallbackLog :: Time.SystemTime -> Ex.SomeException -> Log -> Log
    fallbackLog syst se log' = Log
      { logTime = syst, logLevel = Error
      , logPath = Attr "exception"
          (TL.pack (Ex.displayException se)) (pathRoot (logPath log'))
      , logMessage =
          "Got synchronous exception in desired Di Sink. The "  <>
          "log message that couldn't be written as desired will " <>
          "be rendered here afterwards as a fallback."
      }

--------------------------------------------------------------------------------

-- | 'Log's are written to 'IO.Handle', each one followed by a newline
-- (@'\\n'@).
handle
  :: IO.Handle
  -- ^ Handle where to write 'Log's.
  -> LogRenderer
  -- ^ How to render each 'Log'.
  --
  -- If a 'TextLogRenderer' is given, then the 'IO.Handle''s locale encoding is
  -- used.
  --
  -- If a 'BytesLogRenderer' is given, then it is rendered as is.
  -> SinkInit
handle h (TextLogRenderer frender) = SinkInit $ do
  IO.hSetBinaryMode h False
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  !render <- frender <$> isTty h
  pure $ Sink
    { sinkRun = \log' -> do
         Ex.finally
            (TL.hPutStr h (TB.toLazyText (render log' <> TB.singleton '\n')))
            (IO.hFlush h)
    , sinkClose = pure () }
handle h (BytesLogRenderer frender) = SinkInit $ do
  IO.hSetBinaryMode h True
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  !render <- frender <$> isTty h
  pure $ Sink
    { sinkRun = \log' -> do
         Ex.finally
            (BB.hPutBuilder h (render log' <> BB.char7 '\n'))
            (IO.hFlush h)
    , sinkClose = pure () }

-- | 'Log's are written to 'IO.stderr' using 'IO.stderr''s locale encoding.
stderr
  :: LogRenderer -- ^ How to render each 'Log' line.
  -> SinkInit
stderr = handle IO.stderr

--------------------------------------------------------------------------------

isTty :: IO.Handle -> IO Bool
#ifdef VERSION_unix
isTty h
  | h == IO.stderr = q System.Posix.IO.stdError
  | h == IO.stdout = q System.Posix.IO.stdOutput
  | otherwise = pure False
    -- We should convert h to a Fd and give it q. Not sure how to do that.
  where q = System.Posix.Terminal.queryTerminal
#else
isTty _ = pure False
#endif

