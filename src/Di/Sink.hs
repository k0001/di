{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Sink
 ( Sink(Sink)
 , withSink
 , sinkFallback
 , stderrLines
 , handleLines
 , handleBlob
 ) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_)
import qualified Control.Exception as Ex
import qualified Data.ByteString.Builder as BB
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time
import qualified System.IO as IO
import System.IO.Unsafe (unsafePerformIO)

#ifdef VERSION_unix
import qualified System.Posix.Terminal
import qualified System.Posix.IO
#endif

import Di.Types
  (Log(Log,  logTime, logLevel, logPath, logMessage), Level(Error),
   LogLineRenderer(LogLineRendererUtf8), LogBlobRenderer(LogBlobRenderer),
   Path(Root, Attr), Key(Key), Value(Value))
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
sinkFallback (Sink md) (Sink mf) = Sink $ do
    (cf, wf) <- mf
    (cd, wd) <- Ex.onException md cf
    pure $ (,) (Ex.onException cd cf) $ \log' -> do
       catchSync (wd log') $ \se -> do
          syst <- Time.getSystemTime `Ex.onException` wf log'
          Ex.finally (wf (fallbackLog syst se)) (wf log')
  where
    fallbackLog :: Time.SystemTime -> Ex.SomeException -> Log
    fallbackLog syst se = Log
      { logTime = syst, logLevel = Error
      , logPath = Attr
          (Key "exception") (Value (TL.pack (Ex.displayException se))) Root
      , logMessage =
          "Got synchronous exception in desired Di Sink. The " <>
          "log message that couldn't be written as desired will " <>
          "be rendered here afterwards as a fallback."
      }

--------------------------------------------------------------------------------

newtype Sink = Sink { unSink :: IO (IO (), (Log -> IO ())) }

-- | Obtain a “'Log' writing” function from a 'Sink'.
--
-- Any resources acquired during 'Sink' initialization are released afterwards.
--
-- The @'Log' -> 'IO' ()@ could throw exceptions.
withSink :: Sink -> ((Log -> IO ()) -> IO a) -> IO a
withSink (Sink acq) k = Ex.bracket acq fst (k . snd)

--------------------------------------------------------------------------------

-- | This very ugly gobal keeps track of 'IO.Handle's currently being used by
-- 'handleBlob', so that we don't try and use a same handle more than once,
-- which would cause output to be garbled.
mvHandles :: MVar [IO.Handle]
mvHandles = unsafePerformIO (newMVar [])
{-# NOINLINE mvHandles #-}

data HandleBusy = HandleBusy !IO.Handle deriving (Show)
instance Ex.Exception HandleBusy

-- | Like 'handleBlob', but each 'Log' is rendered as text in its own line.
--
-- If the given 'IO.Handle' is associated to a TTY supporting ANSI colors, and
-- the given 'LogLineRenderer' supports rendering with colors, then you will get
-- colorful output.
handleLines
  :: IO.Handle -- ^ Handle where to write 'Log's.
  -> LogLineRenderer -- ^ How to render each 'Log'.
  -> Sink
handleLines h (LogLineRendererUtf8 render0) = Sink $ do
  !render1 <- render0 <$> isTty h
  let !newline = BB.char7 '\n'
      render2 = \log' -> render1 log' <> newline
  unSink (handleBlob h (LogBlobRenderer render2))

-- | Write 'Log's to a 'IO.Handle' as a binary blob.
handleBlob
  :: IO.Handle -- ^ Handle where to write 'Log's.
  -> LogBlobRenderer -- ^ How to render each 'Log'.
  -> Sink
handleBlob h (LogBlobRenderer render) = Sink $ do
  modifyMVar mvHandles $ \hs ->
     case List.elem h hs of
       True -> Ex.throwIO (HandleBusy h)
       False -> do
          IO.hSetBinaryMode h True
          let !hs' = h : hs
              close = modifyMVar_ mvHandles (pure . List.delete h)
              act = \x -> Ex.finally (BB.hPutBuilder h (render x)) (IO.hFlush h)
          pure (hs', (close, act))

-- | 'Log's are written to 'IO.stderr',
--
-- TODO: Currently this *always* renders as UTF-8.
stderrLines
  :: LogLineRenderer -- ^ How to render each 'Log' line.
  -> Sink
stderrLines = handleLines IO.stderr

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

