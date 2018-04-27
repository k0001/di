{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Di.Writer
 ( stderr
 , handle
 ) where

import qualified Control.Exception as Ex
import qualified Data.ByteString.Builder as BB
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified System.IO as IO

#ifdef VERSION_unix
import qualified System.Posix.Terminal
import qualified System.Posix.IO
#endif

import Di.Types
  (Log, Writer(Writer), LogRenderer(TextLogRenderer, BytesLogRenderer))

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
  -> Writer
handle h (TextLogRenderer frender) = Writer $ do
  IO.hSetBinaryMode h False
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  !render <- frender <$> isTty h
  pure $ \log' -> do
     Ex.finally
        (TL.hPutStr h (TB.toLazyText (render log' <> TB.singleton '\n')))
        (IO.hFlush h)
handle h (BytesLogRenderer frender) = Writer $ do
  IO.hSetBinaryMode h True
  IO.hSetBuffering h (IO.BlockBuffering Nothing)
  !render <- frender <$> isTty h
  pure $ \log' -> do
     Ex.finally
        (BB.hPutBuilder h (render log' <> BB.char7 '\n'))
        (IO.hFlush h)

-- | 'Log's are written to 'IO.stderr' using 'IO.stderr''s locale encoding.
stderr
  :: LogRenderer -- ^ How to render each 'Log' line.
  -> Writer
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
