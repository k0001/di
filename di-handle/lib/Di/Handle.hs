{-# LANGUAGE CPP #-}

module Di.Handle
 ( stderr
 , handle
 , blob
 , LineRenderer(LineRendererUtf8)
 , BlobRenderer(BlobRenderer)
 ) where

import qualified Control.Monad.Catch as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Builder as BB
import Data.Monoid ((<>))
import qualified System.IO as IO

#ifdef VERSION_unix
import qualified System.Posix.Terminal
import qualified System.Posix.IO
#endif

import Di.Core (Log)

--------------------------------------------------------------------------------

-- | How to render a 'Log' as a line of text.
data LineRenderer level path msg
  = LineRendererUtf8 !(Bool -> Log level path msg -> BB.Builder)
  -- ^ The returned bytes must not contain a leading nor trailing newline.
  --
  -- The 'Bool' tells whether we are trying to write these bytes to a
  -- terminal that supports ANSI colors.

-- | How to render a 'Log' as a binary blob.
data BlobRenderer level path msg
  = BlobRenderer !(Log level path msg -> BB.Builder)

--------------------------------------------------------------------------------

-- | Like 'blob', but each 'Log' is rendered as text in its own line.
--
-- If the given 'IO.Handle' is associated to a TTY supporting ANSI colors, and
-- the given 'LineRenderer' supports rendering with colors, and you ask for
-- it, then you will get colorful output.
handle
  :: (MonadIO m, MonadIO n)
  => Maybe Bool
  -- ^ Whether to render with colors.
  --
  -- If 'Nothing', then we'll render with ANSI colors when the given
  -- 'IO.Handle' is a TTY. You probably want to use 'Nothing' here most of the
  -- time.
  -> IO.Handle  -- ^ Handle where to write 'Log's.
  -> LineRenderer level path msg -- ^ How to render each 'Log'.
  -> m (Log level path msg -> n ())
handle ywantColors h (LineRendererUtf8 render0) = liftIO $ do
  wantColors <- maybe (isTty h) pure ywantColors
  let !render1 = render0 wantColors
      !newline = BB.char7 '\n'
      render2 = \log' -> render1 log' <> newline
  blob h (BlobRenderer render2)

-- | Write 'Log's to a 'IO.Handle' as a binary blob.
blob
  :: (MonadIO m, MonadIO n)
  => IO.Handle -- ^ Handle where to write 'Log's.
  -> BlobRenderer level path msg -- ^ How to render each 'Log'.
  -> m (Log level path msg -> n ())
blob h (BlobRenderer render) = liftIO $ do
  IO.hSetBinaryMode h True
  pure $ \log_ ->
     liftIO (Ex.finally (BB.hPutBuilder h (render log_))
                        (IO.hFlush h))

-- | 'Log's are written to 'IO.stderr', one per line.
--
-- /WARNING/ Currently this always renders as UTF-8.
stderr
  :: (MonadIO m, MonadIO n)
  => LineRenderer level path msg -- ^ How to render each 'Log' line.
  -> m (Log level path msg -> n ())
stderr = handle Nothing IO.stderr

--------------------------------------------------------------------------------

isTty :: IO.Handle -> IO Bool
#ifdef VERSION_unix
isTty h
  | h == IO.stderr = q System.Posix.IO.stdError
  | h == IO.stdout = q System.Posix.IO.stdOutput
  | h == IO.stdin  = q System.Posix.IO.stdInput
  | otherwise = pure False  -- Is this good enough?
  where q = System.Posix.Terminal.queryTerminal
#else
isTty _ = pure False
#endif

