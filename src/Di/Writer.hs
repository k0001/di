{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Di.Writer
 ( stderr
 , handle
 ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Time.Clock.System as Time
import Prelude hiding (log, filter)
import qualified System.IO as IO

#ifdef VERSION_unix
import qualified System.Posix.Terminal
import qualified System.Posix.IO
#endif

import Di.Types (Writer(Writer))
import qualified Di.Df1

--------------------------------------------------------------------------------

-- | Log lines are written to 'IO.Handle' using the 'IO.Handle''s locale
-- encoding.
handle :: IO.Handle -> Writer
handle h = Writer $ do
  render <- isTty h >>= \case
     True -> pure Di.Df1.renderLogColor
     False -> undefined
  IO.hSetBuffering h IO.LineBuffering
  pure $ \x -> do
     TL.hPutStrLn h (TB.toLazyText (render x))
     IO.hFlush h

-- | Log lines are written to 'IO.stderr' using 'IO.stderr''s locale encoding.
stderr :: Writer
stderr = handle IO.stderr

--------------------------------------------------------------------------------

#ifdef VERSION_unix
isTty :: IO.Handle -> IO Bool
isTty h
  | h == IO.stderr = q System.Posix.IO.stdError
  | h == IO.stdout = q System.Posix.IO.stdOutput
  | otherwise = pure False
    -- We should convert h to a Fd and give it q. Not sure how to do that.
  where q = System.Posix.Terminal.queryTerminal
#else
isTty :: IO.Handle -> IO Bool
isTty _ = pure False
#endif
