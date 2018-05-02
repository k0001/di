{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Assorted utility functions.
module Di.Misc
 ( catchSync
 , muteSync
 , mute
 , getSystemTimeSTM
 , iterateM
 , drainProducer
 , lin
 ) where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B
import qualified Control.Exception as Ex
import Data.Function (fix)
import qualified Data.Time.Clock.System as Time
import GHC.Conc (unsafeIOToSTM)
import qualified Pipes
import qualified Pipes.ByteString as Pb


--------------------------------------------------------------------------------

-- | Discards all output from a 'P.Producer'.
drainProducer :: Monad m => Pipes.Producer a m r -> m r
{-# INLINE drainProducer #-}
drainProducer p0 = either pure (drainProducer . snd) =<< Pipes.next p0


-- | Breaks input into lines delimited by either CR, LF, or CRLF.
lin
  :: Monad m
  => Pipes.Producer Pb.ByteString m r
  -> Pipes.Producer Pb.ByteString m (Pipes.Producer Pb.ByteString m r)
{-# INLINABLE lin #-}
lin = \p0 ->
  lift (nextSkipEmpty p0) >>= \case
    Left r -> pure (pure r)
    Right (b0, p1) -> do
      -- Search for CR
      case B.break (==13) b0 of
        (b1pre, b1pos)
           | B.null b1pos -> do
             -- No CR found. Search for LF.
             case B.break (==10) b1pre of
               (b2pre, b2pos)
                 | B.null b2pos -> do
                   -- No LF found. Yield everything and loop.
                   Pipes.yield b2pre >> lin p1
                 | otherwise -> do
                   -- LF found. Yield prefix and stop.
                   Pipes.yield b2pre
                   pure (Pipes.yield (B.tail b2pos) >> p1)
           | otherwise -> do
             -- CR found, but before deciding this is our line ending
             -- we need to check whether we skipped a LF in b1pre.
             case B.break (==10) b1pre of
               (b2pre, b2pos)
                 | B.null b2pos -> do
                    -- No LF found in b1pre. CR is the start of our new
                    -- newline ending. Yield prefix.
                    Pipes.yield b1pre
                    -- Skip a LF in b1pos, if any.
                    case B.tail b1pos of
                      b3pos
                        | B.null b3pos -> do
                          -- We ran out of bytes. Get more.
                          lift (nextSkipEmpty p1) >>= \case
                            Left r -> pure (pure r)
                            Right (b4, p2)
                              | B.head b4 == 10 -> do
                                -- We found LF. CRLF is our line ending. Stop.
                                pure (Pipes.yield (B.tail b4) >> p2)
                              | otherwise -> do
                                -- No LF found. CR is our line ending. Stop.
                                pure (Pipes.yield b4 >> p2)
                        | B.head b3pos == 10 -> do
                          -- We found LF. CRLF is our line ending. Stop.
                          pure (Pipes.yield (B.tail b3pos) >> p1)
                        | otherwise -> do
                          -- No LF found. CR is our line ending. Stop.
                          pure (Pipes.yield b3pos >> p1)
                 | otherwise -> do
                   -- LF found in b1pre. LF is our line ending. Yield prefix
                   -- and stop.
                   Pipes.yield b2pre
                   pure (Pipes.yield (B.tail b2pos) >> p1)


-- | Like 'Pipes.next', except it skips leading empty chunks.
nextSkipEmpty
  :: Monad m
  => Pipes.Producer B.ByteString m r
  -> m (Either r (B.ByteString, Pipes.Producer B.ByteString m r))
{-# INLINABLE nextSkipEmpty #-}
nextSkipEmpty = fix $ \k p0 -> do
  x <- Pipes.next p0
  case x of
     Left _ -> pure x
     Right (bs, p1)
        | B.null bs -> k p1
        | otherwise -> pure x

--------------------------------------------------------------------------------

iterateM :: Monad m => (a -> m a) -> a -> m [a]
{-# INLINE iterateM #-}
iterateM f a = f a >>= \a' -> iterateM f a' >>= \as' -> pure (a' : as')

--------------------------------------------------------------------------------

getSystemTimeSTM :: STM Time.SystemTime
{-# INLINE getSystemTimeSTM #-}
getSystemTimeSTM = unsafeIOToSTM Time.getSystemTime

--------------------------------------------------------------------------------

-- | @'catchSync' m f@ runs @m@, and in case of /synchronous/ exceptions, calls
-- @f@ with said exception as argument. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'catchSync'. Notice that
-- neither synchronous nor asynchronous exceptions thrown by @f@ are handled.
catchSync :: IO a -> (Ex.SomeException -> IO a) -> IO a
{-# INLINE catchSync #-}
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwIO (ae :: Ex.AsyncException)
   Nothing -> f se

-- | @'muteSync' m@ runs @m@, and in case of /synchronous/ exceptions, it
-- ignores them and returns @()@. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'muteSync'.
muteSync :: IO () -> IO ()
{-# INLINE muteSync #-}
muteSync m = catchSync m (\_ -> pure ())

-- | @'mute' m@ runs @m@, and in case of either /synchronous/ or /asynchronous/
-- exceptions, it ignores them and returns @()@. Most of the times this is not
-- what you want.
mute :: IO () -> IO ()
{-# INLINE mute #-}
mute m = Ex.catch m (\(_ :: Ex.SomeException) -> pure ())

