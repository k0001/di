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
 , splitter
 , splitterBytes
 , line
 , minga
 ) where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Free (FreeT(FreeT), FreeF(Pure, Free))
import qualified Data.ByteString as B
import qualified Control.Exception as Ex (AsyncException, asyncExceptionFromException)
import qualified Control.Monad.Catch as Ex
import Data.Function (fix)
import qualified Data.Time.Clock.System as Time
import GHC.Conc (unsafeIOToSTM)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as Pb

--------------------------------------------------------------------------------

-- | Discards all output from a 'P.Producer'.
drainProducer :: Monad m => P.Producer a m r -> m r
{-# INLINE drainProducer #-}
drainProducer p0 = either pure (drainProducer . snd) =<< P.next p0

minga
  :: Monad m
  => P.Producer B.ByteString m r
  -> m [B.ByteString]
minga p0 = do
  (bs0, p1) <- P.toListM' (line p0)
  let b0 = B.concat bs0
  Pb.nextByte p1 >>= \case
     Left _ -> pure [b0]
     Right (w, p2) -> do
        bs <- minga (P.yield (B.singleton w) >> p2)
        pure (b0 : bs)

-- | Given a function that splits a 'P.Producer' into a prefix and some
-- leftovers, return a 'FreeT' splitter with the same splitting semantics.
splitter
  :: Monad m
  => (P.Producer a m r -> P.Producer a m (P.Producer a m r))
  -> (P.Producer a m r -> FreeT (P.Producer a m) m r) -- ^
{-# INLINABLE splitter #-}
splitter f = \pr0 -> FreeT (do
   P.next pr0 >>= \case
      Left r -> pure (Pure r)
      Right (a, pr1) -> pure (Free (do
         pp0 <- f (P.yield a >> pr1)
         pure (splitter f pp0) )))

-- | This is like 'splitter', but it will avoid attempting to split empty
-- chunks, which is always desirable. On the other hand, 'splitter' is
-- suceptible to feeding empty chunks to your spliting function.
splitterBytes
  :: Monad m
  => (P.Producer B.ByteString m r
        -> P.Producer B.ByteString m (P.Producer B.ByteString m r))
  -> (P.Producer B.ByteString m r
        -> FreeT (P.Producer B.ByteString m) m r) -- ^
{-# INLINABLE splitterBytes #-}
splitterBytes f = \pr0 -> FreeT (do
   nextBytes pr0 >>= \case
      Left r -> pure (Pure r)
      Right (a, pr1) -> pure (Free (do
         pp0 <- f (P.yield a >> pr1)
         pure (splitterBytes f pp0) )))


-- | Breaks input into lines delimited by either /CR/, /LF/, or /CRLF/.
--
-- The obtained 'P.Producer' yields chunks of the first line, and returns
-- and returns any leftovers afterwards.
--
-- The trailing /CR/, /LF/, or /CRLF/ bytes will be removed from the
-- 'P.Producer' output, and they will also be removed from the beginning of the
-- returned 'P.Producer'.
--
-- It is possible that the line itself is empty, in which case the obtained
-- 'P.Producer' will yield an empty chunk before returning the leftovers.
line
  :: Monad m
  => P.Producer Pb.ByteString m r
  -> P.Producer Pb.ByteString m (P.Producer Pb.ByteString m r)
{-# INLINABLE line #-}
line = \p0 -> do
  lift (nextBytes p0) >>= \case
    Left r -> pure (pure r)
    Right (b0, p1) -> do -- Search for CR
      case B.break (==13) b0 of
        (b1pre, b1pos)
           | B.null b1pos -> do -- No CR found. Search for LF.
             case B.break (==10) b1pre of
               (b2pre, b2pos)
                 | B.null b2pos -> do -- No LF found. Yield and loop.
                   P.yield b2pre
                   line p1
                 | otherwise -> do -- LF found. Yield prefix and stop.
                   P.yield b2pre
                   pure (P.yield (B.tail b2pos) >> p1)
           | otherwise -> do
             -- CR found, but before deciding this is our line ending
             -- we need to check whether we skipped a LF in b1pre.
             case B.break (==10) b1pre of
               (b2pre, b2pos)
                 | B.null b2pos -> do
                    -- No LF found in b1pre. CR is the start of our new
                    -- newline ending. Yield prefix.
                    P.yield b1pre
                    -- See if an LF follows CR.
                    case B.tail b1pos of
                      b3pos
                        | B.null b3pos -> do -- We ran out of bytes. Get more.
                          lift (nextBytes p1) >>= \case
                            Left r -> pure (pure r)
                            Right (b4, p2)
                              | B.head b4 == 10 -> do
                                -- We found LF. CRLF is our line ending. Stop.
                                pure (P.yield (B.tail b4) >> p2)
                              | otherwise -> do
                                -- No LF found. CR is our line ending. Stop.
                                pure (P.yield b4 >> p2)
                        | B.head b3pos == 10 -> do
                          -- We found LF. CRLF is our line ending. Stop.
                          pure (P.yield (B.tail b3pos) >> p1)
                        | otherwise -> do
                          -- No LF found. CR is our line ending. Stop.
                          pure (P.yield b3pos >> p1)
                 | otherwise -> do
                   -- LF found. It is our line ending. Yield prefix and stop.
                   P.yield b2pre
                   pure (P.yield (B.tail b2pos) >> P.yield b1pos >> p1)


-- | Like 'P.next', except it skips leading empty chunks, guaranteeing than a
-- 'Right' result is never 'B.null'.
nextBytes
  :: Monad m
  => P.Producer B.ByteString m r
  -> m (Either r (B.ByteString, P.Producer B.ByteString m r))
{-# INLINABLE nextBytes #-}
nextBytes = fix $ \k p0 -> do
  x <- P.next p0
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
catchSync :: Ex.MonadCatch m => m a -> (Ex.SomeException -> m a) -> m a
{-# INLINE catchSync #-}
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwM (ae :: Ex.AsyncException)
   Nothing -> f se

-- | @'muteSync' m@ runs @m@, and in case of /synchronous/ exceptions, it
-- ignores them and returns @()@. In case @m@ throwed an /asynchronous/
-- exception, then that exception is simply rethrown by 'muteSync'.
muteSync :: Ex.MonadCatch m => m () -> m ()
{-# INLINE muteSync #-}
muteSync m = catchSync m (\_ -> pure ())

-- | @'mute' m@ runs @m@, and in case of either /synchronous/ or /asynchronous/
-- exceptions, it ignores them and returns @()@. Most of the times this is not
-- what you want.
mute :: Ex.MonadCatch m => m () -> m ()
{-# INLINE mute #-}
mute m = Ex.catch m (\(_ :: Ex.SomeException) -> pure ())

--------------------------------------------------------------------------------
