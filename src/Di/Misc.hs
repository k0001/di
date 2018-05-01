{-# LANGUAGE ScopedTypeVariables #-}

-- | Assorted utility functions.
module Di.Misc
 ( catchSync
 , muteSync
 , mute
 , getSystemTimeSTM
 , iterateM
 ) where

import Control.Concurrent.STM (STM)
import qualified Control.Exception as Ex
import qualified Data.Time.Clock.System as Time
import GHC.Conc (unsafeIOToSTM)

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

