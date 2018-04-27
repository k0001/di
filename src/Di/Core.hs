{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Di.Core
 ( new

 , MonadDi(ask, local)

 , DiT(DiT, unDiT)
 , runDiT

 , flush
 , push
 , attr
 , max

 , log
 , emergency
 , alert
 , critical
 , error
 , warning
 , notice
 , info
 , debug


 , test
 ) where

import Control.Applicative (Alternative)
import Control.Concurrent (forkFinally, myThreadId)
import Control.Concurrent.STM
import qualified Control.Exception as Ex
import Control.Monad.Cont (MonadCont)
import Control.Monad.Error (MonadError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus, when)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader)
import qualified Control.Monad.Reader as Reader (local, ask)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Function (fix)
import Data.Monoid ((<>))
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import Data.Word (Word64)
import Prelude hiding (log, max, error)
import qualified System.IO as IO

import Di.Df1 (df1)
import Di.Misc (renderIso8601, catchSync)
import Di.Types
  (Log(Log, logTime, logLevel, logPath, logMessage),
   Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
   Path(Attr, Push, Root),
   Di(Di, diMax, diPath, diLogs),
   Writer(Writer, unWriter))
import qualified Di.Writer (stderr)


--------------------------------------------------------------------------------

-- | Build a new 'Di' from a logging function.
--
-- /Note:/ If the passed in 'IO' function throws a exception, it will be
-- just logged to 'IO.stderr' and then ignored.
--
-- /Note:/ There's no need to "release" the obtained 'Di'.
new
  :: MonadIO m
  => String -- ^ Root path name.
  -> Writer
  -> m Di -- ^
new name writer = liftIO $ do
   me <- myThreadId
   tqLogs :: TQueue Log <- newTQueueIO
   writeFallback :: Log -> IO () <- unWriter (Di.Writer.stderr Di.Df1.df1)
   write :: Log -> IO () <- unWriter writer
   _ <- forkFinally (worker writeFallback write tqLogs)
                    (either (Ex.throwTo me) pure)
   pure (Di Info (Root (TL.pack name)) tqLogs)
 where
   worker :: (Log -> IO ()) -> (Log -> IO ()) -> TQueue Log -> IO ()
   worker writeFallback write tqLogs = fix $ \k -> do
     eio <- Ex.try $ atomically $ do
        log' :: Log <- peekTQueue tqLogs
        pure (log', Ex.finally (write log') (atomically (readTQueue tqLogs)))
     case eio of
        Left (_ :: Ex.BlockedIndefinitelyOnSTM) -> do
           pure ()  -- Nobody writes to '_diLogs' anymore, so we can just stop.
        Right (log', io) -> do
           catchSync io $ \se -> do
              -- Fallback exception logging to stderr.
              syst <- Time.getSystemTime
              writeFallback $ log'
                 { logTime = syst
                 , logLevel = Error
                 , logPath = Attr "exception" (TL.pack (show se)) (logPath log')
                 , logMessage = "Got synchronous exception in Di Writer. The \
                                \Log that couldn't be written will be rendered \
                                \here afterwards, using the fallback Di Writer."
                 }
              writeFallback log'
           k

-- | Log a message with the given importance @level@.
--
-- This function returns immediately after queing the message for logging in a
-- different thread. If you want to explicitly wait for the message to be
-- logged, then call 'flush' afterwards.
--
-- /Note:/ No exceptions from the underlying logging backend (i.e., the 'IO'
-- action given to 'new') will be thrown from 'log'. Instead, those will be
-- recorded to 'IO.stderr' and ignored.
log :: (MonadDi m, MonadIO m) => Level -> TL.Text -> m ()
log l = \m -> ask >>= \di ->
   when (l >= diMax di) $ liftIO $ do
      !syst <- Time.getSystemTime
      let !x = Log syst l (diPath di) m
      atomically $ writeTQueue (diLogs di) x
{-# INLINABLE log #-}

emergency :: (MonadDi m, MonadIO m) => TL.Text -> m ()
emergency = log Emergency
{-# INLINE emergency #-}

alert :: (MonadDi m, MonadIO m) => TL.Text -> m ()
alert = log Alert
{-# INLINE alert #-}

critical :: (MonadDi m, MonadIO m) => TL.Text -> m ()
critical = log Critical
{-# INLINE critical #-}

error :: (MonadDi m, MonadIO m) => TL.Text -> m ()
error = log Error
{-# INLINE error #-}

warning :: (MonadDi m, MonadIO m) => TL.Text -> m ()
warning = log Warning
{-# INLINE warning #-}

notice :: (MonadDi m, MonadIO m) => TL.Text -> m ()
notice = log Notice
{-# INLINE notice #-}

info :: (MonadDi m, MonadIO m) => TL.Text -> m ()
info = log Info
{-# INLINE info #-}

debug :: (MonadDi m, MonadIO m) => TL.Text -> m ()
debug = log Debug
{-# INLINE debug #-}

-- | Block until all messages being logged have finished processing.
--
-- Manually calling 'flush' is not usually necessary, but, if at some point you
-- want to ensure that all messages logged until then have properly rendered to
-- the underlying backend, then 'flush' will block until that happens.
flush :: (MonadDi m, MonadIO m) => m ()
flush = ask >>= \di -> liftIO (atomically (check =<< isEmptyTQueue (diLogs di)))
{-# INLINABLE flush #-}

-- | Returns a new 'Di' on which only messages with at least the specified
-- 'Level' of importance are ever logged.
--
-- For example, @'max' 'Notice'@ will prevent messages with 'Level's 'Info'
-- and 'Debug' from ever being loged through the resulting 'Di'.
--
-- Notice that a use of 'max' won't impact subsequent uses of it. In practical
-- terms, this implies that an exclusion of 'Level's of lower importance can be
-- fully reverted by using 'max' again, giving it a lower importance 'Level' as
-- argument. This can be useful in case you want to temporarily mute some
-- “unimporant” messages. More generally:
--
-- @
-- forall a b.
--    'max' a . 'max' b . 'max' a  ==  'id'
-- @
max :: MonadDi m => Level -> m a -> m a
max !l m = local (\di -> di { diMax = l }) m
{-# INLINE max #-}

-- | Push a new @path@ to the 'Di'.
--
-- Identity:
--
-- @
-- 'push' 'mempty'   ==   'id'
-- @
--
-- Composition:
--
-- @
-- 'push' (a <> b)   ==   'push' b . 'push' a
-- @
push :: MonadDi m => T.Text -> m a -> m a
{-# INLINE push #-}
push x m = local (\di ->
  di { diPath = Push (TL.fromStrict x) (diPath di) }) m

attr :: MonadDi m => T.Text -> T.Text -> m a -> m a
{-# INLINE attr #-}
attr k v m = local (\di ->
  di { diPath = Attr (TL.fromStrict k) (TL.fromStrict v) (diPath di) }) m

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Semantically, @'MonadDi' m@ is equivalent to @'MonadReader' 'Di' m@. All of
-- the laws that apply to @forall r. 'MonadReader' r m@, apply to @'MonadDi' m@
-- as well.
--
-- This class only exists so that @forall r. 'MonadReader' r m@ can be used
-- alongside @'MonadDi' m@ at the same time.
class Monad m => MonadDi m where
  ask :: m Di
  local :: (Di -> Di) -> m a -> m a

instance MonadDi ((->) Di) where
  ask = id
  {-# INLINE ask #-}
  local f m = m . f
  {-# INLINE local #-}

--------------------------------------------------------------------------------

newtype DiT m a = DiT { unDiT :: ReaderT Di m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadIO,
            MonadFail, MonadFix, MonadZip, MonadPlus, MonadCont,
            MonadError e, MonadState s)

runDiT :: DiT m a -> Di -> m a
runDiT (DiT (ReaderT f)) = f
{-# INLINE runDiT #-}

instance MonadReader r m => MonadReader r (DiT m) where
  ask = DiT (ReaderT (\_ -> Reader.ask))
  {-# INLINABLE ask #-}
  local f (DiT (ReaderT gma)) = DiT (ReaderT (\di -> Reader.local f (gma di)))
  {-# INLINABLE local #-}

instance Monad m => MonadDi (DiT m) where
  ask = DiT (ReaderT (\di -> pure di))
  {-# INLINE ask #-}
  local f (DiT (ReaderT gma)) = DiT (ReaderT (\di -> gma (f di)))
  {-# INLINE local #-}

--------------------------------------------------------------------------------

test :: (MonadIO m, MonadDi m) => m ()
test = do
  max Debug $ do
    notice "Running `test`"
    debug "attr a"
    attr "a" "b" $ do
      debug "attr c"
      attr "c" "d" $ do
        debug "attr a again"
        attr "a" "e" $ do
          debug "push z"
          push "z" $ do
            debug "attr f"
            attr "f" "g" $ do
              debug "max Warning"
              max Warning $ do
                debug "IF YOU SEE THIS MESSAGE max DOESN'T WORK"
                info "IF YOU SEE THIS MESSAGE max DOESN'T WORK"
                notice "IF YOU SEE THIS MESSAGE max DOESN'T WORK"
                warning "you should see this message"
                error "you should see this message"
                critical "you should see this message"
                emergency "you should see this message"
                warning "max Debug"
                max Debug $ do
                  debug "testing all levels"
                  info "testing all levels"
                  notice "testing all levels"
                  warning "testing all levels"
                  error "testing all levels"
                  critical "testing all levels"
                  alert "testing all levels"
                  emergency "testing all levels"
