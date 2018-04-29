{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
--
-- Import this module __qualified__ as follows:
--
-- @
-- import qualified Di
-- @
--
-- The /only/ name that you are encouraged to import unqualified is 'MonadDi':
--
-- @
-- import Di (MonadDi)
-- @


module Di
 ( Di
 , new
 , new'

   -- * Sinks
 , df1
 , stderrLines
 , handleLines

   -- * 'DiT'
 , DiT
 , runDiT
 , runDiT'

   -- * 'MonadDi'
 , MonadDi(local, ask, liftSTM)

   -- ** Messages
 , log
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)

 , emergency
 , alert
 , critical
 , error
 , warning
 , notice
 , info
 , debug
   -- ** Meta
 , push
 , attr
 , max
 , flush

   -- * Extras
   --
   -- These types are not particularly important if all you want to do is log
   -- messages. However, if you are trying to extend this library somehow, you
   -- will need these.
 , Log(Log, logTime, logLevel, logPath, logMessage)
 , Path(Attr, Push, Root)
 , pathRoot
 , Sink(Sink)
 , withSink
 , sinkFallback

 , test
 ) where


import Prelude hiding (error, max, log)

import Control.Applicative (Alternative)
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
  (STM, atomically, check,
   TQueue, newTQueueIO, readTQueue, writeTQueue, peekTQueue, isEmptyTQueue)
import qualified Control.Exception as Ex
import Control.Monad.Cont (MonadCont, ContT(ContT))
import Control.Monad.Error (MonadError, ErrorT(ErrorT), Error)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Identity (IdentityT(IdentityT))
import Control.Monad.List (ListT(ListT))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus, when)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader)
import qualified Control.Monad.Reader as Reader (local, ask)
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import Control.Monad.State (MonadState)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Control.Monad.Zip (MonadZip)
import Data.Function (fix)
import qualified Data.Time.Clock.System as Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Di.Df1 (df1)
import Di.Misc (mute, muteSync, catchSync, getSystemTimeSTM)
import Di.Sink
  (Sink(Sink), withSink, stderrLines, handleLines, sinkFallback, withSink)
import Di.Types
  (Log(Log, logTime, logLevel, logPath, logMessage),
   Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
   Path(Attr, Push, Root), pathRoot,
   Di(Di, diMax, diPath, diLogs))
-- TODO specify lazy semantics for Log fields

--------------------------------------------------------------------------------

-- | Create a new 'Di' that writes to 'System.IO.stderr', one log message per
-- line.
--
-- @
-- 'new' name  ==  'new'' name ('stderrLines' 'df1')
-- @
--
-- You are supposed to call 'new' _only once per application_, and this one 'Di'
-- shall be used throughout your application, even concurrently. This ensures
-- that the order of logged messages is preserved, and that 'System.IO.stderr'
-- doesn't get garbled with text comming from different sources.
--
-- @
-- main :: 'IO' ()
-- main = 'Di.new' $ \di -> do
--    something ...
-- @
--
-- /WARNING:/ If you call 'new' more than once, you'll __get an exception__.
--
-- /WARNING:/ Library code should never call 'new', instead, it should rely
-- solely on 'MonadDi' and expect the caller to provide a 'Di' somehow. If
-- library needs to obtain a concrete 'Di', for example, to call 'runDiT' from
-- a different thread, then it could get that 'Di' using @'Di.ask' :: 'MonadDi'
-- m => m 'Di'@, or simply receive as an argument.
new :: String -> (Di -> IO a) -> IO a
new name act = new' name (stderrLines df1) act

new'
  :: String -- ^ Root path name.
  -> Sink
  -> (Di -> IO a)
  -> IO a
new' name sink act =
  withSink sink $ \write -> do
    tqLogs :: TQueue Log <- newTQueueIO
    -- Start worker thread, restarting in in case of sync exceptions (unlikely).
    _ <- fix $ \k -> forkFinally (worker write tqLogs) $ \case
       Right () -> pure () -- All messages processed and nobody cares anymore.
       Left se -> case Ex.asyncExceptionFromException se of
          Just (_ :: Ex.AsyncException) -> Ex.throwIO se
          Nothing -> k >> pure ()
    let di = Di Info (Root (TL.pack name)) tqLogs
    -- Run 'act', silently flushing and logging any unhandled synchronous
    -- exceptions afterwards.
    flip Ex.finally (mute (atomically (flushDi di))) $
      catchSync (act di) $ \se -> do
         syst <- Ex.onException (Time.getSystemTime) (Ex.throw se)
         -- We mute because it doesn't really matter if this fails.
         mute $ atomically $ writeTQueue tqLogs $ Log
           { logTime = syst, logLevel = Alert
           , logPath = Attr "exception"
               (TL.pack (Ex.displayException se)) (diPath di)
           , logMessage = "Unhandled exception. \
                          \Logging system will stop now." }
         Ex.throw se
  where
    worker :: (Log -> IO ()) -> TQueue Log -> IO ()
    worker write tqLogs = fix $ \k -> do
      -- The actual writting of the log message is performed by 'io' later.
      -- Here we just try to build an IO action that when performed will write
      -- the log message and remove it from 'tqLogs' afterwards. This is why
      -- 'flush' works by just checking if 'tqLogs' is empty.
      eio <- Ex.try $ atomically $ do
         log' :: Log <- peekTQueue tqLogs
         pure (Ex.finally (write log')
                          (atomically (readTQueue tqLogs)))
      case eio of
         Left (_ :: Ex.BlockedIndefinitelyOnSTM) -> do
            -- Nobody writes to 'tqLogs' anymore, so we can just stop.
            pure ()
         Right io -> do
            -- Finally we run the IO action that commits the log message to the
            -- outside world. Notice that we mute synchronous exceptions because
            -- 'sink' already includes a fallback printing mechanism, and if
            -- that fallback fails there's not much else we could do. So we just
            -- mute synchronous exceptions and move on to the next iteration.
            muteSync io >> k

-- | See the docs for 'flush'.
flushDi :: Di -> STM ()
{-# INLINE flushDi #-}
flushDi di = check =<< isEmptyTQueue (diLogs di)

-- | Log a message with the given importance @level@.
--
-- This function returns immediately after queing the message for logging. The
-- actual printing of the log message will happen in a different thread,
-- asynchronously. If you want to explicitly wait for the message to be logged,
-- then call 'flush' afterwards.
--
-- Log messages are rendered in FIFO order, and their timestamp records the time
-- when this 'log' function was called (rather than the time when the log
-- message is printed in the future).
--
-- /Note regarding exceptions:/ As witnessed by the 'MonadDi' context, the only
-- exceptions that could ever be thrown by this function are those that
-- 'liftSTM' could. Synchronous exceptions that happen due to failures in the
-- actual priting of the log message are handled in a different by attempting to
-- log the message to 'IO.stderr' as a fallback. Asynchronous exceptions that
-- might there, as expected, are not explicitly handled. In practical terms,
-- this means that unless you know what you are doing, /you should
-- just call 'log' without worrying about it ever throwing exceptions/.
log :: MonadDi m => Level -> TL.Text -> m ()
log l = \m -> ask >>= \di ->
   when (l >= diMax di) $ do
      !syst <- liftSTM getSystemTimeSTM
      let !x = Log syst l (diPath di) m
      liftSTM $ writeTQueue (diLogs di) x
{-# INLINABLE log #-}

-- | @
-- 'emergency' == 'log' 'Emergency'
-- @
emergency :: MonadDi m => TL.Text -> m ()
emergency = log Emergency
{-# INLINE emergency #-}

-- | @
-- 'alert' == 'log' 'Alert'
-- @
alert :: MonadDi m => TL.Text -> m ()
alert = log Alert
{-# INLINE alert #-}

-- | @
-- 'critical' == 'log' 'Critical'
-- @
critical :: MonadDi m => TL.Text -> m ()
critical = log Critical
{-# INLINE critical #-}

-- | @
-- 'error' == 'log' 'Error'
-- @
error :: MonadDi m => TL.Text -> m ()
error = log Error
{-# INLINE error #-}

-- | @
-- 'warning' == 'log' 'Warning'
-- @
warning :: MonadDi m => TL.Text -> m ()
warning = log Warning
{-# INLINE warning #-}

-- | @
-- 'notice' == 'log' 'Notice'
-- @
notice :: MonadDi m => TL.Text -> m ()
notice = log Notice
{-# INLINE notice #-}


-- | @
-- 'info' == 'log' 'Info'
-- @
info :: MonadDi m => TL.Text -> m ()
info = log Info
{-# INLINE info #-}

-- | @
-- 'debug' == 'log' 'Debug'
-- @
debug :: MonadDi m => TL.Text -> m ()
debug = log Debug
{-# INLINE debug #-}

-- | Block until all messages being logged have finished processing.
--
-- Manually calling 'flush' is not usually necessary because all log messages
-- are processed as soon as possible, and 'with' ensures that no log message is
-- left unprocessed. However, the actual printing of log messages happens
-- asynchronously, meaning there might be log messages still waiting to be
-- processed. A call to 'flush' will block until all pending log messages have
-- been processed.
flush :: MonadDi m => m ()
flush = ask >>= \di -> liftSTM (flushDi di)
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

-- | A 'MonadDi' abstracts over the requirements for logging with 'Di' through a
-- monadic interface.
--
-- All of the functions intended to generate new log messages or metadata, such
-- as 'push' or 'debug', rely on 'MonadDi'.
--
-- Semantically, @'MonadDi' m@ is a “reader monad” that carries as its
-- environment a 'Di' and a natural transformation used to lift 'STM'
-- actions to @m@.
class Monad m => MonadDi m where
  -- | Get the 'Di' inside @m@, unmodified.
  --
  -- Idempotence law:
  --
  -- @
  -- 'ask' '>>' 'ask'  ==  'ask'
  -- @
  ask :: m Di
  default ask :: (MonadTrans t, MonadDi n, m ~ t n) => m Di
  ask = lift ask
  {-# INLINE ask #-}

  -- | Run @m a@ with a modified 'Di':
  --
  -- @
  -- 'local' ('const' x) 'ask'  ==  'pure' x
  -- @
  --
  -- Identity law:
  --
  -- @
  -- 'local' 'id' 'ask'  ==  'ask'
  -- @
  --
  -- Distributive law:
  --
  -- @
  -- 'local' f '.' 'local' g  ==  'local' (f '.' g)
  -- @
  --
  -- Idempotence law:
  --
  -- @
  -- 'local' f ('pure' ()) '>>=' 'ask'  ==  'ask'
  -- @
  local :: (Di -> Di) -> m a -> m a

  -- | Natural transformation from 'STM' to @m@.
  liftSTM :: STM a -> m a
  default liftSTM :: (MonadTrans t, MonadDi n, m ~ t n) => STM a -> m a
  liftSTM = \x -> lift (liftSTM x)
  {-# INLINE liftSTM #-}

--------------------------------------------------------------------------------

-- | Natural transformation from @f@ to @g@.
newtype H f g = H (forall x. f x -> g x)

newtype DiT m a = DiT (ReaderT (Di, H STM m) m a)
  deriving newtype
    (Functor, Applicative, Alternative, Monad, MonadIO,
     MonadFail, MonadFix, MonadZip, MonadPlus, MonadCont,
     MonadError e, MonadState s, MonadWriter w)

instance MonadTrans DiT where
  lift = \x -> DiT (lift x)
  {-# INLINE lift #-}

-- | Run a 'DiT'.
runDiT :: MonadIO m => Di -> DiT m a -> m a
runDiT = runDiT' (\x -> liftIO (atomically x))
{-# INLINE runDiT #-}

-- | Like 'runDiT'', however it doesn't require a 'MonadIO' constraint. Instead,
-- it requires the natural transformation that will be used as 'liftSTM'.
--
-- First, this allows any monad that wraps 'IO' without necessarily having a
-- 'MonadIO' instance to work with 'MonadDi'. For example:
--
-- @
-- newtype Foo = Foo ('IO' a)
--   deriving ('Functor', 'Applicative', 'Monad')
--
-- 'runDiT'' (Foo . 'atomically')
--      :: 'Di' -> 'DiT' Foo a -> Foo a
-- @
--
-- Second, this allows @m@ to be 'STM' itself:
--
-- @
-- 'runDiT'' 'id'
--      :: 'Di' -> 'DiT' 'STM' a -> 'STM' a
-- @
--
-- The semantics of logging from within 'STM' are those of any other 'STM'
-- transaction: That is, log messages are only ever commited once to the outside
-- world if and when the 'STM' transaction succeeds, and not before.
--
-- Of course, 'runDiT'' works as well if you decide to wrap 'STM' with your own
-- monad type:
--
-- @
-- newtype Bar = Bar ('STM' a)
--   deriving ('Functor', 'Applicative', 'Monad')
--
-- 'runDiT'' Bar
--      :: 'Di' -> 'DiT' Bar a -> Bar a
-- @
runDiT' :: (forall x. STM x -> m x) -> Di -> DiT m a -> m a
runDiT' h = \di -> \(DiT (ReaderT f)) -> f (di, H h)
{-# INLINE runDiT' #-}

instance MonadReader r m => MonadReader r (DiT m) where
  ask = DiT (ReaderT (\_ -> Reader.ask))
  {-# INLINABLE ask #-}
  local f (DiT (ReaderT gma)) = DiT (ReaderT (\di -> Reader.local f (gma di)))
  {-# INLINABLE local #-}

instance Monad m => MonadDi (DiT m) where
  ask = DiT (ReaderT (\(di,_) -> pure di))
  {-# INLINE ask #-}
  local f (DiT (ReaderT gma)) = DiT (ReaderT (\(di, h) -> gma (f di, h)))
  {-# INLINE local #-}
  liftSTM = \x -> DiT (ReaderT (\(_, H h) -> h x))
  {-# INLINE liftSTM #-}

--------------------------------------------------------------------------------

instance MonadDi m => MonadDi (ReaderT r m) where
  local f (ReaderT gma) = ReaderT (\r -> local f (gma r))
  {-# INLINE local #-}

instance MonadDi m => MonadDi (SS.StateT s m) where
  local f (SS.StateT gma) = SS.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance MonadDi m => MonadDi (SL.StateT s m) where
  local f (SL.StateT gma) = SL.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (WS.WriterT w m) where
  local f (WS.WriterT ma) = WS.WriterT (local f ma)
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (WL.WriterT w m) where
  local f (WL.WriterT ma) = WL.WriterT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (MaybeT m) where
  local f (MaybeT ma) = MaybeT (local f ma)
  {-# INLINE local #-}

instance (Error e, MonadDi m) => MonadDi (ErrorT e m) where
  local f (ErrorT ma) = ErrorT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ExceptT e m) where
  local f (ExceptT ma) = ExceptT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (IdentityT m) where
  local f (IdentityT ma) = IdentityT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ListT m) where
  local f (ListT ma) = ListT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ContT r m) where
  local f (ContT gma) = ContT (\r -> local f (gma r))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (RWSS.RWST r w s m) where
  local f (RWSS.RWST gma) = RWSS.RWST (\r s -> local f (gma r s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (RWSL.RWST r w s m) where
  local f (RWSL.RWST gma) = RWSL.RWST (\r s -> local f (gma r s))
  {-# INLINE local #-}

--------------------------------------------------------------------------------

test :: MonadDi m => m ()
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
