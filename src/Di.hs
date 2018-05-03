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

-- | Import this module __qualified__ as follows:
--
-- @
-- import qualified "Di"
-- @
--
-- The /only/ name that you are encouraged to import unqualified is 'MonadDi':
--
-- @
-- import "Di" ('MonadDi')
-- @
module Di
 ( Di
 , new
 , new'

   -- * Sinks
 , stderrLines
 , handleLines

   -- * 'DiT'
 , DiT
 , runDiT
 , runDiT'

   -- * 'MonadDi'
 , MonadDi(local, ask, natSTM)

   -- ** Messages
 , log

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
 , Log(Log, logTime, logLevel, logPath, logMessage)
 , Message(Message)
 , Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , Path(Attr, Push, Root)
 , Segment(Segment)
 , Key(Key)
 , Value(Value)
 , Sink(Sink)
 , withSink
 , sinkFallback
 , LogLineParser(LogLineParserUtf8)
 , runLogLineParser
 , LogLineRenderer(LogLineRendererUtf8)
 , LogBlobRenderer(LogBlobRenderer)
 , test
 ) where

import Prelude hiding (error, max, log)

import Control.Applicative (Alternative)
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
  (STM, atomically, check,
   TQueue, newTQueueIO, readTQueue, writeTQueue, peekTQueue, isEmptyTQueue)
import qualified Control.Exception as Ex
import Control.Monad (join)
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
import Control.Monad.Trans.Free (iterTM)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Control.Monad.Zip (MonadZip)
import qualified Data.ByteString as B
import Data.String (fromString)
import Data.Function (fix)
import qualified Data.Time.Clock.System as Time
import Lens.Family2 (view)
import qualified Pipes.ByteString as Pb
import qualified Pipes as P
import qualified Pipes.Internal as P
import qualified System.Exit

import qualified Di.Df1
import Di.Misc (mute, muteSync, catchSync, getSystemTimeSTM, drainProducer)
import Di.Sink
  (Sink(Sink), withSink, stderrLines, handleLines, sinkFallback, withSink)
import Di.Types
  (Log(Log, logTime, logLevel, logPath, logMessage), Message(Message),
   Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
   Path(Attr, Push, Root),
   Segment(Segment), Key(Key), Value(Value),
   Di(Di, diMax, diPath, diLogs),
   LogLineParser(LogLineParserUtf8),
   LogLineRenderer(LogLineRendererUtf8),
   LogBlobRenderer(LogBlobRenderer))

{-
TODO Specify lazy semantics for Log fields. E.g., logging a message containing
`undefined` should be caught gracefuly inside di.

TODO Export the df1 rendererer from the Di module.

TODO Tests for lazy semantics.

TODO Mask/restore for filtering? Consider how practical this would be.

TODO Query language.

TODO Test STM semantics (e.g, STM.retry and `runDiT' id`)

TODO Consider DF1 (time, level, path, ...) order rather than (time, path,
level...)? Consider the UX with and without colors, and command-line tooling
support aspects.

TODO Switch for forcing colors off in handleLines.

TODO DF1 Parser (should cope with ANSI color escape stuff in input).

TODO DF1 roundtrip tests.

TODO DF1 rendering tests comparing with reference bytes.

TODO Rendering LogLineRendererUtf8 to non UTF-8 handles.

TODO Message metadata in log entries. Idea:

   data Message = Message
     { messageText :: !TL.Text
     , messageMeta :: ![(Key, Value)]
     }

   instance IsString Message where
     fromString s = Message (TL.pack s) []

   log :: MonadDi m => Message -> m ()

TODO A new `Message x` class for obtaining text and metadata to be rendered
from `x`? Changes to `log`?

   class ToMessage x where
     toMessage :: x -> Message
     default toMessage :: Typeable x => Message
     toMessage = Message constructorName fieldStringyThings

   class ToMessage String where
     toMessage s = Message (TL.pack s) []

   class ToMessage TL.Text where
     toMessage t = Message t []

   class ToMessage T.Text where
     toMessage t = Message (TL.fromStrict t) []

   newtype MyThingHappened = MyThingHappened { myThing_userId :: !Int }
   class ToMessage MyThing where
     toMessage (MyThing x) = Message "The thing happened" [("user-id", x)]

   -- This one has terrible type inferrence with OverloadedStrings.
   log' :: (MonadDi m, ToMessage a) => a -> m ()

TODO Rename MonadDi to MonadLog, DiT to LogT, Log to Request?
-}

--------------------------------------------------------------------------------

-- | Create a new 'Di' that writes to 'System.IO.stderr', one log message per
-- line.
--
-- @
-- 'new' ==  'new'' ('stderrLines' 'Di.Df1.render')
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
new :: (Di -> IO a) -> IO a
new = new' (stderrLines Di.Df1.render)

new' :: Sink -> (Di -> IO a) -> IO a
new' sink act =
  withSink sink $ \write -> do
    tqLogs :: TQueue Log <- newTQueueIO
    -- Start worker thread, restarting in in case of sync exceptions (unlikely).
    _ <- fix $ \k -> forkFinally (worker write tqLogs) $ \case
       Right () -> pure () -- All messages processed and nobody cares anymore.
       Left se -> case Ex.asyncExceptionFromException se of
          Just (_ :: Ex.AsyncException) -> Ex.throwIO se
          Nothing -> k >> pure ()
    let di = Di Debug Root tqLogs
    -- Run 'act', silently flushing and logging any unhandled synchronous
    -- exceptions afterwards.
    flip Ex.finally (mute (atomically (flushDi di))) $
      catchSync (act di) $ \se -> do
         case Ex.fromException se of
            Just (_ :: System.Exit.ExitCode) -> do
               pure () -- 'act' requested an exit, we'll exit as requested.
            Nothing -> do
               syst <- Ex.onException (Time.getSystemTime) (Ex.throw se)
               -- We mute because it doesn't really matter if this fails.
               mute $ atomically $ writeTQueue tqLogs $ Log
                 { logTime = syst, logLevel = Alert
                 , logPath = Attr "exception"
                     (fromString (Ex.displayException se)) (diPath di)
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
-- 'natSTM' could. Synchronous exceptions that happen due to failures in the
-- actual priting of the log message are handled in a different by attempting to
-- log the message to 'IO.stderr' as a fallback. Asynchronous exceptions that
-- might there, as expected, are not explicitly handled. In practical terms,
-- this means that unless you know what you are doing, /you should
-- just call 'log' without worrying about it ever throwing exceptions/.
log :: MonadDi m => Level -> Message -> m ()
log l = \m -> ask >>= \di ->
   when (l >= diMax di) $ do
      !syst <- natSTM getSystemTimeSTM
      let !x = Log syst l (diPath di) m
      natSTM $ writeTQueue (diLogs di) x
{-# INLINABLE log #-}

-- | @
-- 'emergency' == 'log' 'Emergency'
-- @
emergency :: MonadDi m => Message -> m ()
emergency = log Emergency
{-# INLINE emergency #-}

-- | @
-- 'alert' == 'log' 'Alert'
-- @
alert :: MonadDi m => Message -> m ()
alert = log Alert
{-# INLINE alert #-}

-- | @
-- 'critical' == 'log' 'Critical'
-- @
critical :: MonadDi m => Message -> m ()
critical = log Critical
{-# INLINE critical #-}

-- | @
-- 'error' == 'log' 'Error'
-- @
error :: MonadDi m => Message -> m ()
error = log Error
{-# INLINE error #-}

-- | @
-- 'warning' == 'log' 'Warning'
-- @
warning :: MonadDi m => Message -> m ()
warning = log Warning
{-# INLINE warning #-}

-- | @
-- 'notice' == 'log' 'Notice'
-- @
notice :: MonadDi m => Message -> m ()
notice = log Notice
{-# INLINE notice #-}


-- | @
-- 'info' == 'log' 'Info'
-- @
info :: MonadDi m => Message -> m ()
info = log Info
{-# INLINE info #-}

-- | @
-- 'debug' == 'log' 'Debug'
-- @
debug :: MonadDi m => Message -> m ()
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
flush = ask >>= \di -> natSTM (flushDi di)
{-# INLINABLE flush #-}

-- | Returns a new 'Di' on which only messages with at least the specified
-- 'Level' of importance are ever logged.
--
-- For example, @'max' 'Notice'@ will prevent messages with 'Level's 'Info'
-- and 'Debug' from ever being loged through the resulting 'Di'.
--
-- Notice that a use of 'max' can't be “undone” by the given action. That is, in
-- @'max' x y@, the highest log level that @y@ will be able to produce is @x@.
-- More generally:
--
-- @
-- forall a b.
--    'max' a . 'max' b  ==  'max' ('min' a b)  --  This is 'min' from "Prelude".
-- @
max :: MonadDi m => Level -> m a -> m a
max !l = local (\di -> di { diMax = min l (diMax di) })
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
push :: MonadDi m => Segment -> m a -> m a
{-# INLINE push #-}
push s = local (\di -> di { diPath = Push s (diPath di) })

attr :: MonadDi m => Key -> Value -> m a -> m a
{-# INLINE attr #-}
attr k v m = local (\di -> di { diPath = Attr k v (diPath di) }) m

--------------------------------------------------------------------------------

-- | A 'MonadDi' abstracts over the requirements for logging with 'Di' through a
-- monadic interface.
--
-- All of the functions intended to generate new log messages or metadata, such
-- as 'push' or 'debug', rely on 'MonadDi'.
--
-- Semantically, @'MonadDi' m@ is a “reader monad” that carries as its
-- environment a 'Di' and natural transformation from 'STM' to @m@.
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
  --
  -- Notice that /it is not necessary/ for this natural transformation to be a
  -- monad morphism as well. That is, 'atomically' is acceptable.
  natSTM :: STM a -> m a
  default natSTM :: (MonadTrans t, MonadDi n, m ~ t n) => STM a -> m a
  natSTM = \x -> lift (natSTM x)
  {-# INLINE natSTM #-}

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

-- TODO: we can't have a MFunctor DiT instance because of the natural
-- transformation inside 'DiT'. We need to move that natural transformation
-- out.

-- | Like 'runDiT'', but specialized to run with an underlying 'MonadIO'.
--
-- @
-- 'runDiT'  ==  'runDiT'' ('liftIO' . 'atomically')
-- @
--
-- Also, notice that 'runDiT' is a /monad morphism/ from @'DiT' m@ to @m@.
runDiT :: MonadIO m => Di -> DiT m a -> m a
runDiT = runDiT' (\x -> liftIO (atomically x))
{-# INLINE runDiT #-}

-- | Like 'runDiT'', however it doesn't require a 'MonadIO' constraint. Instead,
-- it takes the natural transformation that will be used by 'natSTM' as an
-- argument.
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
-- transaction: That is, a log message is commited only once to the outside
-- world if and when the 'STM' transaction succeeds. That is, the following
-- example will only ever log @\"YES\"@:
--
-- @
-- 'atomically' $ 'runDiT'' 'id' $ do
--    ('info' \"NO\" >> 'lift' 'Control.Concurrent.STM.retry') <|> 'info' \"YES\")
-- @
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
--
-- Additionally, notice that 'runDiT'' itself is a /monad morphism/ from @'DiT'
-- m@ to @m@ which doesn't perform any side effects of its own.  Particularly,
-- the given 'Di' remains unaffected. So you can use it as many times you want.
--
-- @
-- forall f di x.
--    'runDiT'' f di ('lift' x)  ==  x
-- @
runDiT' :: (forall x. STM x -> m x) -> Di -> DiT m a -> m a
runDiT' h = \di -> \(DiT (ReaderT f)) -> f (di, H h)
{-# INLINE runDiT' #-}

instance MonadReader r m => MonadReader r (DiT m) where
  ask = DiT (ReaderT (\_ -> Reader.ask))
  {-# INLINE ask #-}
  local f = \(DiT (ReaderT gma)) ->
     DiT (ReaderT (\di -> Reader.local f (gma di)))
  {-# INLINE local #-}

instance Monad m => MonadDi (DiT m) where
  ask = DiT (ReaderT (\(di,_) -> pure di))
  {-# INLINE ask #-}
  local f = \(DiT (ReaderT gma)) -> DiT (ReaderT (\(di, h) -> gma (f di, h)))
  {-# INLINE local #-}
  natSTM = \x -> DiT (ReaderT (\(_, H h) -> h x))
  {-# INLINE natSTM #-}

--------------------------------------------------------------------------------

instance MonadDi m => MonadDi (ReaderT r m) where
  local f = \(ReaderT gma) -> ReaderT (\r -> local f (gma r))
  {-# INLINE local #-}

instance MonadDi m => MonadDi (SS.StateT s m) where
  local f = \(SS.StateT gma) -> SS.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance MonadDi m => MonadDi (SL.StateT s m) where
  local f = \(SL.StateT gma) -> SL.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (WS.WriterT w m) where
  local f = \(WS.WriterT ma) -> WS.WriterT (local f ma)
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (WL.WriterT w m) where
  local f = \(WL.WriterT ma) -> WL.WriterT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (MaybeT m) where
  local f = \(MaybeT ma) -> MaybeT (local f ma)
  {-# INLINE local #-}

instance (Error e, MonadDi m) => MonadDi (ErrorT e m) where
  local f = \(ErrorT ma) -> ErrorT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ExceptT e m) where
  local f = \(ExceptT ma) -> ExceptT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (IdentityT m) where
  local f = \(IdentityT ma) -> IdentityT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ListT m) where
  local f = \(ListT ma) -> ListT (local f ma)
  {-# INLINE local #-}

instance MonadDi m => MonadDi (ContT r m) where
  local f = \(ContT gma) -> ContT (\r -> local f (gma r))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (RWSS.RWST r w s m) where
  local f = \(RWSS.RWST gma) -> RWSS.RWST (\r s -> local f (gma r s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi m) => MonadDi (RWSL.RWST r w s m) where
  local f = \(RWSL.RWST gma) -> RWSL.RWST (\r s -> local f (gma r s))
  {-# INLINE local #-}

instance MonadDi m => MonadDi (P.Proxy a' a b' b m) where
  {-# INLINABLE local #-}
  local f = \case
     P.Request a' fa -> P.Request a'(\a -> local f (fa  a ))
     P.Respond b fb' -> P.Respond b (\b' -> local f (fb' b'))
     P.Pure r -> P.Pure r
     P.M m -> P.M (local f m >>= \r -> pure (local f r))

--------------------------------------------------------------------------------

-- | Run a 'LogLineParser' on a stream of raw input containing one or more
-- lines.
runLogLineParser
  :: forall m a
  .  Monad m
  => LogLineParser
  -> P.Producer B.ByteString m a
  -- ^ It is not necessary for each of these 'B.ByteStrings' to contain exactly
  -- one line. 'runLogLineParser' will break the input into lines before
  -- processing it. In other words, something like 'Pb.stdin' is acceptable
  -- here.
  -> P.Producer (Either String Log) m a
  -- If it's possible to par
runLogLineParser (LogLineParserUtf8 parser0) = \pb0 -> do
    let pb1 = pb0 P.>-> Pb.filter (/= 13)  -- We discard '\r' from input.
    iterTM fline (view (Pb.splits 10) pb1) -- We split on '\n' and parse.
  where
    -- The outer producer produces a line of input, in chunks.
    fline :: P.Producer B.ByteString m (P.Producer (Either String Log) m a)
          -> P.Producer (Either String Log) m a
    fline = \pb0 -> do
      (yel, pb1) <- lift (SS.runStateT parser1 pb0)
      mapM_ P.yield yel
      lift (Pb.nextByte pb1) >>= \case
        Left pel0 -> pel0
        Right (_, pb2) -> do
           P.yield (Left ("Skipping leftover line input"))
           join (lift (drainProducer pb2))
    parser1 :: Pb.Parser B.ByteString m (Maybe (Either String Log))
    parser1 = do
      Pb.isEndOfBytes >>= \case
         True -> pure Nothing
         False -> fmap Just parser0
    {-# INLINE fline #-}
    {-# INLINE parser1 #-}

--------------------------------------------------------------------------------

test :: MonadDi m => m ()
test = do
  max Debug $ do
      notice "Running test"
      debug "attr a"
      attr "a" "b" $ do
        debug "push z"
        push "z" $ do
          debug "attr c"
          attr "c" "d" $ do
            debug "attr a again"
            attr "a" "e" $ do
              debug "push y"
              push "y" $ do
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
