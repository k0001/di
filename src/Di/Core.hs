{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Di.Core
 ( Di(..)
 , mkDi
 , log
 , flush
 , push
 , filter
 , contralevel
 , contrapath
 , contramsg
 ) where

import Control.Concurrent (forkFinally, myThreadId)
import Control.Concurrent.STM
import qualified Control.Exception as Ex
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (fix)
import Data.Monoid (mconcat, (<>))
import qualified Data.Time as Time
import Prelude hiding (log, filter)
import qualified System.IO as IO

--------------------------------------------------------------------------------

-- | @'Di' level path msg@ allows you to to log messages of type @msg@, with a
-- particular importance @level@, under a scope identified by @path@ (think of
-- @path@ as a filesystem path that you can use to group together related log
-- messages).
--
-- Each @msg@ gets logged together with its @level@, @path@ and the
-- 'Time.UTCTime' stating the instant when the logging requests was made.
--
-- Even though logging is usually associated with rendering text, 'Di' makes no
-- assumption about the types of the @msg@ values being logged, nor the @path@
-- values that convey their scope, nor the @level@ values that convey their
-- importance. Instead, it delays conversion from these precise types into the
-- ultimately desired raw representation (if any) as much as possible. This
-- makes it possible to log more precise information (for example, logging a
-- datatype of your own without having to convert it to text first), richer
-- scope paths (for example, the scope could be a 'Data.Map.Strict.Map' that
-- gets enriched with more information as we 'push' down the @path@), and
-- importance @level@s that are never too broad nor too narrow. This improves
-- type safety, as well as the composability of the @level@, @path@ and @msg@
-- values. In particular, all of @level@, @path@ and @msg@ are contravariant
-- values, which in practice means including a precise 'Di' into a more general
-- 'Di' is always possible (see the 'contralevel', 'contrapath' and 'contramsg'
-- functions).
--
-- Messages of undesired importance levels can be muted by using 'filter'.
--
-- Contrary to other logging approaches based on monad transformers, a 'Di' is a
-- value that is expected to be passed around explicitly. Of course, if
-- necessary you can always put a 'Di' in some internal monad state or
-- environment and provide a custom API for it. That's a choice you can make.
--
-- A 'Di' can be safely used concurrently, and messages are rendered in the
-- absolute order they were submitted for logging.
--
-- 'Di' is pronounced as \"dee" (not \"die" nor \"dye" nor \"day"). \"Di" is
-- the spanish word for an imperative form of the verb \"decir", which in
-- english means "to say", which clearly has something to do with logging.
data Di level path msg = Di
  { _diLog :: Time.UTCTime -> level -> path -> msg -> IO ()
    -- ^ Low level logging function.
  , _diFilter :: level -> Bool
    -- ^ Whether a particular message @level@ should be logged or not.
  , _diLogs :: TQueue (IO ())
    -- ^ Work queue. This queue keeps fully applied '_diLog' calls.
  }

-- | Build a new 'Di' from a logging function.
--
-- /Note:/ If the passed in 'IO' function throws a exception, it will be
-- just logged to 'IO.stderr' and then ignored.
--
-- /Note:/ There's no need to "release" the obtained 'Di'.
mkDi
  :: MonadIO m
  => (Time.UTCTime -> level -> path -> msg -> IO ())
  -> m (Di level path msg)  -- ^
mkDi f = liftIO $ do
   di <- Di f (const True) <$> newTQueueIO
   me <- myThreadId
   _ <- forkFinally (worker di) (either (Ex.throwTo me) pure)
   pure di
 where
   worker :: Di level path msg -> IO ()
   worker di = fix $ \k -> do
     eio <- Ex.try $ atomically $ do
        io <- peekTQueue (_diLogs di)
        pure (Ex.finally io (atomically (readTQueue (_diLogs di))))
     case eio of
        Left (_ :: Ex.BlockedIndefinitelyOnSTM) -> do
           pure ()  -- Nobody writes to '_diLogs' anymore, so we can just stop.
        Right io -> do
           catchSync io $ \se -> do
              ts <- fmap renderIso8601 Time.getCurrentTime
              IO.hPutStrLn IO.stderr $ mconcat
                 [ "Logging error at ", ts
                 , ": could not log message at due to "
                 , Ex.displayException se, ". Ignoring." ]
           k

-- | Log a message with the given importance @level@.
--
-- This function returns immediately after queing the message for logging in a
-- different thread. If you want to explicitly wait for the message to be
-- logged, then call 'flush' afterwards.
--
-- /Note:/ No exceptions from the underlying logging backend (i.e., the 'IO'
-- action given to 'mkDi') will be thrown from 'log'. Instead, those will be
-- recorded to 'IO.stderr' and ignored.
log :: (MonadIO m, Monoid path) => Di level path msg -> level -> msg -> m ()
log (Di dLog dFilter dLogs) !l = \(!m) ->
   when (dFilter l) $ liftIO $ do
      ts <- Time.getCurrentTime
      atomically $ writeTQueue dLogs $! dLog ts l mempty m
{-# INLINABLE log #-}


-- | Block until all messages being logged have finished processing.
--
-- Manually calling 'flush' is not usually necessary, but, if at some point you
-- want to ensure that all messages logged until then have properly rendered to
-- the underlying backend, then 'flush' will block until that happens.
flush :: MonadIO m => Di level path msg -> m ()
flush di = liftIO (atomically (check =<< isEmptyTQueue (_diLogs di)))
{-# INLINABLE flush #-}


-- | Returns a new 'Di' on which only messages with a @level@ satisfying the
-- given predicate—in addition to any previous 'filter's—are ever logged.
--
-- Identity:
--
-- @
-- 'filter' ('const' 'True')    ==   'id'
-- @
--
-- Composition:
--
-- @
-- 'filter' ('Control.Applicative.liftA2' (&&) f g)   ==   'filter' f . 'filter' g
-- @
--
-- Conmutativity:
--
-- @
-- 'filter' f . 'filter' g    ==    'filter' g . 'filter' f
-- @
filter :: (level -> Bool) -> Di level path msg -> Di level path msg
filter f = \di -> di { _diFilter = \l -> f l && _diFilter di l }
{-# INLINABLE filter #-}


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
push :: Monoid path => path -> Di level path msg -> Di level path msg
push ps = \(Di dLog dFilter dLogs) ->
  Di (\ts l p1 m -> dLog ts l (ps <> p1) m) dFilter dLogs
{-# INLINABLE push #-}


-- | A 'Di' is contravariant in its @level@ argument.
--
-- This function is used to go from a /more general/ to a /less general/ type
-- of @level@. For example, @data Level = Info | Error@ is a less general type
-- than @data Level' = Info' | Warning' | Error'@, since the former can only
-- convey two logging levels, whereas the latter can convey three. We can
-- convert from the more general to the less general @level@ type using this
-- 'contralevel' function:
--
-- @
-- 'contralevel' (\\case { Info -> Info'; Error -> Error' }) (di :: 'Di' Level' ['String'] msg)
--     :: 'Di' Level ['Int'] msg
-- @
--
-- Identity:
--
-- @
-- 'contralevel' 'id'   ==   'id'
-- @
--
-- Composition:
--
-- @
-- 'contralevel' (f . g)   ==   'contralevel' g . 'contralevel' f
-- @
contralevel :: (level -> level') -> Di level' path msg  -> Di level path msg
contralevel f = \(Di dLog dFilter dLogs) ->
  Di (\ts l p m -> dLog ts (f l) p m) (\l -> dFilter (f l)) dLogs
{-# INLINABLE contralevel #-}

-- | A 'Di' is contravariant in its @path@ argument.
--
-- This function is used to go from a /more general/ to a /less specific/ type
-- of @path@. For example, @['Int']@ is a less general type than @['String']@,
-- since the former clearly conveys the idea of a list of numbers, whereas the
-- latter could be a list of anything that is representable as 'String', such as
-- names of fruits and poems. We can convert from the more general to the less
-- general @path@ type using this 'contrapath' function:
--
-- @
-- 'contrapath' ('map' 'show') (di :: 'Di' level ['String'] msg)
--     :: 'Di' ['Int'] msg
-- @
--
-- Identity:
--
-- @
-- 'contrapath' 'id'   ==   'id'
-- @
--
-- Composition:
--
-- @
-- 'contrapath' (f . g)   ==   'contrapath' g . 'contrapath' f
-- @
contrapath :: (path -> path') -> Di level path' msg  -> Di level path msg
contrapath f = \(Di dLog dFilter dLogs) ->
  Di (\ts l p m -> dLog ts l (f p) m) dFilter dLogs
{-# INLINABLE contrapath #-}

-- | A 'Di' is contravariant in its @msg@ argument.
--
-- This function is used to go from a /more general/ to a /less general/ type
-- of @msg@. For example, @'Int'@ is a less general type than @'String'@, since
-- the former clearly conveys the idea of a numbers, whereas the latter could be
-- a anything that is representable as 'String', such as names of painters and
-- colors. We can convert from the more general to the less general @msg@ type
-- using this 'contramsg' function:
--
-- @
-- 'contramsg' 'show' (di :: 'Di' level path 'String')
--     :: 'Di' level path 'Int'
-- @
--
-- Identity:
--
-- @
-- 'contramsg' 'id'   ==   'id'
-- @
--
-- Composition:
--
-- @
-- 'contramsg' (f . g)   ==   'contramsg' g . 'contramsg' f
-- @
contramsg :: (msg -> msg') -> Di level path msg' -> Di level path msg
contramsg f = \(Di dLog dFilter dLogs) ->
  Di (\ts l p m -> dLog ts l p (f m)) dFilter dLogs
{-# INLINABLE contramsg #-}

--------------------------------------------------------------------------------

renderIso8601 :: Time.UTCTime -> String
renderIso8601 = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ"

catchSync :: IO a -> (Ex.SomeException -> IO a) -> IO a
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwIO (ae :: Ex.AsyncException)
   Nothing -> f se
