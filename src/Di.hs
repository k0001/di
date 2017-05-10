{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended module usage:
--
-- @
-- import Di (Di, dbg, inf, wrn, err)
-- import qualified Di
-- @

module Di
 ( Di
 , mkDi
 , push
 , contrapath
 , contramsg
 , level
 , Level(DBG, INF, WRN, ERR)
   -- * Asynchronous logging
 , dbg
 , inf
 , wrn
 , err
 , flush
   -- * Synchronous logging
 , dbg'
 , inf'
 , wrn'
 , err'
   -- * Backends
 , mkDiStringStderr
 , mkDiStringHandle
 ) where

import Control.Concurrent (forkFinally, myThreadId)
import Control.Concurrent.STM
import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mconcat, mappend, (<>))
import Data.String (IsString(fromString))
import qualified Data.Time as Time
import qualified System.IO as IO

--------------------------------------------------------------------------------

-- | @'Di' path msg@ allows you to to log messages of type @msg@, under a scope
-- identified by @path@ (think of @path@ as a filesystem path).
--
-- Each @msg@ gets logged together with its 'Level', @path@ and the
-- 'Time.UTCTime' timestamp stating when the logging requests was made.
--
-- Even though logging is usually associated with rendering text, 'Di' makes no
-- assumption about the types of the @msg@ values being logged, nor the @path@
-- values that convey their scope. Instead, it delays conversion from these
-- precise types into the ultimately desired raw representation as much as
-- possible. This makes it possible to log more precise information (for
-- example, logging a datatype of your own without having to convert it to text
-- first), richer scope paths (for example, the scope could be a
-- 'Data.Map.Strict.Map' that gets enriched with more information as we 'push'
-- down the @path@). This improves type safety, as well as the composability of
-- the @path@ and @msg@ values. In particular, @path@ and @msg@ are
-- contravariant values (see the 'contrapath' and 'contramsg' functions).
--
-- Contrary to other logging approaches based on monadic interfaces, a 'Di' is a
-- value that is expected to be passed around explicitly. A 'Di' can be safely
-- used concurrently, and messages are rendered in the order they were submitted
-- for logging, both in the case of synchronous logging (e.g., 'err'') and
-- asynchronous logging (e.g., 'err').
--
-- 'Di' is pronounced as \"dee" (not \"die" nor \"dye" nor \"day"). \"Di" is
-- the spanish word for an imperative form of the verb \"decir", which in
-- english means "to say".
data Di path msg = forall path'. Monoid path' => Di
  { _diLog :: Level -> Time.UTCTime -> path' -> msg -> IO ()
    -- ^ Low level logging function.
  , _diPathMap :: path -> path'
    -- ^ Used to implement the 'path' function while preserving the 'Monoid'
    -- semantics of the @path'@ type.
  , _diMinLevel :: Level
    -- ^ Minimum level that we are allowed to log.
  , _diLogs :: TQueue (IO ())
    -- ^ Work queue. This queue keeps fully applied '_diLog' calls.
  }

-- | Build a 'Di' from a logging function.
mkDi
  :: (MonadIO m, Monoid path)
  => (Level -> Time.UTCTime -> path -> msg -> IO ())
  -> m (Di path msg)  -- ^
mkDi f = liftIO $ do
   di <- Di f id minBound <$> newTQueueIO
   me <- myThreadId
   _ <- forkFinally (worker di) (either (Ex.throwTo me) pure)
   pure di
 where
   worker :: Di path msg -> IO ()
   worker di = do
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
                 [ "ERR ", ts, " Di: could not log message at due to: "
                 , Ex.displayException se ]
           worker di

-- | Block until all messages being logged have finished processing.
flush :: MonadIO m => Di path msg -> m ()
flush di = liftIO $ atomically $ check =<< isEmptyTQueue (_diLogs di)
{-# INLINE flush #-}

-- | Asynchronously log a message with the given 'Level' by queueing it in FIFO
-- order to be logged in a different thread as soon as possible. The timestamp
-- of the logged message will correctly represent the time of the 'diAsync'
-- call.
--
-- /WARNING/ This function returns immediately, which makes it ideal for usage
-- in tight loops. However, if logging the message fails later, you won't be
-- able to catch the relevant exception.
diAsync :: MonadIO m => Di path msg -> Level -> msg -> m ()
diAsync (Di dLog _ dMinLevel dLogs) l m
  | l < dMinLevel = pure ()
  | otherwise = liftIO $ do
       ts <- Time.getCurrentTime
       atomically $ writeTQueue dLogs (dLog l ts mempty m)
{-# INLINABLE diAsync #-}

-- | Log a message with the given 'Level'.
diSync :: MonadIO m => Di path msg -> Level -> msg -> m ()
diSync di l m = diAsync di l m >> flush di
{-# INLINABLE diSync #-}

-- | Push new @path@s to the 'Di'.
--
-- The passed in 'Di' can continue to be used even after using 'push' or the
-- returned 'Di'.
--
-- @
-- 'push' di [a, b]   ===   'push' ('push' di [a]) [b]
-- @
--
-- See 'mkDiStringStderr' for an example behaviour.
push :: Di path msg -> [path] -> Di path msg
push (Di dLog dPathMap dMinLevel dLogs) ps =
  let dLog' = \l ts p1 m -> dLog l ts (mconcat (map dPathMap ps) <> p1) m
  in Di dLog' dPathMap dMinLevel dLogs
{-# INLINABLE push #-}

-- | A 'Di' is contravariant in its @path@ argument.
--
-- This function is used to go from a /more general/ to a /more specific/ type
-- of @path@. For example, @['Int']@ is a more specific type than @['String']@,
-- since the former clearly conveys the idea of a list of numbers, whereas the
-- latter could be a list of anything that is representable as 'String', such as
-- dictionary words. We can convert from the more general to the more specific
-- @path@ type using this 'path' function:
--
-- @
-- 'contrapath' ('map' 'show') (x :: 'Di' ['String'] msg) :: 'Di' ['Int'] msg
-- @
--
-- The 'Monoid'al behavior of the original @path'@ is preserved in the resulting
-- 'Di'.
contrapath :: (path -> path') -> Di path' msg  -> Di path msg
contrapath f (Di dLog dPathMap dMinLevel dLogs) =
  Di dLog (dPathMap . f) dMinLevel dLogs
{-# INLINABLE contrapath #-}

-- | A 'Di' is contravariant in its @msg@ argument.
--
-- This function is used to go from a /more general/ to a /more specific/ type
-- of @msg@. For example, @'Int'@ is a more specific type than @'String'@, since
-- the former clearly conveys the idea of a numbers, whereas the latter could be
-- a anything that is representable as 'String', such as a dictionary word. We
-- can convert from the more general to the more specific @msg@ type using this
-- 'msg' function:
--
-- @
-- 'contramsg' 'show' (x :: 'Di' path 'String') :: 'Di' path 'Int'
-- @
contramsg :: (msg -> msg') -> Di path msg' -> Di path msg
contramsg f (Di dLog dPathMap dMinLevel dLogs) =
  let dLog' = \l ts p m -> dLog l ts p (f m)
  in Di dLog' dPathMap dMinLevel dLogs
{-# INLINABLE contramsg #-}

-- | Returns a new 'Di' on which messages below the given 'Level' are not
-- logged, where ther ordering of levels is as follow:
--
-- @
-- 'DBG' < 'INF' < 'WRN' < 'ERR'
-- @
--
-- For example, @'level' x 'WRN'@ will prevent 'DBG' and 'INF' from being logged.
--
-- Notice that @'level' di x@ will allow messages with a level greater than or
-- equal to @x@ even if they had been previously silenced in the given @di@.
level :: Di path msg -> Level -> Di path msg
level di l = di { _diMinLevel = l }

--------------------------------------------------------------------------------

data Level
  = DBG    -- ^ Debug
  | INF    -- ^ Info
  | WRN    -- ^ Warning
  | ERR    -- ^ Error
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

--------------------------------------------------------------------------------

-- | Synchronously log a message with 'DBG' level.
dbg' :: MonadIO m => Di path msg -> msg -> m ()
dbg' di = diSync di DBG
{-# INLINE dbg' #-}

-- | Synchronously log a message with 'INF' level.
inf' :: MonadIO m => Di path msg -> msg -> m ()
inf' di = diSync di INF
{-# INLINE inf' #-}

-- | Synchronously log a message with 'WRN' level.
wrn' :: MonadIO m => Di path msg -> msg -> m ()
wrn' di = diSync di WRN
{-# INLINE wrn' #-}

-- | Synchronously log a message with 'ERR' level.
err' :: MonadIO m => Di path msg -> msg -> m ()
err' di = diSync di ERR
{-# INLINE err' #-}

--------------------------------------------------------------------------------

-- | Asynchronously log a message with 'DBG' level by queueing it in FIFO
-- order to be logged in a different thread as soon as possible. The timestamp
-- of the logged message will correctly represent the time of the 'dbg' call.
--
-- /WARNING/ This function returns immediately, which makes it ideal for usage
-- in tight loops. However, if logging the message fails later, you won't be
-- able to catch the relevant exception. Said exception will be printed out to
-- 'IO.stderr' as a last resort, and processing of further log messages will
-- continue.
dbg :: MonadIO m => Di path msg -> msg -> m ()
dbg di = diAsync di DBG
{-# INLINE dbg #-}

-- | Asynchronously log a message with 'INF' level by queueing it in FIFO
-- order to be logged in a different thread as soon as possible. The timestamp
-- of the logged message will correctly represent the time of the 'inf' call.
--
-- /WARNING/ This function returns immediately, which makes it ideal for usage
-- in tight loops. However, if logging the message fails later, you won't be
-- able to catch the relevant exception. Said exception will be printed out to
-- 'IO.stderr' as a last resort, and processing of further log messages will
-- continue.
inf :: MonadIO m => Di path msg -> msg -> m ()
inf di = diAsync di INF
{-# INLINE inf #-}

-- | Asynchronously log a message with 'WRN' level by queueing it in FIFO
-- order to be logged in a different thread as soon as possible. The timestamp
-- of the logged message will correctly represent the time of the 'wrn' call.
--
-- /WARNING/ This function returns immediately, which makes it ideal for usage
-- in tight loops. However, if logging the message fails later, you won't be
-- able to catch the relevant exception. Said exception will be printed out to
-- 'IO.stderr' as a last resort, and processing of further log messages will
-- continue.
wrn :: MonadIO m => Di path msg -> msg -> m ()
wrn di = diAsync di WRN
{-# INLINE wrn #-}

-- | Asynchronously log a message with 'ERR' level by queueing it in FIFO
-- order to be logged in a different thread as soon as possible. The timestamp
-- of the logged message will correctly represent the time of the 'err' call.
--
-- /WARNING/ This function returns immediately, which makes it ideal for usage
-- in tight loops. However, if logging the message fails later, you won't be
-- able to catch the relevant exception. Said exception will be printed out to
-- 'IO.stderr' as a last resort, and processing of further log messages will
-- continue.
err :: MonadIO m => Di path msg -> msg -> m ()
err di = diAsync di ERR
{-# INLINE err #-}

--------------------------------------------------------------------------------

{-
test :: IO ()
test = do
  d0 <- mkDiStringStderr
  dbg d0 "a"
  let d1 = push d0 ["f/oo"]
  inf' d1 "b"
  let d2 = push d1 ["b ar"]
  wrn d2 "c"
  let d3 = push d2 ["qux"]
  inf (level d3 WRN) "d"
  err d0 "e\nf"
  let d4 = push (contrapath show d3) [True, False]
  err d4 "asd"
  let d5 = push (contramsg show d4) [False]
  err' d5 True
-}

--------------------------------------------------------------------------------

-- | Strings separated by a forward slash. Doesn't contain white space.
--
-- Use 'fromString' (GHC's  @OverloadedStrings@ extension) to construct a
-- 'StringPath'.
newtype StringPath = StringPath { unStringPath :: String }
  deriving (Eq, Ord, Show)

instance IsString StringPath where
  fromString = stringPathSingleton

stringPathSingleton :: String -> StringPath
stringPathSingleton = StringPath . map f
  where f :: Char -> Char
        f '/'  = '.'
        f ' '  = '_'
        f '\n' = '_'
        f '\r' = '_'
        f c    = c

instance Monoid StringPath where
  mempty = StringPath ""
  mappend (StringPath "") b = b
  mappend a (StringPath "") = a
  mappend (StringPath a) (StringPath b) = StringPath (a <> "/" <> b)

-- | 'String's are written to 'IO.Handle' using the 'IO.Handle''s locale
-- encoding.
--
-- See 'mkDiStringStderr' for example output.
mkDiStringHandle
  :: MonadIO m
  => IO.Handle
  -> m (Di String String)
mkDiStringHandle h = liftIO $ do
    IO.hSetBuffering h IO.LineBuffering
    fmap (contrapath stringPathSingleton) $ mkDi $ \l ts p m -> do
       IO.hPutStrLn h $ mconcat
          [ show l, " ", renderIso8601 ts
          , if p == mempty then "" else (" " <> unStringPath p)
          , ": ", noBreaks m ]
       IO.hFlush h
  where
    noBreaks :: String -> String
    noBreaks = concatMap $ \case
      '\n' -> "\\n"
      '\r' -> "\\r"
      c -> [c]

-- | 'String' is written to 'IO.stderr' using the system's locale encoding.
--
-- @
-- > d0 <- 'mkDiStringStderr'
-- > 'dbg' d0 "a"
-- __DBG 2017-05-06T19:01:27:306168750000Z: a__
-- > let d1 = push d0 ["f\/oo"]       -- /\'\/' is converted to \'.'/
-- > 'inf' d1 "b"
-- __INF 2017-05-06T19:01:27:314333636000Z f.oo: b__
-- > let d2 = push d1 ["b ar"]        -- /\' ' is converted to \'_'/
-- > 'wrn' d2 "c"
-- __WRN 2017-05-06T19:01:27:322092498000Z f.oo\/b_ar: c__
-- > let d3 = push d2 ["qux"]
-- > 'err' d3 "d"
-- __ERR 2017-05-06T19:01:27:326704385000Z f.oo\/b_ar\/qux: d__
-- > 'err' d0 "e\\nf"                  -- /d0, of course, still works/
-- __ERR 2017-05-06T19:01:27:823167007000Z: e\\nf__
-- @
mkDiStringStderr :: MonadIO m => m (Di String String)
mkDiStringStderr = mkDiStringHandle IO.stderr

--------------------------------------------------------------------------------

renderIso8601 :: Time.UTCTime -> String
renderIso8601 = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ"

catchSync :: IO a -> (Ex.SomeException -> IO a) -> IO a
catchSync m f = Ex.catch m $ \se -> case Ex.asyncExceptionFromException se of
   Just ae -> Ex.throwIO (ae :: Ex.AsyncException)
   Nothing -> f se
