{-# LANGUAGE RankNTypes #-}

-- | This module extends extends the /di logging ecosystem/ with support for the
-- [df1](https://hackage.haskell.org/package/df1) hierarchical structured
-- logging format.
--
-- Particularly, it exports 'df1' for rendering /df1/-formatted logs, an
-- extension to the "Di.Core" API with vocabulary specific to /df1/, and
-- functions like 'fromDiLog' or 'fromDf1Log' to convert back and forth between
-- /di/ and /df1/ types.
--
-- The "Di.Df1.Monad" module belonging to this same package exports an extension
-- to the "Di.Monad" API, rather than to "Di.Core".
--
-- Consider this a preview release: The API is likely to stay stable, but
-- extensive testing, formalization and tooling is due.
module Di.Df1
 ( Df1

   -- * Hierarchy
 , push
   -- * Metadata
 , attr
   -- * Logging from @IO@
 , debug
 , info
 , notice
 , warning
 , error
 , alert
 , critical
 , emergency
   -- ** Logging from @STM@
 , debug'
 , info'
 , notice'
 , warning'
 , error'
 , alert'
 , critical'
 , emergency'

   -- * Support for @Di.Handle@
 , df1
   -- * Conversion
 , fromDiLog
 , fromDf1Log

   -- * Types from @Df1@
 , Df1.Level
 , Df1.Path
 , Df1.Segment
 , Df1.ToSegment(segment)
 , Df1.Key
 , Df1.ToKey(key)
 , Df1.Value
 , Df1.ToValue(value)
 , Df1.Message
 , Df1.ToMessage(message)
 ) where

import Control.Concurrent.STM (STM)
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (error)
import Unsafe.Coerce (unsafeCoerce)

import qualified Di.Core as Di (Di, Log(Log), log, log', push)
import qualified Di.Handle as Di (LineRenderer(LineRendererUtf8))
import qualified Df1

--------------------------------------------------------------------------------

-- | Convenience type-synonym for a 'Di.Di' restricted to all the /df1/
-- monomorphic types.
--
-- @
-- 'Df1' == 'Di.Di' 'Df1.Level' 'Df1.Path' 'Df1.Message'
--    :: *
-- @
--
-- This type-synonym is not used within the @di-df1@ library itself because
-- all functions exposed in the library have more general types. However,
-- users are encouraged to use 'Df1' if they find it useful to reduce
-- boilerplate and improve type inferrence.
type Df1 = Di.Di Df1.Level Df1.Path Df1.Message

--------------------------------------------------------------------------------

-- | Push a new 'Df1.Segment' to the 'Di.Di'
push
  :: Df1.Segment
  -> Di.Di level Df1.Path msg
  -> Di.Di level Df1.Path msg   -- ^
push s = Di.push (Df1.Push s)
{-# INLINE push #-}

-- | Push a new attribute 'Df1.Key' and 'Df1.Value' to the 'Di.Di'.
attr
  :: Df1.ToValue value
  => Df1.Key
  -> value
  -> Di.Di level Df1.Path msg
  -> Di.Di level Df1.Path msg   -- ^
attr k v = Di.push (Df1.Attr k (Df1.value v))
{-# INLINE attr #-}

--------------------------------------------------------------------------------
-- MonadIO variants.

-- | Log a message stating that the system is unusable.
emergency
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
emergency di = Di.log di Df1.Emergency . Df1.message
{-# INLINE emergency #-}

-- | Log a condition that should be corrected immediately, such as a corrupted
-- database.
alert
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
alert di = Di.log di Df1.Alert . Df1.message
{-# INLINE alert #-}

-- | Log a critical condition that could result in system failure, such as a
-- disk running out of space.
critical
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
critical di = Di.log di Df1.Critical . Df1.message
{-# INLINE critical #-}

-- | Log an error condition, such as an unhandled exception.
error
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
error di = Di.log di Df1.Error . Df1.message
{-# INLINE error #-}

-- | Log a warning condition, such as an exception being gracefully handled or
-- some missing configuration setting being assigned a default value.
warning
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
warning di = Di.log di Df1.Warning . Df1.message
{-# INLINE warning #-}

-- | Log a condition that is not an error, but should possibly be handled
-- specially.
notice
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
notice di = Di.log di Df1.Notice . Df1.message
{-# INLINE notice #-}

-- | Log an informational message.
info
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
info di = Di.log di Df1.Info . Df1.message
{-# INLINE info #-}

-- | Log a message intended to be useful only when deliberately debugging a
-- program.
debug
  :: (MonadIO m, Df1.ToMessage msg)
  => Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
debug di = Di.log di Df1.Debug . Df1.message
{-# INLINE debug #-}

--------------------------------------------------------------------------------
-- STM variants

-- | Log a message stating that the system is unusable.
emergency'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
emergency' natSTM di = Di.log' natSTM di Df1.Emergency . Df1.message
{-# INLINE emergency' #-}

-- | Log a condition that should be corrected immediately, such as a corrupted
-- database.
alert'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
alert' natSTM di = Di.log' natSTM di Df1.Alert . Df1.message
{-# INLINE alert' #-}

-- | Log a critical condition that could result in system failure, such as a
-- disk running out of space.
critical'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
critical' natSTM di = Di.log' natSTM di Df1.Critical . Df1.message
{-# INLINE critical' #-}

-- | Log an error condition, such as an unhandled exception.
error'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
error' natSTM di = Di.log' natSTM di Df1.Error . Df1.message
{-# INLINE error' #-}

-- | Log a warning condition, such as an exception being gracefully handled or
-- some missing configuration setting being assigned a default value.
warning'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
warning' natSTM di = Di.log' natSTM di Df1.Warning . Df1.message
{-# INLINE warning' #-}

-- | Log a condition that is not an error, but should possibly be handled
-- specially.
notice'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
notice' natSTM di = Di.log' natSTM di Df1.Notice . Df1.message
{-# INLINE notice' #-}

-- | Log an informational message.
info'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
info' natSTM di = Di.log' natSTM di Df1.Info . Df1.message
{-# INLINE info' #-}

-- | Log a message intended to be useful only when deliberately debugging a
-- program.
debug'
  :: (Monad m, Df1.ToMessage msg)
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> msg
  -> m ()
debug' natSTM di = Di.log' natSTM di Df1.Debug . Df1.message
{-# INLINE debug' #-}

--------------------------------------------------------------------------------

-- | A 'LineRenderer' to be used with tools like 'Di.Handle.handle' or
-- 'Di.Handle.stderr' from the "Di.Handle" module.
df1 :: Di.LineRenderer Df1.Level Df1.Path Df1.Message
{-# INLINE df1 #-}
df1 = Di.LineRendererUtf8 (\x ->
  if x then Df1.renderColor . fromDiLog
       else Df1.render . fromDiLog)

--------------------------------------------------------------------------------

-- | Convert a 'Df1.Log' from "Df1" to a 'Di.Log' from "Di.Core".
--
-- @
-- 'fromDiLog' . 'fromDf1Log'  ==  'id'
-- @
--
-- @
-- 'fromDf1Log' . 'fromDiLog'  ==  'id'
-- @
fromDiLog :: Di.Log Df1.Level Df1.Path Df1.Message -> Df1.Log
{-# INLINE fromDiLog #-}
fromDiLog = unsafeCoerce  -- see _fromDiLog

-- | Unused. Just type-checking that the order of arguments didn't change.
_fromDiLog :: Di.Log Df1.Level Df1.Path Df1.Message -> Df1.Log
_fromDiLog (Di.Log a b c d) = Df1.Log a b c d

---

-- | Convert a 'Di.Log' from "Di.Core" to a 'Df1.Log' from "Df1".
--
-- @
-- 'fromDiLog' . 'fromDf1Log'  ==  'id'
-- @
--
-- @
-- 'fromDf1Log' . 'fromDiLog'  ==  'id'
-- @
fromDf1Log :: Df1.Log -> Di.Log Df1.Level Df1.Path Df1.Message
{-# INLINE fromDf1Log #-}
fromDf1Log = unsafeCoerce  -- see _fromDf1Log

-- | Unused. Just type-checking that the order of arguments didn't change.
_fromDf1Log :: Df1.Log -> Di.Log Df1.Level Df1.Path Df1.Message
_fromDf1Log (Df1.Log a b c d) = Di.Log a b c d

