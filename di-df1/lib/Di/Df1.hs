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
 ( -- * Hierarchy
   push
   -- * Metadata
 , attr
   -- * Messages
 , debug
 , info
 , notice
 , warning
 , error
 , alert
 , critical
 , emergency
   -- ** Non 'MonadIO' variants
 , debug'
 , info'
 , notice'
 , warning'
 , error'
 , alert'
 , critical'
 , emergency'

   -- * "Di.Handle" support
 , df1
   -- * Conversion
 , fromDiLog
 , fromDf1Log
 ) where

import Control.Concurrent.STM (STM)
import Control.Monad.IO.Class (MonadIO)
import Prelude hiding (error)
import Unsafe.Coerce (unsafeCoerce)

import qualified Di.Core as Di (Di, Log(Log), log, log', push)
import qualified Di.Handle as Di (LineRenderer(LineRendererUtf8))
import qualified Df1

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
  :: Df1.Key
  -> Df1.Value
  -> Di.Di level Df1.Path msg
  -> Di.Di level Df1.Path msg   -- ^
attr k v = Di.push (Df1.Attr k v)
{-# INLINE attr #-}

--------------------------------------------------------------------------------
-- MonadIO variants.

-- | Log a message stating that the system is unusable.
--
-- @
-- 'emergency' == 'Di.log' 'Df1.Emergency'
-- @
emergency
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
emergency di = Di.log di Df1.Emergency
{-# INLINE emergency #-}

-- | Log a condition that should be corrected immediately, such as a corrupted
-- database.
--
-- @
-- 'alert' == 'Di.log' 'Df1.Alert'
-- @
alert
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
alert di = Di.log di Df1.Alert
{-# INLINE alert #-}

-- | Log a critical condition that could result in system failure, such as a
-- disk running out of space.
--
-- @
-- 'critical' == 'Di.log' 'Df1.Critical'
-- @
critical
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
critical di = Di.log di Df1.Critical
{-# INLINE critical #-}

-- | Log an error condition, such as an unhandled exception.
--
-- @
-- 'error' == 'Di.log' 'Df1.Error'
-- @
error
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
error di = Di.log di Df1.Error
{-# INLINE error #-}

-- | Log a warning condition, such as an exception being gracefully handled or
-- some missing configuration setting being assigned a default value.
--
-- @
-- 'warning' == 'Di.log' 'Df1.Warning'
-- @
warning
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
warning di = Di.log di Df1.Warning
{-# INLINE warning #-}

-- | Log a condition that is not an error, but should possibly be handled
-- specially.
--
-- @
-- 'notice' == 'Di.log' 'Df1.Notice'
-- @
notice
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
notice di = Di.log di Df1.Notice
{-# INLINE notice #-}

-- | Log an informational message.
--
-- @
-- 'info' == 'Di.log' 'Df1.Info'
-- @
info
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
info di = Di.log di Df1.Info
{-# INLINE info #-}

-- | Log a message intended to be useful only when deliberately debugging a
-- program.
--
-- @
-- 'debug' == 'Di.log' 'Df1.Debug'
-- @
debug
  :: MonadIO m
  => Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
debug di = Di.log di Df1.Debug
{-# INLINE debug #-}

--------------------------------------------------------------------------------
-- STM variants

-- | Log a message stating that the system is unusable.
--
-- @
-- 'emergency'' == 'Di.log'' 'Df1.Emergency'
-- @
emergency'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
emergency' natSTM di = Di.log' natSTM di Df1.Emergency
{-# INLINE emergency' #-}

-- | Log a condition that should be corrected immediately, such as a corrupted
-- database.
--
-- @
-- 'alert'' == 'Di.log'' 'Df1.Alert'
-- @
alert'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
alert' natSTM di = Di.log' natSTM di Df1.Alert
{-# INLINE alert' #-}

-- | Log a critical condition that could result in system failure, such as a
-- disk running out of space.
--
-- @
-- 'critical'' == 'Di.log'' 'Df1.Critical'
-- @
critical'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
critical' natSTM di = Di.log' natSTM di Df1.Critical
{-# INLINE critical' #-}

-- | Log an error condition, such as an unhandled exception.
--
-- @
-- 'error'' == 'Di.log'' 'Df1.Error'
-- @
error'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
error' natSTM di = Di.log' natSTM di Df1.Error
{-# INLINE error' #-}

-- | Log a warning condition, such as an exception being gracefully handled or
-- some missing configuration setting being assigned a default value.
--
-- @
-- 'warning'' == 'Di.log'' 'Df1.Warning'
-- @
warning'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
warning' natSTM di = Di.log' natSTM di Df1.Warning
{-# INLINE warning' #-}

-- | Log a condition that is not an error, but should possibly be handled
-- specially.
--
-- @
-- 'notice'' == 'Di.log'' 'Df1.Notice'
-- @
notice'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
notice' natSTM di = Di.log' natSTM di Df1.Notice
{-# INLINE notice' #-}

-- | Log an informational message.
--
-- @
-- 'info'' == 'Di.log'' 'Df1.Info'
-- @
info'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
info' natSTM di = Di.log' natSTM di Df1.Info
{-# INLINE info' #-}

-- | Log a message intended to be useful only when deliberately debugging a
-- program.
--
-- @
-- 'debug'' == 'Di.log'' 'Df1.Debug'
-- @
debug'
  :: MonadIO m
  => (forall x. STM x -> m x)
  -> Di.Di Df1.Level path Df1.Message
  -> Df1.Message
  -> m ()
debug' natSTM di = Di.log' natSTM di Df1.Debug
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

