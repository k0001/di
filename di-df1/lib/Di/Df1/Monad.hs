{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module exports an API compatible with "Di.Monad".
module Di.Df1.Monad
 ( Df1T
 , MonadDf1
   -- * Hierarchy
 , push
   -- * Metadata
 , attr
   -- * Logging
 , debug
 , info
 , notice
 , warning
 , error
 , alert
 , critical
 , emergency
   -- ** Type-inference helpers
 , debug_
 , info_
 , notice_
 , warning_
 , error_
 , alert_
 , critical_
 , emergency_
 ) where

import Prelude hiding (error)

import qualified Df1
import qualified Di.Monad as Di

--------------------------------------------------------------------------------

-- | Convenience type-synonym for a 'Di.DiT' restricted to all the /df1/
-- monomorphic types.
--
-- @
-- 'Df1T' == 'Di.DiT' 'Df1.Level' 'Df1.Path' 'Df1.Message'
--    :: (* -> *) -> * -> *
--
-- 'Df1T' m == 'Di.DiT' 'Df1.Level' 'Df1.Path' 'Df1.Message' m
--    :: * -> *
--
-- 'Df1T' m a == 'Di.DiT' 'Df1.Level' 'Df1.Path' 'Df1.Message' m a
--    :: *
-- @
--
-- This type-synonym is not used within the @di-df1@ library itself because
-- all functions exposed in the library have more general types. However,
-- users are encouraged to use 'MonadDf1' if they find it useful to reduce
-- boilerplate and improve type inferrence.
type Df1T = Di.DiT Df1.Level Df1.Path Df1.Message

-- | Convenience type-synonym for a 'Di.MonadDi' restricted to all the /df1/
-- monomorphic types.
--
-- @
-- 'MonadDf1' == 'Di.MonadDi' 'Df1.Level' 'Df1.Path' 'Df1.Message'
--    :: (* -> *) -> 'GHC.Exts.Constraint'
--
-- 'MonadDf1' m == 'Di.MonadDi' 'Df1.Level' 'Df1.Path' 'Df1.Message' m
--    :: 'GHC.Exts.Constraint'
-- @
--
-- This type-synonym is not used within the @di-df1@ library itself because
-- all functions exposed in the library have more general types. However,
-- users are encouraged to use 'MonadDf1' if they find it useful to reduce
-- boilerplate and improve type inferrence.
type MonadDf1 = Di.MonadDi Df1.Level Df1.Path Df1.Message

--------------------------------------------------------------------------------

-- | Push a new 'Df1.Segment' to the 'Di.MonadDi'.
push
  :: Di.MonadDi level Df1.Path msg m
  => Df1.Segment
  -> m a
  -> m a  -- ^
push s = Di.push (Df1.Push s)
{-# INLINE push #-}

-- | Push a new attribute 'Df1.Key' and 'Df1.Value' to the 'Di.MonadDi'.
attr
  :: (Di.MonadDi level Df1.Path msg m, Df1.ToValue value)
  => Df1.Key
  -> value
  -> m a
  -> m a -- ^
attr k v = Di.push (Df1.Attr k (Df1.value v))
{-# INLINE attr #-}

--------------------------------------------------------------------------------
-- MonadIO variants.

-- | Log a message stating that the system is unusable.
emergency
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
emergency = emergency_ . Df1.message
{-# INLINE emergency #-}

-- | Log a condition that should be corrected immediately, such as a corrupted
-- database.
alert
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
alert = alert_ . Df1.message
{-# INLINE alert #-}

-- | Log a critical condition that could result in system failure, such as a
-- disk running out of space.
critical
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
critical = critical_ . Df1.message
{-# INLINE critical #-}

-- | Log an error condition, such as an unhandled exception.
error
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
error = error_ . Df1.message
{-# INLINE error #-}

-- | Log a warning condition, such as an exception being gracefully handled or
-- some missing configuration setting being assigned a default value.
warning
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
warning = warning_ . Df1.message
{-# INLINE warning #-}

-- | Log a condition that is not an error, but should possibly be handled
-- specially.
notice
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
notice = notice_ . Df1.message
{-# INLINE notice #-}

-- | Log an informational message.
info
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
info = info_ . Df1.message
{-# INLINE info #-}

-- | Log a message intended to be useful only when deliberately debugging a
-- program.
debug
  :: (Di.MonadDi Df1.Level path Df1.Message m, Df1.ToMessage msg)
  => msg
  -> m ()
debug = debug_ . Df1.message
{-# INLINE debug #-}

--------------------------------------------------------------------------------
-- MonadIO variants with better type inference

-- | Like 'emergency', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
emergency_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
emergency_ = Di.log Df1.Emergency
{-# INLINE emergency_ #-}

-- | Like 'critical', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
critical_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
critical_ = Di.log Df1.Critical
{-# INLINE critical_ #-}

-- | Like 'alert', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
alert_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
alert_ = Di.log Df1.Alert
{-# INLINE alert_ #-}

-- | Like 'error', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
error_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
error_ = Di.log Df1.Error
{-# INLINE error_ #-}

-- | Like 'warning', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
warning_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
warning_ = Di.log Df1.Warning
{-# INLINE warning_ #-}

-- | Like 'notice', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
notice_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
notice_ = Di.log Df1.Notice
{-# INLINE notice_ #-}

-- | Like 'info', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
info_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
info_ = Di.log Df1.Info
{-# INLINE info_ #-}

-- | Like 'debug', but takes a 'Df1.Message' rather than any 'Df1.ToMessage'.
--
-- This helps with type inference in case you are trying to
-- log a literal string and have the @OverloadedStrings@ GHC extension enabled.
debug_
  :: Di.MonadDi Df1.Level path Df1.Message m
  => Df1.Message
  -> m ()
debug_ = Di.log Df1.Debug
{-# INLINE debug_ #-}
