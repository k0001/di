{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module offers a monadic alternative to the “bare” logging API offered
-- by "Di.Core".
--
-- Whereas "Di.Core" expects you to explicitly pass around a 'Di' object,
-- "Di.Monad" offers a 'MonadDi' typeclass, as well functions operating on
-- 'MonadDi' instances, as its public facing API.
--
-- "Di.Monad" exports 'MonadDi' instances for all of the monad transformer
-- types in [transformers](https://hackage.haskell.org/package/transformers)
-- and [pipes](https://hackage.haskell.org/package/pipes).
--
-- Nevertheless, be aware that these two APIs are compatible, so you may
-- choose to use the monadic API for some parts of your application, the
-- “bare” API for some other parts, and everything will compose and behave
-- as expected. Usually, 'run' is the boundary between these two 'APIs'.
--
-- "Di.Monad" also provides a 'DiT' monad transformer that has an instance
-- of the 'MonadDi' typeclass and you can readily use out of the box. 'DiT'
-- also implements instances for all of the typeclasses in
-- [base](https://hackage.haskell.org/package/base),
-- [mtl](https://hackage.haskell.org/package/mtl), and
-- [exceptions](https://hackage.haskell.org/package/mtl).
--
-- Import this module as follows:
--
-- @
-- import "Di.Core" as Di ('Di.Core.new')
-- import "Di.Monad" as Di
-- @
module Di.Monad
   ( -- * MonadDi
    MonadDi (ask)
   , diatomically
   , log
   , push
   , flush
   , filter
   , onException
   , local
   , localT
   , dilift

    -- * DiT
   , DiT (..)
   , run
   , run'

    -- * Misc
   , MonadAtomically (..)
   ) where

import Control.Applicative (Alternative (..), optional)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Monad (MonadPlus (..), ap, liftM, liftM2)
import Control.Monad.Catch qualified as Ex
import Control.Monad.Except qualified as E
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (MFunctor (hoist), MMonad (embed))
import Control.Monad.Reader qualified as Reader
import Control.Monad.State qualified as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer qualified as Writer
import Data.Sequence (Seq)
import GHC.Show (appPrec1)
import GHC.Stack (HasCallStack)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (error, filter, log)

#ifdef FLAG_unliftio_core
import Control.Monad.IO.Unlift qualified as U
#endif

#ifdef FLAG_pipes_safe
import Pipes.Safe qualified as P
#endif

#ifdef FLAG_primitive
import Control.Monad.Primitive qualified as Prim
#endif

#ifdef FLAG_transformers_base
import Control.Monad.Base qualified as B
#endif

#ifdef FLAG_monad_control
import Control.Monad.Trans.Control qualified as BC
#endif

#ifdef FLAG_resourcet
import Control.Monad.Trans.Resource.Internal qualified as R
#endif

import Di.Core (Di)
import Di.Core qualified as Di

--------------------------------------------------------------------------------
-- MonadAtomically

class (Monad m) => MonadAtomically m where
   atomically :: STM a -> m a
   default atomically :: (MonadIO m) => STM a -> m a
   atomically = \sa -> liftIO (STM.atomically sa)
   {-# INLINE atomically #-}

instance MonadAtomically STM where
   atomically = id
   {-# INLINE atomically #-}

instance MonadAtomically IO where
   atomically = STM.atomically
   {-# INLINE atomically #-}

instance
   {-# OVERLAPPABLE #-}
   (MonadTrans t, MonadAtomically m)
   => MonadAtomically (t m)
   where
   atomically = \sa -> lift (atomically sa)
   {-# INLINE atomically #-}

--------------------------------------------------------------------------------
-- DiT data

data DiT level path msg m a where
   -- | 'Bind' isn't guaranteed to be right-associated by construction. When
   -- eliminating 'DiT' into @m@, be sure to re-associate it to the right.
   Bind
      :: DiT level path msg m x
      -> (x -> DiT level path msg m a)
      -> DiT level path msg m a
   Effect
      :: m (DiT level path msg m a)
      -> DiT level path msg m a
   Pure
      :: a
      -> DiT level path msg m a
   Local
      :: (Di level path msg -> Di level' path' msg')
      -> DiT level' path' msg' m a
      -> DiT level path msg m a
   Ask
      :: DiT level path msg m (Di level path msg)

-- | Merge consecutive 'Effect' layers into a single 'Effect' layer.
pattern Effect1
   :: (Monad m)
   => m (DiT level path msg m a)
   -> DiT level path msg m a
pattern Effect1 mt <- Effect (p_Effect1 -> mt)
   where
      Effect1 mt = Effect (p_Effect1 mt)

{-# COMPLETE Effect1, Bind, Pure, Local, Ask #-}

p_Effect1
   :: (Monad m)
   => m (DiT level path msg m a)
   -> m (DiT level path msg m a)
p_Effect1 = \m0 ->
   m0 >>= \case
      Effect m1 -> p_Effect1 m1
      tb -> pure tb
{-# INLINE p_Effect1 #-}

_showsPrecDiT :: Int -> DiT level path msg m a -> ShowS
_showsPrecDiT n = \case
   Bind tx _ ->
      showParen (n >= appPrec1) $
         showString "Bind " . _showsPrecDiT appPrec1 tx . showString " _"
   Effect _ -> showParen (n >= appPrec1) $ showString "Effect _"
   Pure _ -> showParen (n >= appPrec1) $ showString "Pure _"
   Local _ tx ->
      showParen (n >= appPrec1) $
         showString "Local _ " . _showsPrecDiT appPrec1 tx
   Ask -> showString "Ask"

-- | Run a 'DiT' over @m@, correctly handing the logging of exceptions.
--
-- This function imposes a 'MonadIO' constraint on @m@ to differentiate
-- itself from 'diatomically', which is the “run” function to use in case
-- the 'DiT' is wrapping 'STM'. In case your @m@ doesn't implement 'MonadIO',
-- you can use 'run'' instead.
--
-- Please notice that 'run' doesn't perform a 'flush' on the given 'Di'
-- before returning. You are responsible for doing that (or, more likely,
-- 'Di.Core.new' will do it for you).
run
   :: forall level path msg m a
    . (Ex.MonadMask m, MonadIO m)
   => Di level path msg
   -> DiT level path msg m a
   -> m a
run = run' liftIO
{-# INLINE run #-}

-- | Like 'run', but doesn't impose a 'MonadIO' constraint on @m@.
--
-- This function is useful in case @m@ supports running 'IO', but doesn't
-- publicly offer the general-purpose 'IO'-lifting 'MonadIO'.
run'
   :: forall level path msg m a
    . (Ex.MonadMask m)
   => (forall x. IO x -> m x)
   -- ^ How to run 'IO' actions in @m@.
   -> Di level path msg
   -> DiT level path msg m a
   -> m a
run' iom = \d -> \ta -> Ex.uninterruptibleMask \r -> go r d ta
  where
   {-# INLINE go #-}
   go
      :: forall level' path' msg' b
       . (forall x. m x -> m x)
      -> Di level' path' msg'
      -> DiT level' path' msg' m b
      -> m b
   go r d = \case
      Bind tc kc -> case tc of
         -- Reassociate Binds to the to the right:
         Bind tx kx -> go r d $ Bind tx \x -> Bind (kx x) kc
         -- Inline `go r d tc` instead of looping:
         Effect1 mta -> wrapM r d mta >>= go r d >>= \a -> go r d (kc a)
         Local f ta -> go r (f d) ta >>= \a -> go r d (kc a)
         Pure a -> go r d (kc a)
         Ask -> go r d (kc d)
      Effect1 mta -> wrapM r d mta >>= \ta -> go r d ta
      Pure a -> pure a
      Ask -> pure d
      Local f ta -> go r (f d) ta
   {-# INLINE wrapM #-}
   wrapM
      :: forall level' path' msg' b
       . (forall x. m x -> m x)
      -> Di level' path' msg'
      -> m b
      -> m b
   wrapM r d mb = Ex.catch (r mb) \(e :: Ex.SomeException) ->
      Di.throw' (\sb -> iom (STM.atomically sb)) d e

--------------------------------------------------------------------------------
-- MonadDi

-- | A 'MonadDi' allows interacting with a 'Di' through a
-- [mtl](https://hackage.haskell.org/package/mtl)-like monadic API, rather
-- than through the “bare” API proposed by "Di.Core".
--
-- Nevertheless, be aware that these two APIs are compatible, so you may
-- choose to use the monadic API for some parts of your application, the
-- “bare” API for some other parts, and everything will compose and behave
-- as expected. Usually, 'run' is the boundary between these two 'APIs',
-- although not necessarily.
--
-- Semantically, @'MonadDi' m@ is a “reader monad” that carries a 'Di' as its
-- environment.
class
   (MonadAtomically m) =>
   MonadDi level path msg m
      | m -> level path msg
   where
   -- | Get the 'Di' inside @m@, unmodified.
   --
   -- Idempotence law:
   --
   -- @
   -- 'ask' '>>' 'ask'  ==  'ask'
   -- @
   ask :: m (Di level path msg)
   default ask
      :: (MonadTrans t, MonadDi level path msg n, m ~ t n)
      => m (Di level path msg)
   ask = lift ask
   {-# INLINE ask #-}

   -- `local'` is defined as a method so that the `MonadDi` instances
   -- for `DiT` can use the much more efficient `Local` constructor.
   -- It's not necessary to export it, however.
   local' :: (Di level path msg -> Di level path msg) -> m a -> m a
   local' f = \ma -> localT f (lift ma)
   {-# INLINE local' #-}

-- | Run @m a@ with a modified 'Di':
--
-- @
-- 'local' ('const' x) 'ask'  ==  'pure' x
-- @
--
-- Identity law:
--
-- @
-- 'local' 'id' x  ==  x
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
-- 'local' f ('pure' ()) '>>' x  ==  x
-- @
local
   :: forall level path msg m a
    . (MonadDi level path msg m)
   => (Di level path msg -> Di level path msg)
   -> m a
   -- ^ Computation having access to the modified 'Di'.
   -> m a
local f = local' f
{-# INLINE local #-}

-- | Like 'local', but allows modifying the @level@, @path@ and @msg@ types.
--
-- Notice that the original unmodified 'Di' can be recovered from inside 'DiT':
--
-- @
-- 'ask' == 'localT' f ('lift' 'ask')
-- @
localT
   :: forall level level' path path' msg msg' m a
    . (MonadDi level path msg m)
   => (Di level path msg -> Di level' path' msg')
   -> DiT level' path' msg' m a
   -- ^ Computation having access to the modified 'Di' through the outer 'DiT'
   -- layer, and to the original 'Di' through the inner @m@ layer.
   -> m a
localT = \f -> go f
  where
   {-# INLINE go #-}
   go
      :: forall level'' path'' msg'' b
       . (Di level path msg -> Di level'' path'' msg'')
      -> DiT level'' path'' msg'' m b
      -> m b
   go f = \case
      Bind tc0 kc -> case tc0 of
         -- Reassociate Binds to the to the right:
         Bind tx kx -> go f $ Bind tx \x -> Bind (kx x) kc
         -- Inline `go f tc` instead of looping:
         Effect1 mtmc -> mtmc >>= go f >>= \a -> go f (kc a)
         Pure c -> go f (kc c)
         Local g tc1 -> go (\d -> g (f d)) tc1 >>= \c -> go f (kc c)
         Ask -> ask >>= \d -> go f (kc (f d))
      Effect1 mtmb -> mtmb >>= go f
      Pure b -> pure b
      Local g tmb -> go (\d -> g (f d)) tmb
      Ask -> ask >>= \d -> pure (f d)

-- | Like "Control.Concurrent.STM".'STM.atomically', but it also logging
-- from within the 'STM' transaction.
--
-- The semantics of logging from within 'STM' are those of any other 'STM'
-- transaction: That is, the log messages are commited only once to the
-- outside world if and when the 'STM' transaction succeeds.
--
-- The 'Di' environment used for logging is the same as in @m@:
--
-- @
-- 'ask'  ==  'diatomically' 'ask'
-- @
diatomically
   :: forall level path msg m a
    . (MonadDi level path msg m, MonadIO m)
   => DiT level path msg STM a
   -- ^ Computation in 'STM' having access to the same logging features as @m@.
   -> m a
diatomically = \t -> do
   d <- ask
   liftIO do
      ea <- STM.atomically $ Ex.try $ go d t
      either Ex.throwM pure (ea :: Either Ex.SomeException a)
  where
   {-# INLINE go #-}
   go
      :: forall level' path' msg' b
       . Di level' path' msg'
      -> DiT level' path' msg' STM b
      -> STM b
   go d = \case
      Bind tc kc -> case tc of
         -- Reassociate Binds to the to the right:
         Bind tx kx -> go d $ Bind tx \x -> Bind (kx x) kc
         -- Inline `go d tc` instead of looping:
         Local f ta -> go (f d) ta >>= \a -> go d (kc a)
         Effect1 mta -> wrapSTM d mta >>= go d >>= \a -> go d (kc a)
         Pure a -> go d (kc a)
         Ask -> go d (kc d)
      Effect1 mta -> wrapSTM d mta >>= \ta -> go d ta
      Pure a -> pure a
      Ask -> pure d
      Local f ta -> go (f d) ta
   {-# INLINE wrapSTM #-}
   wrapSTM :: forall level' path' msg' b. Di level' path' msg' -> STM b -> STM b
   wrapSTM d mb = Ex.catch mb $ Di.throw' @Ex.SomeException id d

-- | @'dilift' ma@ will run @ma@ with the current logging environment of 't'.
--
-- @
-- 'dilift' ma    ==   'ask' >>= \d -> 'lift' ('local' ('const' d) ma)
-- @
dilift
   :: (MonadDi level path msg m, MonadDi level path msg (t m), MonadTrans t)
   => m a
   -> t m a
dilift = \ma -> ask >>= \d -> lift (local (const d) ma)
{-# INLINE dilift #-}

-- | Log a message @msg@ with the given importance @level@.
--
-- This function returns immediately after queing the message for logging. The
-- actual printing of the log message will happen in a different thread,
-- asynchronously. If you want to explicitly wait for the message to be logged,
-- then call 'flush' afterwards.
--
-- Log messages are rendered in FIFO order, and their timestamp records the
-- time when this 'log' function was called, rather than the time when the log
-- message is printed in the future.
--
-- /Note regarding exceptions:/ Any exception thrown by 'atomically' will be
-- thrown here. /Synchronous/ exceptions that happen due to failures in the
-- actual committing of the log message, which itself is performed in a
-- different thread, are ignored (they should be handled in the function passed
-- to 'new' instead). If an asynchronous exception kills the logging thread,
-- then you will synchronously get 'Di.ExceptionInLoggingWorker' here, but by
-- the time that happens, that same exception will have already already been
-- thrown asynchronously to this same thread anyway, so unless you did
-- something funny to recover from that exception, you will have died already.
log
   :: forall level path msg m
    . (MonadDi level path msg m)
   => level
   -- ^ Log level.
   -> msg
   -- ^ Log message.
   -> m ()
log l = \m -> ask >>= \d -> Di.log' atomically d l m
{-# INLINE log #-}

-- | Block until all messages being logged have finished processing.
--
-- Manually calling 'flush' is not usually necessary because all log messages
-- are processed as soon as possible, and 'with' ensures that no log message is
-- left unprocessed. However, the actual printing of log messages happens
-- asynchronously, meaning there might be log messages still waiting to be
-- processed. A call to 'flush' will block until all pending log messages have
-- been processed.
--
-- Please see 'log' to understand how exceptions behave in this function (hint:
-- they behave unsurprisingly).
flush :: forall level path msg m. (MonadDi level path msg m) => m ()
flush = ask >>= Di.flush' atomically
{-# INLINEABLE flush #-}

-- | Require that any logged messages within the given action satisfy the given
-- predicate in order to be accepted for processing. Logged messages that
-- don't satisfy the predicate will be silently discarded.
--
-- Identity:
--
-- @
-- 'filter' (\\_ _ _ -> 'True')  ==  'id'
-- @
--
-- Composition:
--
-- @
-- 'filter' (\\l ps m -> f l ps m '&&' g l ps m)  ==  'filter' f . 'filter' g
-- @
--
-- Commutativity:
--
-- @
-- 'filter' f . 'filter' g  ==  'filter' g . 'filter' f
-- @
filter
   :: forall level path msg m a
    . (MonadDi level path msg m)
   => (level -> Seq path -> msg -> Bool)
   -- ^ Whether a particular log entry with the given @level@, @path@s and
   -- @msg@ should be logged.
   --
   -- The given @path@s indicate where the 'log' call was made from, with an
   -- empty 'Seq' representing 'log' calls made at the current depth level
   -- (see 'push'). The leftmost @path@ in the 'Seq.Seq' is the most immediate
   -- child, while the rightmost is the most distand child (i.e., the @path@
   -- closest to the place where 'log' call actually took place).
   -> m a
   -> m a
filter f = local (Di.filter f)
{-# INLINE filter #-}

-- | Run the given action under a deeper @path@.
push
   :: forall level path msg m a
    . (MonadDi level path msg m)
   => path
   -> m a
   -> m a
push p = local (Di.push p)
{-# INLINE push #-}

-- | Within the passed given @m a@, exceptions will be logged as a @msg@ with a
-- particular @level@ if both the passed in function returns 'Just', and
-- 'filter' so allows it afterwards.
--
-- If the given function returns 'Nothing', then no logging is performed.
--
-- Composition:
--
-- @
-- 'onException' f . 'onException' g   ==   'onException' (g e *> f e)
-- @
--
-- Notice that the @level@ and @msg@ resulting from @g@ are discarded, yet the
-- policy regarding whether to log or not is preserved in the same way as
-- 'filter'. That is, 'onException' can't accept an exception already rejected
-- by a previous use of 'onException', but it can reject a previously accepted
-- one.
onException
   :: forall level path msg m a
    . (MonadDi level path msg m)
   => (Ex.SomeException -> Maybe (level, msg))
   -- ^ Whether to log a particular exception as a message @msg@ at
   -- a particular @level@.
   -> m a
   -- ^ Computation whose exceptions will be logged if necessary.
   -> m a
onException f = local (Di.onException f)
{-# INLINE onException #-}

--------------------------------------------------------------------------------
-- DiT instances

instance MonadDi level path msg (DiT level path msg STM) where
   ask = Ask
   {-# INLINE ask #-}
   local' = Local
   {-# INLINE local' #-}

instance MonadDi level path msg (DiT level path msg IO) where
   ask = Ask
   {-# INLINE ask #-}
   local' = Local
   {-# INLINE local' #-}

instance
   (MonadTrans t, MonadAtomically m)
   => MonadDi level path msg (DiT level path msg (t m))
   where
   ask = embed id Ask
   {-# INLINE ask #-}
   local' = Local
   {-# INLINE local' #-}

instance
   {-# OVERLAPPABLE #-}
   (MonadTrans t, MonadDi level path msg m)
   => MonadDi level path msg (t m)

instance Functor (DiT level path msg m) where
   fmap = liftM
   {-# INLINE fmap #-}

instance Applicative (DiT level path msg m) where
   pure = Pure
   {-# INLINE pure #-}
   liftA2 = liftM2
   {-# INLINE liftA2 #-}
   (<*>) = ap
   {-# INLINE (<*>) #-}
   ma *> mb = Bind ma \_ -> mb
   {-# INLINE (*>) #-}

instance (Alternative m) => Alternative (DiT level path msg m) where
   empty = liftDiT empty
   {-# INLINE empty #-}
   tl0 <|> tr = case tl0 of
      Bind tx xtl -> Bind (optional tx) (maybe tr xtl)
      Effect mtl1 -> Bind (liftDiT (optional mtl1)) \case
         Just tl1 -> tl1 <|> tr
         Nothing -> tr
      Local f g -> Bind (Local f (optional g)) (maybe tr pure)
      Pure l -> Pure l
      Ask -> Ask

instance (MonadPlus m) => MonadPlus (DiT level path msg m) where
   mzero = liftDiT mzero
   {-# INLINE mzero #-}
   mplus tl0 tr = case tl0 of
      Bind tx xtl -> Bind (moptional tx) (maybe tr xtl)
      Effect mtl1 -> Bind (liftDiT (moptional mtl1)) \case
         Just tl1 -> mplus tl1 tr
         Nothing -> tr
      Pure l -> Pure l
      Local f g -> Bind (Local f (moptional g)) (maybe tr pure)
      Ask -> Ask

instance Monad (DiT level path msg m) where
   return = pure
   {-# INLINE return #-}
   (>>=) = Bind -- Notice that we reassociate to during DiT elimination.
   {-# INLINE (>>=) #-}

instance (MonadFail m) => MonadFail (DiT level path msg m) where
   fail = \s -> lift (fail s)
   {-# INLINE fail #-}

instance
   forall e level path msg m
    . (Reader.MonadReader e m)
   => Reader.MonadReader e (DiT level path msg m)
   where
   ask = lift Reader.ask
   {-# INLINE ask #-}
   local r = go id
     where
      {-# INLINE go #-}
      go
         :: forall level' path' msg' b
          . (Di level path msg -> Di level' path' msg')
         -> DiT level' path' msg' m b
         -> DiT level path msg m b
      go f = \case
         Bind tx xtb -> Bind (go f tx) \x -> go f (xtb x)
         Effect mtb -> Effect (go f <$> Reader.local r mtb)
         Local g tb -> go (\d -> g (f d)) tb
         tb -> unsafeCoerce tb -- safe

instance (State.MonadState s m) => State.MonadState s (DiT level path msg m) where
   get = lift State.get
   {-# INLINE get #-}
   put = \s -> lift (State.put s)
   {-# INLINE put #-}
   state = \f -> lift (State.state f)
   {-# INLINE state #-}

instance
   (Writer.MonadWriter w m)
   => Writer.MonadWriter w (DiT level path msg m)
   where
   tell = \w -> lift (Writer.tell w)
   {-# INLINE tell #-}
   writer = \aw -> lift (Writer.writer aw)
   {-# INLINE writer #-}
   listen = \case
      Bind tx xtb -> Bind (Writer.listen tx) \ ~(x, wx) ->
         Bind (Writer.listen (xtb x)) \ ~(b, wb) ->
            Pure (b, wx <> wb)
      Effect mtb -> Effect (fmap Writer.listen mtb)
      Pure b -> lift (Writer.listen (pure b))
      Local f tb -> Local f (Writer.listen tb)
      Ask -> Ask >>= \d -> lift (Writer.listen (pure d))
   pass = \case
      Bind tx xtb -> Bind tx \x -> Bind (xtb x) \bww ->
         lift (Writer.pass (pure bww))
      Effect mtb -> Effect (fmap Writer.pass mtb)
      Pure bww -> lift (Writer.pass (pure bww))
      Local f tb -> Local f (Writer.pass tb)

instance (E.MonadError e m) => E.MonadError e (DiT level path msg m) where
   throwError = \e -> lift (E.throwError e)
   {-# INLINE throwError #-}
   catchError (Bind tx xtb) h = Bind (E.tryError tx) \case
      Right x -> E.catchError (xtb x) h
      Left e -> h e
   catchError (Effect1 mtb) h =
      Effect $ E.catchError (E.handleError h <$> mtb) (\e -> pure (h e))
   catchError (Pure b) _ = Pure b
   catchError (Local f tb) h = Bind (Local f (E.tryError tb)) \case
      Right x -> pure x
      Left e -> h e
   catchError Ask _ = Ask

instance MonadTrans (DiT level path msg) where
   lift = liftDiT
   {-# INLINE lift #-}

-- | Like 'lift', but doesn't require a @'Monad' m@ constraint.
liftDiT :: (Functor m) => m a -> DiT level path msg m a
liftDiT = \ma -> Effect (fmap Pure ma)
{-# INLINE liftDiT #-}

instance (Ex.MonadThrow m) => Ex.MonadThrow (DiT level path msg m) where
   throwM = \e -> lift (Ex.throwM e)
   {-# INLINE throwM #-}

instance (Ex.MonadCatch m) => Ex.MonadCatch (DiT level path msg m) where
   catch
      :: forall e a
       . (HasCallStack, Ex.Exception e)
      => DiT level path msg m a
      -> (e -> DiT level path msg m a)
      -> DiT level path msg m a
   catch (Bind tx xtb) h = Bind (Ex.try @_ @e tx) \case
      Right x -> Ex.catch (xtb x) h
      Left e -> h e
   catch (Effect1 mtb) h =
      Effect $ Ex.catch (Ex.handle h <$> mtb) (\e -> pure (h e))
   catch (Pure b) _ = Pure b
   catch (Local f tb) h = Bind (Local f (Ex.try @_ @e tb)) \case
      Right x -> pure x
      Left e -> h e
   catch Ask _ = Ask

{-# INLINE [1] liftMask #-}
liftMask
   :: forall level path msg m r
    . (Monad m)
   => (forall s. ((forall x. m x -> m x) -> m s) -> m s)
   -> ( (forall x. DiT level path msg m x -> DiT level path msg m x)
        -> DiT level path msg m r
      )
   -> DiT level path msg m r
liftMask maskM = \k ->
   Effect $ maskM $ \restoreM ->
      pure $ maskD_ $ k $ restoreD restoreM
  where
   {-# INLINE [0] restoreD #-}
   restoreD
      :: forall level' path' msg' b
       . (forall x. m x -> m x)
      -> DiT level' path' msg' m b
      -> DiT level' path' msg' m b
   restoreD rM = \case
      Bind tx xtb -> Bind (restoreD rM tx) \x -> restoreD rM (xtb x)
      Effect1 mtb -> Effect (restoreD rM <$> rM mtb)
      Local f tb -> Local f (restoreD rM tb)
      tb -> tb
   {-# INLINE [0] maskD_ #-}
   maskD_
      :: forall level' path' msg' b
       . DiT level' path' msg' m b
      -> DiT level' path' msg' m b
   maskD_ = \case
      Bind tx xtb -> Bind (maskD_ tx) \x -> maskD_ (xtb x)
      Effect1 mtb -> Effect (maskM \_ -> fmap maskD_ mtb)
      Local f tb -> Local f (maskD_ tb)
      tb -> tb

instance (Ex.MonadMask m) => Ex.MonadMask (DiT level path msg m) where
   mask = liftMask Ex.mask
   {-# NOINLINE mask #-}
   uninterruptibleMask = liftMask Ex.uninterruptibleMask
   {-# NOINLINE uninterruptibleMask #-}
   generalBracket acq rel use = Ex.mask $ \restore -> do
      a <- acq
      b <- Ex.catch (restore (use a)) \e -> do
         _ <- rel a $ Ex.ExitCaseException e
         Ex.throwM e
      c <- rel a $ Ex.ExitCaseSuccess b
      pure (b, c)

instance (MonadIO m) => MonadIO (DiT level path msg m) where
   liftIO = \ioa -> lift (liftIO ioa)
   {-# INLINE liftIO #-}

instance (Semigroup a) => Semigroup (DiT level path msg m a) where
   (<>) = liftM2 (<>)
   {-# INLINE (<>) #-}

instance (Monoid a) => Monoid (DiT level path msg m a) where
   mempty = Pure mempty
   {-# INLINE mempty #-}

instance MFunctor (DiT level path msg) where
   hoist
      :: forall m n a
       . (Functor m)
      => (forall x. m x -> n x)
      -> DiT level path msg m a
      -> DiT level path msg n a
   hoist mn = go
     where
      {-# INLINE go #-}
      go
         :: forall level' path' msg' b
          . DiT level' path' msg' m b
         -> DiT level' path' msg' n b
      go = \case
         Bind tx xtmb -> Bind (go tx) \x -> go (xtmb x)
         Effect mtmb -> Effect (mn (fmap go mtmb))
         Local f tmb -> Local f (go tmb)
         tmb -> unsafeCoerce tmb -- safe

instance MMonad (DiT level path msg) where
   embed
      :: forall m n a
       . (forall x. m x -> DiT level path msg n x)
      -> DiT level path msg m a
      -> DiT level path msg n a
   embed mtn = go id
     where
      {-# INLINE go #-}
      go
         :: forall level' path' msg' b
          . (Di level path msg -> Di level' path' msg')
         -> DiT level' path' msg' m b
         -> DiT level path msg n b
      go f = \case
         Bind tx xtmb -> Bind (go f tx) \x -> go f (xtmb x)
         Effect mtmb -> Bind (mtn mtmb) (go f)
         Pure b -> Pure b
         Local g tmb -> go (\d -> g (f d)) tmb
         Ask -> Ask >>= \d -> Pure (f d)

-- | TODO Does this work?
instance (MonadFix m) => MonadFix (DiT level path msg m) where
   mfix :: forall a. (a -> DiT level path msg m a) -> DiT level path msg m a
   mfix kt = ta
     where
      ta :: DiT level path msg m a
      ta = lift (go id ta) >>= kt
      go
         :: forall level' path' msg' b
          . (Di level path msg -> Di level' path' msg')
         -> DiT level' path' msg' m b
         -> m b
      go f tb = mfix \b1 -> case tb of
         Bind tc0 kc -> case tc0 of
            -- Reassociate Binds to the to the right:
            Bind tx kx -> go f $ Bind tx \x -> Bind (kx x) kc
            -- Inline `go f tc` instead of looping:
            Effect1 mtmc -> mtmc >>= go f >>= \a -> go f (kc a)
            Pure c -> go f (kc c)
            Local g tc1 -> go (\d -> g (f d)) tc1 >>= \c -> go f (kc c)
            Ask -> go f Ask >>= \c -> go f (kc c)
         Effect1 mtb -> mtb >>= \tb1 -> go f tb1
         Pure b0 -> pure b0
         Local g tb1 -> go (\d -> g (f d)) tb1
         Ask -> pure b1

{- TODO
instance (MonadCont m) => MonadCont (DiT level path msg m) where
   callCC
      :: forall a b
       . ((a -> DiT level path msg m b) -> DiT level path msg m a)
      -> DiT level path msg m a
-}

{- TODO
instance (MonadZip m) => MonadZip (DiT level path msg m) where
   mzipWith
      :: (a -> b -> c)
      -> DiT level path msg m a
      -> DiT level path msg m b
      -> DiT level path msg m c
   mzipWith g (Effect1 mta) (Effect1 mtb) =
      Effect $ mzipWith (mzipWith g) mta mtb
   mzipWith g ta@(Effect _) tb@(Pure _) = mzipWith g ta (Effect (pure tb))
   mzipWith g ta@(Pure _) tb@(Effect _) = mzipWith g (Effect (pure ta)) tb
   mzipWith g (Pure a) (Pure b) = lift $ mzipWith g (pure a) (pure b)
   mzipWith g Ask tb = Ask >>= \a -> mzipWith g (Pure a) tb
   mzipWith g ta Ask = Ask >>= \b -> mzipWith g ta (Pure b)
-}

#ifdef FLAG_primitive
instance (Prim.PrimMonad m) => Prim.PrimMonad (DiT level path msg m) where
   type PrimState (DiT level path msg m) = Prim.PrimState m
   primitive = \op -> lift (Prim.primitive op)
   {-# INLINE primitive #-}
#endif

#ifdef FLAG_unliftio_core
instance (U.MonadUnliftIO m) => U.MonadUnliftIO (DiT level path msg m) where
   withRunInIO ktmb = do
      d <- Ask
      lift $ U.withRunInIO \mio -> ktmb \tma ->
         -- We could use `mio (run d tma)` instead.
         -- However, that requires `MonadMask m`.
         run d $ hoist mio tma
#endif

#ifdef FLAG_pipes_safe
instance (P.MonadSafe m) => P.MonadSafe (DiT level path msg m) where
   type Base (DiT level path msg m) = P.Base m
   liftBase = \ba -> lift (P.liftBase ba)
   {-# INLINE liftBase #-}
   register = \ba -> lift (P.register ba)
   {-# INLINE register #-}
   release = \m -> lift (P.release m)
   {-# INLINE release #-}
#endif

#ifdef FLAG_resourcet
instance (R.MonadResource m) => R.MonadResource (DiT level path msg m) where
   liftResourceT = \ra -> lift (R.liftResourceT ra)
   {-# INLINE liftResourceT #-}
#endif

#ifdef FLAG_transformers_base
instance (B.MonadBase b m) => B.MonadBase b (DiT level path msg m) where
   liftBase = \ba -> lift (B.liftBase ba)
   {-# INLINE liftBase #-}
#endif

#ifdef FLAG_monad_control
instance
   forall level path msg m
    . (BC.MonadBaseControl IO m, Ex.MonadMask m)
   => BC.MonadBaseControl IO (DiT level path msg m)
   where
   type StM (DiT level path msg m) a = BC.StM m a
   liftBaseWith
      :: (BC.RunInBase (DiT level path msg m) IO -> IO a)
      -> DiT level path msg m a
   liftBaseWith f = do
      d <- Ask
      Effect $ BC.liftBaseWith \g -> do
         let rb :: BC.RunInBase (DiT level path msg m) IO
             rb = \tx -> g (run' B.liftBase d tx)
         pure <$> f rb
   restoreM :: BC.StM m a -> DiT level path msg m a
   restoreM = \stma -> lift (BC.restoreM stma)
   {-# INLINE restoreM #-}

{- TODO
instance
    (BC.MonadBaseControl STM m)
   => BC.MonadBaseControl STM (DiT level path msg m)
-}
#endif

--------------------------------------------------------------------------------
-- Misc

-- | Like 'optional', but uses 'mplus' instead of '(<|>)'.
moptional :: (MonadPlus m) => m a -> m (Maybe a)
moptional ma = mplus (fmap Just ma) (pure Nothing)
{-# INLINE moptional #-}
