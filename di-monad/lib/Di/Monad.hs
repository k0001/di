{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
-- as expected. Usually, 'runDiT' is the boundary between these two 'APIs'.
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
    MonadDi (ask, local, natSTM)
   , log
   , flush
   , push
   , filter
   , throw
   , onException

    -- * DiT
   , DiT
   , diT
   , runDiT
   , runDiT'
   , hoistDiT
   , localDiT
   ) where

import Control.Applicative (Alternative)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (MonadPlus)
import Control.Monad.Catch qualified as Ex
import Control.Monad.Catch.Pure qualified as Ex
import Control.Monad.Cont (ContT (ContT), MonadCont)
import Control.Monad.Except (ExceptT (ExceptT), MonadError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Lazy qualified as RWSL
import Control.Monad.RWS.Strict qualified as RWSS
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT))
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (MonadState)
import Control.Monad.State.Lazy qualified as SL
import Control.Monad.State.Strict qualified as SS
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT (IdentityT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Lazy qualified as WL
import Control.Monad.Writer.Strict qualified as WS
import Control.Monad.Zip (MonadZip)
import Data.Sequence (Seq)
import Prelude hiding (error, filter, log)

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum (AccumT(AccumT))
import Control.Monad.Trans.Select (SelectT(SelectT))
#endif

#ifdef FLAG_unliftio_core
import Control.Monad.IO.Unlift (MonadUnliftIO)
#endif

#ifdef FLAG_pipes
import Pipes qualified as P
import Pipes.Internal qualified as P
#endif

#ifdef FLAG_pipes_safe
import Pipes.Safe qualified as P
#endif

#ifdef FLAG_primitive
import Control.Monad.Primitive (PrimMonad)
#endif

#ifdef FLAG_transformers_base
import Control.Monad.Base (MonadBase)
#endif

#ifdef FLAG_monad_control
import Control.Monad.Trans.Control (MonadBaseControl)
#endif

#ifdef FLAG_streaming
import Streaming.Internal qualified as S
#endif

#ifdef FLAG_resourcet
import Control.Monad.Trans.Resource.Internal qualified as R
#endif

#ifdef FLAG_conduit
import Data.Conduit.Internal qualified as C
#endif

import Di.Core (Di)
import Di.Core qualified as Di

--------------------------------------------------------------------------------

-- | Natural transformation from @f@ to @g@.
newtype H f g = H (forall x. f x -> g x)

-- | A @'DiT' level path msg m@ is a “reader monad” that carries as its
-- environment a @'Di' level path msg@ and natural transformation from 'STM' to
-- @m@.
--
-- The most primitive way to build a 'DiT' is through 'diT'.
--
-- The most primitive way to run a 'DiT' is through 'runDiT''.
newtype DiT level path msg m a
   = DiT (ReaderT (Di level path msg, H STM m) m a)
   deriving
      ( Functor
      , Applicative
      , Alternative
      , Monad
      , MonadIO
      , MonadFail
      , MonadFix
      , MonadZip
      , MonadPlus
      , MonadCont
      , MonadState s
      , MonadWriter w
      , MonadError e

#ifdef FLAG_unliftio_core
      , MonadUnliftIO
#endif

#ifdef FLAG_pipes_safe
      , P.MonadSafe
#endif

#ifdef FLAG_primitive
      , PrimMonad
#endif

#ifdef FLAG_transformers_base
      , MonadBase b
#endif

#ifdef FLAG_monad_control
      , MonadBaseControl b
#endif

      )

-- | Build a 'DiT'.
--
-- @
-- forall nat di.
--    'runDiT'' nat di ('diT' (\\nat' di' -> pure (nat', di')))
--        == 'pure' (nat, di)
-- @
diT
   :: ((forall x. STM x -> m x) -> Di level path msg -> m a)
   -> DiT level path msg m a
diT f = DiT (ReaderT (\(di, H nat) -> f nat di))
{-# INLINE diT #-}

instance MonadTrans (DiT level path msg) where
   lift = \x -> DiT (lift x)
   {-# INLINE lift #-}

-- | Run a 'DiT'.
--
-- @
-- forall di.
--    'runDiT' di ('diT' (\\nat' di' -> pure (nat', di')))
--        == 'pure' ('natSTM', di)
-- @
--
-- This is like 'runDiT'', but specialized to run with an underlying 'MonadIO'.
--
-- @
-- 'runDiT'  ==  'runDiT'' ('liftIO' . 'atomically')
-- @
--
-- Please notice that 'runDiT' doesn't perform a 'flush' on the given 'Di'
-- before returning. You are responsible for doing that (or, more likely,
-- 'Di.Core.new' will do it for you).
--
-- Also, notice that 'runDiT' is a /monad morphism/ from @'DiT' m@ to @m@.
runDiT
   :: (MonadIO m)
   => Di level path msg
   -> DiT level path msg m a
   -> m a
runDiT = runDiT' (\x -> liftIO (atomically x))
{-# INLINE runDiT #-}

-- | Run a 'DiT'.
--
-- @
-- forall nat di.
--    'runDiT'' nat di ('diT' (\\nat' di' -> pure (nat', di')))
--        == 'pure' (nat, di)
-- @
--
-- 'runDiT'' is like 'runDiT'. However it doesn't require a 'MonadIO'
-- constraint. Instead, it takes the natural transformation that will be used
-- by 'natSTM' as an argument.
--
-- First, this allows any monad that wraps 'IO' without necessarily having a
-- 'MonadIO' instance to work with 'MonadDi'. For example:
--
-- @
-- newtype Foo = Foo ('IO' a)
--   deriving ('Functor', 'Applicative', 'Monad')
--
-- 'runDiT'' (Foo . 'atomically')
--      :: 'Di' level path msg
--      -> 'DiT' level path msg Foo a
--      -> Foo a
-- @
--
-- Second, this allows @m@ to be 'STM' itself:
--
-- @
-- 'runDiT'' 'id'
--      :: 'Di' level path msg
--      -> 'DiT' level path msg 'STM' a
--      -> 'STM' a
-- @
--
-- The semantics of logging from within 'STM' are those of any other 'STM'
-- transaction: That is, a log message is commited only once to the outside
-- world if and when the 'STM' transaction succeeds. That is, the following
-- example will only ever commit the log containing @ly@ and @my@, and not
-- the one containing @lx@ and @mx@.
--
-- @
-- 'atomically' $ 'runDiT'' 'id' $ do
--    ('log' 'id' lx mx >> 'lift' 'Control.Concurrent.STM.retry') \<|>
--    ('log' 'id' ly my)
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
--      :: 'Di' level path msg
--      -> 'DiT' level path msg Bar a
--      -> Bar a
-- @
--
-- Additionally, notice that 'runDiT'' itself is a /monad morphism/ from
-- @'DiT' level path msg m@ to @m@ which doesn't perform any side effects
-- of its own. Particularly, the given 'Di' remains unaffected. So you can use
-- it as many times you want.
--
-- @
-- forall f di x.
--    'runDiT'' f di ('lift' x)  ==  x
-- @
--
-- Please notice that 'runDiT' doesn't perform a 'flush' on the given 'Di'
-- before returning. You are responsible for doing that (or, more likely,
-- 'Di.Core.new' will do it for you).
runDiT'
   :: (forall x. STM x -> m x)
   -> Di level path msg
   -> DiT level path msg m a
   -> m a
runDiT' h = \di -> \(DiT (ReaderT f)) -> f (di, H h)
{-# INLINE runDiT' #-}

-- | Lift a monad morphism from @m@ to @n@ to a monad morphism from
-- @'DiT' level path msg m@ to @'DiT' level path msg n@.
--
-- Notice that 'DiT' itself is not a functor in the category of monads,
-- so it can't be an instance of 'Control.Monad.Morph.MFunctor' from the
-- [mmorph](https://hackage.haskell.org/package/mmorph) package.
-- However, it becomes one if you pair it with a natural transformation
-- @'nat' :: forall x. n x -> m x@. That is:
--
-- @
-- forall nat.  /such that `nat` is a natural transformation/
--    'hoistDiT' nat  ==  'Control.Monad.Morph.hoist'
-- @
--
-- In practical terms, it means that most times you can “hoist” a 'DiT'
-- anyway, just not through 'Control.Monad.Morph.hoist'.
hoistDiT
   :: (forall x. n x -> m x)
   -- ^ Natural transformation from @n@ to @m@.
   -> (forall x. m x -> n x)
   -- ^ Monad morphism from @m@ to @n@.
   -> (DiT level path msg m a -> DiT level path msg n a)
   -- ^ Monad morphism from @'DiT' m@ to @'DiT' n@.
{-# INLINE hoistDiT #-}
hoistDiT hgf hfg = \(DiT (ReaderT f)) ->
   DiT (ReaderT (\(di, H hstmg) ->
      hfg (f (di, H (\stm -> hgf (hstmg stm))))))

-- | Run a 'DiT' with a modified 'Di':
--
-- @
-- 'localDiT' ('const' x) 'ask'  ==  'pure' x
-- @
--
-- Notice that, contrary to 'local', this allows changing the type of 'Di',
-- which means that you can use 'localDiT' with 'Di.Core.contralevel',
-- 'Di.Core.contrapath' or 'Di.Core.contramsg' to change the types of @level@,
-- @path@, or @message@ you 'DiT' works with.
--
-- @
-- 'localDiT' ('Di.Core.contralevel' (f :: level -> level'))
--     :: 'DiT' level' path msg m a
--     -> 'DiT' level path msg m a
-- @
--
-- @
-- 'localDiT' ('Di.Core.contrapath' (f :: path -> path'))
--     :: 'DiT' level path' msg m a
--     -> 'DiT' level path msg m a
-- @
--
-- @
-- 'localDiT' ('Di.Core.contramsg' (f :: msg -> msg'))
--     :: 'DiT' level path msg' m a
--     -> 'DiT' level path msg m a
-- @
--
-- Identity law:
--
-- @
-- 'localDiT' 'id' x  ==  x
-- @
--
-- Distributive law:
--
-- @
-- 'localDiT' f '.' 'localDiT' g  ==  'localDiT' (f '.' g)
-- @
--
-- Idempotence law:
--
-- @
-- 'localDiT' f ('pure' ()) '>>' x  ==  x
-- @
localDiT
   :: (Di level path msg -> Di level' path' msg')
   -> DiT level' path' msg' m a
   -> DiT level path msg m a
localDiT f = \(DiT (ReaderT gma)) ->
   DiT (ReaderT (\(di, h) -> gma (f di, h)))
{-# INLINE localDiT #-}

instance (MonadReader r m) => MonadReader r (DiT level path msg m) where
   ask = DiT (ReaderT (\_ -> Reader.ask))
   {-# INLINE ask #-}
   local f = \(DiT (ReaderT gma)) ->
      DiT (ReaderT (\di -> Reader.local f (gma di)))
   {-# INLINE local #-}

-- | Throw an 'Ex.Exception' from the underlying @m@, without logging it.
--
-- If you want to log the 'Ex.Exception' as you throw it, use 'throw' instead.
instance (Ex.MonadThrow m) => Ex.MonadThrow (DiT level path msg m) where
   throwM e = DiT (ReaderT (\_ -> Ex.throwM e))
   {-# INLINE throwM #-}

instance (Ex.MonadCatch m) => Ex.MonadCatch (DiT level path msg m) where
   catch (DiT (ReaderT f)) = \g ->
      DiT $ ReaderT $ \x ->
         Ex.catch (f x) (\e -> let DiT (ReaderT h) = g e in h x)
   {-# INLINE catch #-}

instance (Ex.MonadMask m) => Ex.MonadMask (DiT level path msg m) where
   mask f =
      DiT $ ReaderT $ \x -> Ex.mask $ \u ->
         case f (\(DiT (ReaderT g)) -> DiT (ReaderT (u . g))) of
            DiT (ReaderT h) -> h x
   {-# INLINE mask #-}
   uninterruptibleMask f =
      DiT $ ReaderT $ \x -> Ex.uninterruptibleMask $ \u ->
         case f (\(DiT (ReaderT g)) -> DiT (ReaderT (u . g))) of
            DiT (ReaderT h) -> h x
   {-# INLINE uninterruptibleMask #-}
   generalBracket acq rel use =
      DiT $ ReaderT $ \x -> Ex.generalBracket
         (case acq of DiT (ReaderT m) -> m x)
         (\res ec -> case rel res ec of DiT (ReaderT m) -> m x)
         (\res -> case use res of DiT (ReaderT m) -> m x)
   {-# INLINEABLE generalBracket #-}

--------------------------------------------------------------------------------

-- | A 'MonadDi' allows interacting with a 'Di' through a
-- [mtl](https://hackage.haskell.org/package/mtl)-like monadic API, rather
-- than through the “bare” API proposed by "Di.Core".
--
-- Nevertheless, be aware that these two APIs are compatible, so you may
-- choose to use the monadic API for some parts of your application, the
-- “bare” API for some other parts, and everything will compose and behave
-- as expected. Usually, 'runDiT' is the boundary between these two 'APIs',
-- although not necessarily.
--
-- Semantically, @'MonadDi' m@ is a “reader monad” that carries as its
-- environment a 'Di' and natural transformation from 'STM' to @m@.
class (Monad m) => MonadDi level path msg m | m -> level path msg where
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
   local :: (Di level path msg -> Di level path msg) -> m a -> m a

   -- | Natural transformation from 'STM' to @m@.
   --
   -- Notice that /it is not necessary/ for this natural transformation to be a
   -- monad morphism as well. That is, 'atomically' is acceptable.
   natSTM :: STM a -> m a
   default natSTM
      :: (MonadTrans t, MonadDi level path msg n, m ~ t n)
      => STM a
      -> m a
   natSTM = \x -> lift (natSTM x)
   {-# INLINE natSTM #-}

instance (Monad m) => MonadDi level path msg (DiT level path msg m) where
   ask = DiT (ReaderT (\(di, _) -> pure di))
   {-# INLINE ask #-}
   natSTM = \x -> DiT (ReaderT (\(_, H h) -> h x))
   {-# INLINE natSTM #-}
   local f = localDiT f
   {-# INLINE local #-}

instance
   (MonadDi level path msg m)
   => MonadDi level path msg (ReaderT r m)
   where
   local f = \(ReaderT gma) -> ReaderT (\r -> local f (gma r))
   {-# INLINE local #-}

instance
   (MonadDi level path msg m)
   => MonadDi level path msg (SS.StateT s m)
   where
   local f = \(SS.StateT gma) -> SS.StateT (\s -> local f (gma s))
   {-# INLINE local #-}

instance
   (MonadDi level path msg m)
   => MonadDi level path msg (SL.StateT s m)
   where
   local f = \(SL.StateT gma) -> SL.StateT (\s -> local f (gma s))
   {-# INLINE local #-}

instance
   (Monoid w, MonadDi level path msg m)
   => MonadDi level path msg (WS.WriterT w m)
   where
   local f = \(WS.WriterT ma) -> WS.WriterT (local f ma)
   {-# INLINE local #-}

instance
   (Monoid w, MonadDi level path msg m)
   => MonadDi level path msg (WL.WriterT w m)
   where
   local f = \(WL.WriterT ma) -> WL.WriterT (local f ma)
   {-# INLINE local #-}

instance (MonadDi level path msg m)
   => MonadDi level path msg (MaybeT m) where
   local f = \(MaybeT ma) -> MaybeT (local f ma)
   {-# INLINE local #-}

instance (MonadDi level path msg m)
   => MonadDi level path msg (ExceptT e m) where
   local f = \(ExceptT ma) -> ExceptT (local f ma)
   {-# INLINE local #-}

instance (MonadDi level path msg m)
   => MonadDi level path msg (IdentityT m) where
   local f = \(IdentityT ma) -> IdentityT (local f ma)
   {-# INLINE local #-}

instance (MonadDi level path msg m)
   => MonadDi level path msg (ContT r m) where
   local f = \(ContT gma) -> ContT (\r -> local f (gma r))
   {-# INLINE local #-}

instance
   (Monoid w, MonadDi level path msg m)
   => MonadDi level path msg (RWSS.RWST r w s m)
   where
   local f = \(RWSS.RWST gma) -> RWSS.RWST (\r s -> local f (gma r s))
   {-# INLINE local #-}

instance
   (Monoid w, MonadDi level path msg m)
   => MonadDi level path msg (RWSL.RWST r w s m)
   where
   local f = \(RWSL.RWST gma) -> RWSL.RWST (\r s -> local f (gma r s))
   {-# INLINE local #-}

#if MIN_VERSION_transformers(0,5,3)
instance (Monoid w, MonadDi level path msg m)
  => MonadDi level path msg (AccumT w m) where
  local f = \(AccumT gma) -> AccumT (\w -> local f (gma w))
  {-# INLINE local #-}

instance MonadDi level path msg m => MonadDi level path msg (SelectT r m) where
  local f = \(SelectT gma) -> SelectT (\r -> local f (gma r))
  {-# INLINE local #-}
#endif

instance MonadDi level path msg m => MonadDi level path msg (Ex.CatchT m) where
  local f = \(Ex.CatchT m) -> Ex.CatchT (local f m)
  {-# INLINE local #-}

#ifdef FLAG_pipes
instance
   (MonadDi level path msg m)
   => MonadDi level path msg (P.Proxy a' a b' b m)
   where
   {-# INLINEABLE local #-}
   local f = \case
      P.Request a' fa -> P.Request a' (\a -> local f (fa a))
      P.Respond b fb' -> P.Respond b (\b' -> local f (fb' b'))
      P.Pure r -> P.Pure r
      P.M m -> P.M (local f m >>= \r -> pure (local f r))

instance (MonadDi level path msg m) => MonadDi level path msg (P.ListT m) where
   {-# INLINE local #-}
   local f = \(P.Select p) -> P.Select (local f p)
#endif

#ifdef FLAG_pipes_safe
{- TODO: Needs the following upstream patch:
           https://github.com/Gabriella439/Haskell-Pipes-Safe-Library/pull/62
instance (MonadDi level path msg m) => MonadDi level path msg (P.SafeT m) where
   {-# INLINE local #-}
   local f = \(P.SafeT g) -> P.SafeT (local f g)
-}
#endif

#ifdef FLAG_streaming
instance
   (MonadDi level path msg m, Functor f)
   => MonadDi level path msg (S.Stream f m)
   where
   {-# INLINEABLE local #-}
   local g = \case
      S.Step fs -> S.Step (local g <$> fs)
      S.Effect ms -> S.Effect (local g <$> ms)
      S.Return r -> S.Return r
#endif

#ifdef FLAG_resourcet
instance R.MonadResource m => R.MonadResource (DiT level path msg m) where
   {-# INLINE liftResourceT #-}
   liftResourceT = lift . R.liftResourceT

instance
   (MonadDi level path msg m)
   => MonadDi level path msg (R.ResourceT m)
   where
   {-# INLINE local #-}
   local f = \(R.ResourceT g) -> R.ResourceT (local f . g)
#endif

#ifdef FLAG_conduit
instance
   (MonadDi level path msg m)
   => MonadDi level path msg (C.Pipe l i o u m)
   where
   {-# INLINEABLE local #-}
   local f = \case
     C.HaveOutput p o -> C.HaveOutput (local f p) o
     C.NeedInput p c -> C.NeedInput (local f . p) (local f . c)
     C.Done x -> C.Done x
     C.PipeM mp -> C.PipeM (local f <$> local f mp)
     C.Leftover p i -> C.Leftover (local f p) i

instance
   (MonadDi level path msg m)
   => MonadDi level path msg (C.ConduitT i o m)
   where
   {-# INLINE local #-}
   local f = \(C.ConduitT k) -> C.ConduitT (local f . k)
#endif

--------------------------------------------------------------------------------

-- | Log a message with the given importance @level@.
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
-- /Note regarding exceptions:/ Any exception thrown by 'natSTM' will be thrown
-- here. /Synchronous/ exceptions that happen due to failures in the actual
-- committing of the log message, which itself is performed in a different
-- thread, are ignored (they should be handled in the function passed to 'new'
-- instead). If an asynchronous exception kills the logging thread, then you
-- will synchronously get 'Di.ExceptionInLoggingWorker' here, but by the time
-- that happens, that same exception will have already already been thrown
-- asynchronously to this same thread anyway, so unless you did something funny
-- to recover from that exception, you will have died already.
log :: (MonadDi level path msg m) => level -> msg -> m ()
log l = \m -> ask >>= \di -> Di.log' natSTM di l m
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
flush :: (MonadDi level path msg m) => m ()
flush = Di.flush' natSTM =<< ask
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
   :: (MonadDi level path msg m)
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
push :: (MonadDi level path msg m) => path -> m a -> m a
push p = local (Di.push p)
{-# INLINE push #-}

-- | Within the passed given @m a@, exceptions thrown with 'throw' could could
-- be logged as a @msg@ with a particular @level@ if both the passed in function
-- returns 'Just', and 'filter' so allows it afterwards.
--
-- If the given function returns 'Nothing', then no logging is performed.
--
-- The returned @'Seq.Seq' path@ will extend the 'path' at the 'throw' call site
-- before sending the log. The leftmost @path@ is closest to the root.
--
-- Composition:
--
-- @
-- 'onException' f . 'onException' g   ==   'onException' (g e *> f e)
-- @
--
-- Notice that the @level@, @path@s and @msg@ resulting from @g@ are discarded,
-- yet its policy regarding whether to log or not is preserved in the same way
-- as 'filter'. That is, 'onException' can't accept an exception already
-- rejected by a previous use of 'onException', but it can reject a previously
-- accepted one.
onException
   :: (MonadDi level path msg m)
   => (Ex.SomeException -> Maybe (level, Seq path, msg))
   -> m a
   -> m a
onException f = local (Di.onException f)

-- | Throw an 'Ex.Exception', but not without logging it first according to the
-- rules established by 'onException', and further restricted by the rules
-- established by 'filter'.
--
-- If the exception doesn't need to be logged, according to the policy set with
-- 'onException', then this function behaves just as
-- 'Control.Concurrent.STM.throwSTM'.
--
-- WARNING: Note that when `m` is `STM`, or ultimately runs on 'STM', then
-- 'throw' /will not log/ the exception, just throw it. This might change in
-- the future if we figure out how to make it work safely.
throw :: (MonadDi level path msg m, Ex.Exception e) => e -> m a
throw e = ask >>= \di -> Di.throw' natSTM di e
{-# INLINE throw #-}
