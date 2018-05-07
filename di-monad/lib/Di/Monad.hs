{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
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
   MonadDi(ask, local, natSTM)
 , flush
 , push
 , filter
 , log

 , -- * DiT
   DiT
 , diT
 , runDiT
 , runDiT'
 , hoistDiT
 , localDiT
 ) where

import Control.Applicative (Alternative)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Catch as Ex
import Control.Monad.Cont (MonadCont, ContT(ContT))
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import Control.Monad.State (MonadState)
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT(IdentityT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Control.Monad.Zip (MonadZip)
import Data.Sequence (Seq)
import qualified Pipes as P
import qualified Pipes.Internal as P
import Prelude hiding (filter, error, log)

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum (AccumT(AccumT))
import Control.Monad.Trans.Select (SelectT(SelectT))
#endif

import Di.Core (Di)
import qualified Di.Core as Di

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
  deriving (Functor, Applicative, Alternative, Monad, MonadIO,
            MonadFail, MonadFix, MonadZip, MonadPlus, MonadCont,
            MonadState s, MonadWriter w)

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
  :: MonadIO m
  => Di level path msg
  -> DiT level path msg m a
  -> m a  -- ^
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
  -> m a  -- ^
runDiT' h = \di -> \(DiT (ReaderT f)) -> f (di, H h)
{-# INLINE runDiT' #-}

-- | Lift a monad morphism from @m@ to @n@ to a monad morphism from
-- @'DiT' level path msg m@ to @'DiT' n@.
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
  :: (forall x. n x -> m x) -- ^ Natural transformation from @n@ to @m@.
  -> (forall x. m x -> n x) -- ^ Monad morphism from @m@ to @n@.
  -> (DiT level path msg m a -> DiT level path msg n a)
  -- ^ Monad morphism from @'DiT' m@ to @'DiT' n@.
{-# INLINE hoistDiT #-}
hoistDiT hgf hfg = \(DiT (ReaderT f)) ->
  DiT (ReaderT (\(di, H hstmg) -> hfg (f (di, H (\stm -> hgf (hstmg stm))))))

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
  -> DiT level path msg m a -- ^
localDiT f = \(DiT (ReaderT gma)) -> DiT (ReaderT (\(di, h) -> gma (f di, h)))
{-# INLINE localDiT #-}

instance MonadReader r m => MonadReader r (DiT level path msg m) where
  ask = DiT (ReaderT (\_ -> Reader.ask))
  {-# INLINE ask #-}
  local f = \(DiT (ReaderT gma)) ->
    DiT (ReaderT (\di -> Reader.local f (gma di)))
  {-# INLINE local #-}

instance Ex.MonadThrow m => Ex.MonadThrow (DiT level path msg m) where
  throwM = \x -> lift (Ex.throwM x)
  {-# INLINE throwM #-}

instance Ex.MonadCatch m => Ex.MonadCatch (DiT level path msg m) where
  catch (DiT (ReaderT f)) = \g -> DiT (ReaderT (\x ->
    Ex.catch (f x) (\e -> let DiT (ReaderT h) = g e in h x)))
  {-# INLINE catch #-}

instance Ex.MonadMask m => Ex.MonadMask (DiT level path msg m) where
  mask f = DiT (ReaderT (\x ->
    Ex.mask (\u ->
      case f (\(DiT (ReaderT g)) -> DiT (ReaderT (u . g))) of
         DiT (ReaderT h) -> h x)))
  {-# INLINE mask #-}
  uninterruptibleMask f = DiT (ReaderT (\x ->
    Ex.uninterruptibleMask (\u ->
      case f (\(DiT (ReaderT g)) -> DiT (ReaderT (u . g))) of
        DiT (ReaderT h) -> h x)))
  {-# INLINE uninterruptibleMask #-}
  generalBracket acq rel use = DiT (ReaderT (\x ->
    Ex.generalBracket
      (case acq of DiT (ReaderT m) -> m x)
      (\res ec -> case rel res ec of DiT (ReaderT m) -> m x)
      (\res -> case use res of DiT (ReaderT m) -> m x)))
  {-# INLINABLE generalBracket #-}

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
class Monad m => MonadDi level path msg m | m -> level path msg where
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
    => STM a -> m a
  natSTM = \x -> lift (natSTM x)
  {-# INLINE natSTM #-}

instance Monad m => MonadDi level path msg (DiT level path msg m) where
  ask = DiT (ReaderT (\(di,_) -> pure di))
  {-# INLINE ask #-}
  natSTM = \x -> DiT (ReaderT (\(_, H h) -> h x))
  {-# INLINE natSTM #-}
  local f = localDiT f
  {-# INLINE local #-}

instance MonadDi level path msg m
  => MonadDi level path msg (ReaderT r m) where
  local f = \(ReaderT gma) -> ReaderT (\r -> local f (gma r))
  {-# INLINE local #-}

instance MonadDi level path msg m
  => MonadDi level path msg (SS.StateT s m) where
  local f = \(SS.StateT gma) -> SS.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance MonadDi level path msg m
  => MonadDi level path msg (SL.StateT s m) where
  local f = \(SL.StateT gma) -> SL.StateT (\s -> local f (gma s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi level path msg m)
  => MonadDi level path msg (WS.WriterT w m) where
  local f = \(WS.WriterT ma) -> WS.WriterT (local f ma)
  {-# INLINE local #-}

instance (Monoid w, MonadDi level path msg m)
  => MonadDi level path msg (WL.WriterT w m) where
  local f = \(WL.WriterT ma) -> WL.WriterT (local f ma)
  {-# INLINE local #-}

instance MonadDi level path msg m => MonadDi level path msg (MaybeT m) where
  local f = \(MaybeT ma) -> MaybeT (local f ma)
  {-# INLINE local #-}

instance MonadDi level path msg m => MonadDi level path msg (ExceptT e m) where
  local f = \(ExceptT ma) -> ExceptT (local f ma)
  {-# INLINE local #-}

instance MonadDi level path msg m => MonadDi level path msg (IdentityT m) where
  local f = \(IdentityT ma) -> IdentityT (local f ma)
  {-# INLINE local #-}

instance MonadDi level path msg m => MonadDi level path msg (ContT r m) where
  local f = \(ContT gma) -> ContT (\r -> local f (gma r))
  {-# INLINE local #-}

instance (Monoid w, MonadDi level path msg m)
  => MonadDi level path msg (RWSS.RWST r w s m) where
  local f = \(RWSS.RWST gma) -> RWSS.RWST (\r s -> local f (gma r s))
  {-# INLINE local #-}

instance (Monoid w, MonadDi level path msg m)
  => MonadDi level path msg (RWSL.RWST r w s m) where
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

instance MonadDi level path msg m
  => MonadDi level path msg (P.Proxy a' a b' b m) where
  {-# INLINABLE local #-}
  local f = \case
     P.Request a' fa -> P.Request a'(\a -> local f (fa  a ))
     P.Respond b fb' -> P.Respond b (\b' -> local f (fb' b'))
     P.Pure r -> P.Pure r
     P.M m -> P.M (local f m >>= \r -> pure (local f r))

instance MonadDi level path msg m => MonadDi level path msg (P.ListT m) where
  {-# INLINE local #-}
  local f = \(P.Select p) -> P.Select (local f p)

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
-- /Note regarding exceptions:/ Any exception thrown by 'natSTM' will be
-- thrown here. /Synchronous/ exceptions that happen due to failures in the
-- actual committing of the log message are handled by attempting to log the
-- message to 'IO.stderr' as a fallback if possible. /Asynchronous/ exceptions
-- happening as part of the committing process will be thrown in a different
-- thread, and are not not explicitly handled. /Pure/ exceptions originating
-- from the 'filter' function will be thrown here. In practical terms, this
-- means that unless you know what you are doing, you should just call 'log''
-- without worrying about it ever throwing exceptions.
log :: MonadDi level path msg m => level -> msg -> m ()
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
flush :: MonadDi level path msg m => m ()
flush = Di.flush' natSTM =<< ask
{-# INLINABLE flush #-}

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
  :: MonadDi level path msg m
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
push :: MonadDi level path msg m => path -> m a -> m a
push p = local (Di.push p)
{-# INLINE push #-}
