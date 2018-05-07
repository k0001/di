{-# LANGUAGE OverloadedStrings #-}
-- | This module is a highly opinionated, basic, and yet sufficient
-- choice of a concrete stack of logging solutions belonging to the [di
-- logging ecosystem](https://github.com/k0001/di)—an otherwise rather
-- general ecosystem, flexible and full of choices.
--
-- For most logging scenarios out there, the choices made here should suffice,
-- but if you find these are not sufficient for your particular use case, please
-- refer to other libraries of the /di logging ecosystem/ such as
-- [di-core](https://hackage.haskell.org/package/di-core),
-- [di-monad](https://hackage.haskell.org/package/di-core),
-- [di-handle](https://hackage.haskell.org/package/di-core), or
-- [di-df1](https://hackage.haskell.org/package/di-core), and you are likely
-- to find a compatible and composable solution there. For this reason, staring
-- with this package rather than one of the those other lower-level packages is
-- always recommended.
--
-- The choices made here are:
--
-- * We encourage a [mtl](https://hackage.haskell.org/package/mtl) approach
--   through a typeclass called 'Di.Monad.MonadDi', for which all of the
--   monad transformers in
--   [transformers](https://hackage.haskell.org/package/transformers) and
--   [pipes](https://hackage.haskell.org/package/pipes) have instances.
--
-- * We provide our own 'Di.Monad.DiT' monad transformer which
--   has a 'MonadDi' instance, as well as instances for all the relevant
--   typeclasses in the
--   [base](https://hackage.haskell.org/package/base),
--   [mtl](https://hackage.haskell.org/package/mtl), and
--   [exceptions](https://hackage.haskell.org/package/exceptions) libraries.
--   All of the 'MonadDi' instances exported by this package expect a
--   'DiT' transformer in the stack somewhere, and defer all work to it.
--
-- * We embrace the [df1](https://hackage.haskell.org/package/df1) hierarchical
--   structured logging format, both at the type-level and when rendering the
--   log lines as text. Most notably, this means that we embrace the /df1/
--   importance 'Df1.Level's.
--
-- * We commit logs to the outside world by printing them to 'System.IO.stderr'.
--
-- You will notice that some of the functions in this module mention the types
-- 'Df1.Level', 'Df1.Path' and 'Df1.Message', and some other functions
-- talk about @level@, @path@ and @msg@ type variables. This is
-- because even while our particular set of choices require some monomorphic
-- types, the /di logging ecosystem/ treats these values polymorphically, so
-- they will show up in the types in one way or another, either in concrete or
-- polymorphic form. This can seem a bit noisy, but the good news is that if,
-- for example, want to call a third party library that uses other types
-- for conveying the idea of a “log importance level” or a “log message”,
-- then you can do so if you can convert between these different types.
-- For more information about this, see "Di.Monad" and "Di.Core", but not today.
--
-- The intended usage of this module is:
--
-- @
-- import qualified "Di"
-- @
module Di
 ( new
 , Di.Core.Di

   -- * Monadic API
 , Di.Monad.MonadDi

   -- ** Hierarchy
 , Di.Df1.Monad.push
 , Di.Path
 , Df1.Segment
 , Df1.segment

   -- ** Metadata
 , Di.Df1.Monad.attr
 , Df1.Key
 , Df1.key
 , Df1.Value
 , Df1.value

   -- ** Messages
 , Df1.Message
 , Di.Df1.Monad.debug
 , Di.Df1.Monad.info
 , Di.Df1.Monad.notice
 , Di.Df1.Monad.warning
 , Di.Df1.Monad.error
 , Di.Df1.Monad.alert
 , Di.Df1.Monad.critical
 , Di.Df1.Monad.emergency

   -- * Basic DiT support
 , Di.Monad.DiT
 , Di.Monad.runDiT
 , Di.Monad.hoistDiT
 ) where

import Control.Monad.Catch as Ex
import Control.Monad.IO.Class (MonadIO)

import qualified Df1
import qualified Di.Core
import qualified Di.Df1
import qualified Di.Df1.Monad
import qualified Di.Handle
import qualified Di.Monad

--------------------------------------------------------------------------------

-- | Obtain a 'Di.Core.Di' that will write logs in the /df1/ format to
-- 'System.IO.stderr'.
--
-- Generally, you will want to call 'new' just once per application, right from
-- your @main@ function. For example:
--
-- @
-- main :: 'IO' ()
-- main = do
--    'new' $ \\di -> do
--       'Di.Monad.runDiT' di $ do
--           -- /The rest of your program goes here./
--           -- /You can start logging right away./
--           'Di.Df1.Monad.notice' "Welcome to my program!"
--           -- /You can use 'Di.Df1.Monad.push' to separate different/
--           -- /logging scopes of your program:/
--           'Di.Df1.Monad.push' "initialization" $ do
--               -- /something something do initialization/
--               'Di.Df1.Monad.notice' "Starting web server"
--           'Di.Df1.Monad.push' "server" $ do
--               -- /And you can use 'Di.Df1.Monad.attr' to add metadata to/
--               -- /messages logged within a particular scope./
--               'Di.Df1.Monad.attr' "port" "80" $ do
--                    'Di.Df1.Monad.info' "Listening for new clients"
--                    clientAddress <- /somehow get a client connection/
--                    'Di.Df1.Monad.push' "handler" $ do
--                       'Di.Df1.Monad.attr' "client-address" clientAddress $ do
--                          'Di.Df1.Monad.info' "Connection established"
-- @
--
-- That program will render something like this to 'System.IO.stderr' (in colors!):
--
-- @
-- 2018-05-06T19:48:06.194579393Z NOTICE Welcome to my program!
-- 2018-05-06T19:48:06.195041422Z \/initialization NOTICE Starting web server
-- 2018-05-06T19:48:06.195052862Z \/server port=80 INFO Listening for new clients
-- 2018-05-06T19:48:06.195059084Z \/server port=80 \/handler client%2daddress=192%2e168%2e0%2e25%3a32528 INFO Connection established
-- @
--
-- (Unrelated: Notice how /df1/ escapes pretty much all punctuation characters.
-- This is temporal until /df1/ is formalized and a more limited set of
-- punctuation characters is reserved.)
new
  :: (MonadIO m, Ex.MonadMask m)
  => (Di.Core.Di Df1.Level Df1.Path Df1.Message -> m a)
  -- ^ Within this scope, you can use the obtained 'Di.Core.Di' safely, even
  -- concurrently. As soon as @m a@ finishes, 'new' will block until
  -- all logs have finished processing, before returning.
  --
  -- /WARNING:/ Even while
  -- @'new' commit 'pure' :: m ('Di.Core.Di' 'Df1.Level' 'Df1.Path' 'Df1.Message')@
  -- type-checks, and you can use it to work with the 'Di.Core.Di' outside the
  -- intended scope, you will have to remember to call 'Di.Monad.flush'
  -- yourself before exiting your application. Otherwise, some log messages may
  -- be left unprocessed. If possible, use the 'Di.Core.Di' within this function
  -- and don't let it escape this scope.
  -> m a -- ^
new act = do
  commit <- Di.Handle.stderr Di.Df1.df1
  Di.Core.new commit act

