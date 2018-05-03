{-# LANGUAGE LambdaCase #-}

module Di.Backend
 ( mkDiStringStderr
 , mkDiStringHandle
 ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mconcat, mappend)
import Data.String (IsString(fromString))
import qualified Data.Time as Time
import Prelude hiding (log, filter)
import qualified System.IO as IO
import Data.Semigroup (Semigroup(..))

import Di.Core (Di, mkDi)

--------------------------------------------------------------------------------

-- | Strings separated by a forward slash.
--
-- The string doesn't contain any of @[\'/'\, \' \', \'\\n\', \'\\r\']@.
--
-- Use 'fromString' (GHC's @OverloadedStrings@ extension) to construct a
-- 'StringPath'.
newtype StringPath = UnsafeStringPath { unStringPath :: String }
  deriving (Eq, Ord, Show)

instance IsString StringPath where
  fromString = stringPathSingleton
  {-# INLINE fromString #-}

-- | Ocurrences of one of @[\'/'\, \' \', \'\\n\', \'\\r\']@ in the given
-- 'String' will be replaced by @\'_\'@.
stringPathSingleton :: String -> StringPath
stringPathSingleton = \s -> UnsafeStringPath (map f s)
  where f :: Char -> Char
        f = \case '/'  -> '_'
                  ' '  -> '_'
                  '\n' -> '_'
                  '\r' -> '_'
                  c    -> c

instance Semigroup StringPath where
  UnsafeStringPath "" <> b = b
  a <> UnsafeStringPath "" = a
  UnsafeStringPath a <> UnsafeStringPath b = UnsafeStringPath (a <> "/" <> b)

instance Monoid StringPath where
  mempty = UnsafeStringPath ""
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}
-- | 'String's are written to 'IO.Handle' using the 'IO.Handle''s locale
-- encoding.
--
-- The @level@ and @message@ are plain 'String's.
--
-- The @path@ is a list of 'String's which will be separated by @\'/\'@ when
-- rendered. Ocurrences of one of @[\'/'\, \' \', \'\\n\', \'\\r\']@ in those
-- 'String's will be replaced by @\'_\'@.
--
-- @
-- > 'log' ('push' [\"cuatro\"] ('push' [\"uno dos\", \"tres\"] di)) \"WARNING\" \"Hello!\"
-- WARNING 2018-05-03T09:15:54.819379740000Z uno_dos\/tres\/cuatro: Hello!
-- @
mkDiStringHandle
  :: (MonadIO m)
  => IO.Handle
  -> m (Di String [String] String)
mkDiStringHandle h = liftIO $ do
    IO.hSetBuffering h IO.LineBuffering
    mkDi $ \ts l p m -> do
      IO.hPutStrLn h $ mconcat
        [ l, " ", renderIso8601 ts
        , if p == mempty then "" else (" " <> unStringPath (foldMap stringPathSingleton p))
        , ": ", noBreaks m ]
      IO.hFlush h
  where
    noBreaks :: String -> String
    noBreaks = concatMap $ \case
      '\n' -> "\\n"
      '\r' -> "\\r"
      c -> [c]

-- |
-- @
-- 'mkDiStringStderr'  ==  'mkDiStringHandle' 'IO.stderr'
-- @
mkDiStringStderr :: MonadIO m => m (Di String [String] String)
mkDiStringStderr = mkDiStringHandle IO.stderr

--------------------------------------------------------------------------------

renderIso8601 :: Time.UTCTime -> String
renderIso8601 = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ"
