{-# LANGUAGE LambdaCase #-}

module Di.Backend
 ( mkDiStringStderr
 , mkDiStringHandle
 ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (mconcat, mappend, (<>))
import Data.String (IsString(fromString))
import qualified Data.Time as Time
import Prelude hiding (log, filter)
import qualified System.IO as IO

import Di.Core (Di, mkDi, contrapath)

--------------------------------------------------------------------------------

-- | Strings separated by a forward slash. Doesn't contain white space.
--
-- Use 'fromString' (GHC's @OverloadedStrings@ extension) to construct a
-- 'StringPath'.
newtype StringPath = StringPath { unStringPath :: String }
  deriving (Eq, Ord, Show)

instance IsString StringPath where
  fromString = stringPathSingleton
  {-# INLINE fromString #-}

stringPathSingleton :: String -> StringPath
stringPathSingleton = \s -> StringPath (map f s)
  where f :: Char -> Char
        f = \case '/'  -> '.'
                  ' '  -> '_'
                  '\n' -> '_'
                  '\r' -> '_'
                  c    -> c

instance Monoid StringPath where
  mempty = StringPath ""
  mappend (StringPath "") b = b
  mappend a (StringPath "") = a
  mappend (StringPath a) (StringPath b) = StringPath (a <> "/" <> b)

-- | 'String's are written to 'IO.Handle' using the 'IO.Handle''s locale
-- encoding.
mkDiStringHandle
  :: (MonadIO m)
  => IO.Handle
  -> m (Di String String String)
mkDiStringHandle h = liftIO $ do
    IO.hSetBuffering h IO.LineBuffering
    fmap (contrapath stringPathSingleton) $ mkDi $ \ts l p m -> do
       IO.hPutStrLn h $ mconcat
          [ l, " ", renderIso8601 ts
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
mkDiStringStderr :: MonadIO m => m (Di String String String)
mkDiStringStderr = mkDiStringHandle IO.stderr

--------------------------------------------------------------------------------

renderIso8601 :: Time.UTCTime -> String
renderIso8601 = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ"
