{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings#-}

module Main where

import Control.Monad (forever)
import qualified Test.QuickCheck as QC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time

import qualified Di

--------------------------------------------------------------------------------

main :: IO ()
main = do
  s <- QC.generate genStringShort
  Di.new s $ \di -> do
      Di.runDiT di =<< QC.generate (fmap (sequence_ . take 10000) genMonadDis)

--------------------------------------------------------------------------------

genTextShort :: QC.Gen T.Text
genTextShort = do
  n <- QC.choose (3, 14)
  xs <- QC.vectorOf n (QC.elements "abcdefghijklmnopqrstuvwxyz1234567890-=+_(\n\r\\'\"*&^%$#@!,.[]")
  pure (T.pack xs)

genTextLShort :: QC.Gen TL.Text
genTextLShort = fmap TL.fromStrict genTextShort

genStringShort :: QC.Gen String
genStringShort = fmap T.unpack genTextShort

genPathNext :: Di.Path -> QC.Gen Di.Path
genPathNext p0 = case p0 of
  Di.Root _ -> QC.frequency
    [ (3, Di.Push <$> genTextLShort <*> pure p0)
    , (2, Di.Attr <$> genTextLShort <*> genTextLShort <*> pure p0) ]
  Di.Push _ p -> QC.frequency
    [ (3, Di.Push <$> genTextLShort <*> pure p0)
    , (2, Di.Attr <$> genTextLShort <*> genTextLShort <*> pure p0)
    , (1, pure p) ]
  Di.Attr _ _ p -> QC.frequency
    [ (3, Di.Push <$> genTextLShort <*> pure p0)
    , (2, Di.Attr <$> genTextLShort <*> genTextLShort <*> pure p0)
    , (1, pure p) ]

-- | Infinite list.
genPaths :: QC.Gen [Di.Path]
genPaths = iterateM genPathNext =<< genPath

genPath :: QC.Gen Di.Path
genPath = do
  n <- QC.choose (0, 10)
  paths <- genTextLShort >>= \t -> iterateM genPathNext (Di.Root t)
  pure (paths !! n)

genSystemTime :: QC.Gen Time.SystemTime
genSystemTime = do
  a <- fmap abs QC.arbitrary
  b <- QC.choose (0, 1000000000)
  pure (Time.MkSystemTime a b)

genSystemTimeSoonAfter :: Time.SystemTime -> QC.Gen Time.SystemTime
genSystemTimeSoonAfter (Time.MkSystemTime a _) = do
  a' <- (a +) <$> QC.choose (1, 1000)
  b' <- QC.choose (0, 1000000000)
  pure (Time.MkSystemTime a' b')

-- | Infinite list.
genSystemTimesAscending :: QC.Gen [Time.SystemTime]
genSystemTimesAscending = iterateM genSystemTimeSoonAfter =<< genSystemTime

genLevel :: QC.Gen Di.Level
genLevel = QC.frequency
  [ (30, pure Di.Debug)
  , (20, pure Di.Info)
  , (16, pure Di.Notice)
  , (12, pure Di.Warning)
  , (8,  pure Di.Error)
  , (4,  pure Di.Critical)
  , (3,  pure Di.Alert)
  , (1,  pure Di.Emergency) ]

genLog :: QC.Gen Di.Log
genLog = Di.Log <$> genSystemTime <*> genLevel <*> genPath <*> genTextLShort

genLogNewer :: Di.Log -> QC.Gen Di.Log
genLogNewer l0 = Di.Log
  <$> genSystemTimeSoonAfter (Di.logTime l0)
  <*> genLevel
  <*> genPathNext (Di.logPath l0)
  <*> genTextLShort

-- | Infinite list.
genLogsNewer :: QC.Gen [Di.Log]
genLogsNewer = iterateM genLogNewer =<< genLog

genMonadDi :: Di.MonadDi m => QC.Gen (m ())
genMonadDi = QC.frequency
  [ (1, Di.push <$> genTextShort <*> genMonadDi)
  , (2, Di.attr <$> genTextShort <*> genTextShort <*> genMonadDi)
  , (7, Di.log <$> genLevel <*> genTextLShort) ]

genMonadDiNext :: Di.MonadDi m => m () -> QC.Gen (m ())
genMonadDiNext m0 = (m0 >>) <$> QC.frequency
  [ (1, Di.push <$> genTextShort <*> genMonadDi)
  , (2, Di.attr <$> genTextShort <*> genTextShort <*> genMonadDi)
  , (7, Di.log <$> genLevel <*> genTextLShort) ]

-- | Infinite list.
genMonadDis :: Di.MonadDi m => QC.Gen [m ()]
genMonadDis = iterateM genMonadDiNext =<< genMonadDi

--------------------------------------------------------------------------------
-- Misc

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f a = f a >>= \a' -> iterateM f a' >>= \as' -> pure (a' : as')

