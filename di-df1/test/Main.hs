{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.System as Time
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import qualified Di.Core as Di
import qualified Di.Df1
import qualified Df1

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "di-df1"
  [ QC.testProperty "fromDiLog . fromDf1Log == id" $ do
      QC.forAllShrink QC.arbitrary QC.shrink $ \df1Log -> do
         df1Log === Di.Df1.fromDiLog (Di.Df1.fromDf1Log df1Log)

  , QC.testProperty "fromDf1Log . fromDiLog == id" $ do
      QC.forAllShrink QC.arbitrary QC.shrink $ \diLog -> do
         diLog === Di.Df1.fromDf1Log (Di.Df1.fromDiLog diLog)
  ]

instance QC.Arbitrary (Di.Log Df1.Level Df1.Path Df1.Message) where
  arbitrary = Di.Log
    <$> genSystemTime <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (Di.Log a b c d) = Di.Log
    <$> pure a <*> QC.shrink b <*> QC.shrink c <*> QC.shrink d

instance QC.Arbitrary Df1.Log where
  arbitrary = Df1.Log
    <$> genSystemTime <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (Df1.Log a b c d) = Df1.Log
    <$> pure a <*> QC.shrink b <*> QC.shrink c <*> QC.shrink d

instance QC.Arbitrary Df1.Path where
  arbitrary = QC.oneof
    [ Df1.Push <$> QC.arbitrary
    , Df1.Attr <$> QC.arbitrary <*> QC.arbitrary ]
  shrink (Df1.Push s) = Df1.Push <$> QC.shrink s
  shrink (Df1.Attr k v) = Df1.Attr <$> QC.shrink k <*> QC.shrink v

instance QC.Arbitrary Df1.Level where
  arbitrary = QC.elements [minBound .. minBound]

instance QC.Arbitrary Df1.Segment where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map Df1.segment . QC.shrink . Df1.unSegment

instance QC.Arbitrary Df1.Key where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map Df1.key . QC.shrink . Df1.unKey

instance QC.Arbitrary Df1.Value where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map Df1.value . QC.shrink . Df1.unValue

instance QC.Arbitrary Df1.Message where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map Df1.message . QC.shrink . Df1.unMessage

instance QC.Arbitrary T.Text where
  arbitrary = T.pack <$> QC.arbitrary
  shrink = map T.pack . QC.shrink . T.unpack

instance QC.Arbitrary TL.Text where
  arbitrary = TL.pack <$> QC.arbitrary
  shrink = map TL.pack . QC.shrink . TL.unpack

genSystemTime :: QC.Gen Time.SystemTime
genSystemTime = do
  a <- QC.choose (0, 253402300799) -- up to 4 digit years
  b <- QC.choose (0, 1000000000)
  pure (Time.MkSystemTime a b)

