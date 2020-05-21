{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Functor (($>))
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.System as Time
import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import qualified Df1
import qualified Df1.Render

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "df1"
  [ Tasty.localOption (QC.QuickCheckTests 2000) $
    QC.testProperty "Render/Parse roundtrip" $ do
      QC.forAllShrink QC.arbitrary QC.shrink $ \log0 -> do
         let bl = BB.toLazyByteString (Df1.Render.log log0)
         Right log0 === ABL.eitherResult (ABL.parse Df1.parse bl)

  , Tasty.localOption (QC.QuickCheckTests 2000) $
    QC.testProperty "Color renders the same content" $ do
      QC.forAllShrink QC.arbitrary QC.shrink $ \log0 -> do
         let bl = BB.toLazyByteString (Df1.Render.log log0)
             blColor = BB.toLazyByteString (Df1.Render.logColorANSI log0)
         bl === removeAnsiEscapes blColor
  ]

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
  shrink = map fromString . QC.shrink . TL.unpack . Df1.unSegment

instance QC.Arbitrary Df1.Key where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . Df1.unKey

instance QC.Arbitrary Df1.Value where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . Df1.unValue

instance QC.Arbitrary Df1.Message where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . Df1.unMessage

genSystemTime :: QC.Gen Time.SystemTime
genSystemTime = do
  a <- QC.choose (0, 253402300799) -- up to 4 digit years
  b <- QC.choose (0, 1000000000)
  pure (Time.MkSystemTime a b)

--------------------------------------------------------------------------------

-- | Remove ANSI escapes. This is not complete, but it covers the ANSI codes we
-- use. In particular:
--
-- @
-- forall x.
--    'BB.toByteString' . 'log'
--        == 'removeAnsiEscapes' . 'BB.toByteString' . 'logColor'
-- @
removeAnsiEscapes :: BL.ByteString -> BL.ByteString
removeAnsiEscapes b0 = do
    case ABL.eitherResult (ABL.parse p b0) of
       Right b1 -> b1
       Left e -> Prelude.error ("removeAnsiEscapes: unexpected " ++ e)
  where
    p :: ABL.Parser BL.ByteString
    p = fmap BL.fromChunks $ many $
          (ABL.takeWhile1 (/= 27)) <|>
          (pAnsiEscape $> "") <|>
          (ABL.satisfy (== 27) $> "")
    pAnsiEscape :: ABL.Parser ()
    pAnsiEscape = (ABL.<?> "pAnsiEscape") $ do
      _ <- ABL.satisfy (== 27)  -- '\ESC'
      _ <- ABL.satisfy (== 91)  -- '['
      _ <- ABL.takeWhile (\w -> w == 59 || (w >= 48 && w <= 57)) -- ';' '0'-'9'
      _ <- ABL.satisfy (== 109) -- 'm'
      pure ()


