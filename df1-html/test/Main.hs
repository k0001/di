{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Sequence as Seq
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Df1.Html.Parse as DHP
import qualified Df1.Html.Render as DHR
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty
import qualified Xmlbf as X

--------------------------------------------------------------------------------

main :: IO ()
main =
  Tasty.defaultMainWithIngredients
    [ Tasty.consoleTestReporter,
      Tasty.listingTests
    ]
    tt

tt :: Tasty.TestTree
tt =
  Tasty.testGroup
    "df1-html"
    [ HU.testCase "Given a D.Log, render it as HTML" $ do
        expected1 @=? DHR.log log1,

      HU.testCase "Parse that HTML to reobtain the D.Log" $ do
         case X.parse DHP.log expected1 of
           Left _ -> fail "Could not parse Log."
           Right a -> log1 @=? a,

      Tasty.localOption (QC.QuickCheckTests 2000) $
      QC.testProperty "Render/Parse roundtrip" $ do
        QC.forAllShrink QC.arbitrary QC.shrink $ \log0 -> do
         let html = DHR.log log0
         Right log0 === X.parse DHP.log html
    ]


log1 :: D.Log
log1 =
  D.Log
    { D.log_time = Time.MkSystemTime 355 43,
      D.log_level = D.Warning,
      D.log_path = examplePath,
      D.log_message = D.message ("example" :: String)
    }

examplePath :: Seq.Seq D.Path
examplePath =
  [ D.Push (D.segment ("foo" :: String)),
    D.Attr (D.key ("=" :: String)) (D.value ("a" :: String)),
    D.Attr (D.key ("y" :: String)) (D.value ("b" :: String)),
    D.Push (D.segment ("bar" :: String)),
    D.Push (D.segment ("qux" :: String)),
    D.Attr (D.key ("z" :: String)) (D.value ("c" :: String)),
    D.Attr (D.key ("z" :: String)) (D.value ("d" :: String))
  ]


expected1 :: [X.Node]
expected1 =
  X.element
    "div"
    [("class", "df1-log df1-warning")]
    $ mconcat
      [ X.element "span" [("class", "df1-time")] (X.text "1970-01-01T00:05:55.000000043Z"),
        X.text " ",
        X.element "span" [("class", "df1-path")] $
          mconcat
            [ X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "foo")],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "%3d"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "a")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "y"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "b")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "bar")],
              X.text " ",
              X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "qux")],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "z"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "c")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "z"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "d")
                  ]
            ],
        X.text " ",
        X.element "span" [("class", "df1-level")] (X.text "WARNING"),
        X.text " ",
        X.element "span" [("class", "df1-msg")] (X.text "example")
      ]


instance QC.Arbitrary D.Log where
  arbitrary = D.Log
    <$> genSystemTime <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
  shrink (D.Log a b c d) = D.Log
    <$> pure a <*> QC.shrink b <*> QC.shrink c <*> QC.shrink d

instance QC.Arbitrary D.Path where
  arbitrary = QC.oneof
    [ D.Push <$> QC.arbitrary
    , D.Attr <$> QC.arbitrary <*> QC.arbitrary ]
  shrink (D.Push s) = D.Push <$> QC.shrink s
  shrink (D.Attr k v) = D.Attr <$> QC.shrink k <*> QC.shrink v

instance QC.Arbitrary D.Level where
  arbitrary = QC.elements [minBound .. minBound]

instance QC.Arbitrary D.Segment where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . D.unSegment

instance QC.Arbitrary D.Key where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . D.unKey

instance QC.Arbitrary D.Value where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . D.unValue

instance QC.Arbitrary D.Message where
  arbitrary = fromString <$> QC.arbitrary
  shrink = map fromString . QC.shrink . TL.unpack . D.unMessage

genSystemTime :: QC.Gen Time.SystemTime
genSystemTime = do
  a <- QC.choose (0, 253402300799) -- up to 4 digit years
  b <- QC.choose (0, 1000000000)
  pure (Time.MkSystemTime a b)
