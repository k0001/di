{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Df1.Html.Parse (log) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Df1 as D
import qualified Df1.Parse
import qualified Xmlbf as X
import Prelude hiding (log)

-- | An "Xmlbf" parser for a 'D.Log' rendered as HTML as 'Df1.Html.Render.log' renders it.
--
-- Notice that this parser will not ignore leading and trailing white space in the HTML.
-- It will become part of the parsed 'D.Key', 'D.Value', 'D.Segment', 'D.Message'.
log :: X.Parser D.Log
log = X.pElement "div" $ do
  attrClass "df1-log"
  t <- parseTime
  p <- parsePaths
  l <- parseLevel
  m <- parseMessage
  let raw = BL.toStrict $ TLE.encodeUtf8 $ TL.intercalate " " [t, p, l, m]
  case AB.parseOnly Df1.Parse.log raw of
    Left _ -> fail "Could not parse Log."
    Right a -> pure a

attrClass :: T.Text -> X.Parser ()
attrClass t = do
  attrs <- X.pAttr "class"
  case elem t (T.words attrs) of
    False -> fail ("Expected \"class\" value to contain " <> show t <> ".")
    True -> pure ()

parseTime :: X.Parser TL.Text
parseTime = X.pElement "span" $ do
  attrClass "df1-time"
  X.pText

parseLevel :: X.Parser TL.Text
parseLevel = X.pElement "span" $ do
  attrClass "df1-level"
  X.pText

parsePaths :: X.Parser TL.Text
parsePaths = X.pElement "span" $ do
  attrClass "df1-path"
  TL.intercalate " " <$> many (parsePush <|> parseAttr)

parsePush :: X.Parser TL.Text
parsePush = X.pElement "span" $ do
  attrClass "df1-push"
  t <- X.pText
  s <- parseSeg
  pure (t <> s)

parseSeg :: X.Parser TL.Text
parseSeg = X.pElement "span" $ do
  attrClass "df1-seg"
  X.pText <|> pure ""

parseAttr :: X.Parser TL.Text
parseAttr = X.pElement "span" $ do
  attrClass "df1-attr"
  k <- parseKey
  eq <- X.pText
  v <- parseValue
  pure (k <> eq <> v)

parseKey :: X.Parser TL.Text
parseKey = X.pElement "span" $ do
  attrClass "df1-key"
  X.pText <|> pure ""

parseValue :: X.Parser TL.Text
parseValue = X.pElement "span" $ do
  attrClass "df1-value"
  X.pText <|> pure ""

parseMessage :: X.Parser TL.Text
parseMessage = X.pElement "span" $ do
  attrClass "df1-msg"
  X.pText <|> pure ""
