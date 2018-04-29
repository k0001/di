{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1
 ( df1
 ) where

import qualified Data.ByteString.Builder as BB
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Prelude hiding (log, filter)

import Di.Misc (renderIso8601)
import Di.Types
 (Log, logTime, logPath, logLevel, logMessage,
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root),
  LogLineRenderer(LogLineRendererUtf8))

--------------------------------------------------------------------------------

-- | Render in the __df1__ format.
df1 :: LogLineRenderer
df1 = LogLineRendererUtf8 $ \case
  True -> renderLogColor
  False -> renderLogColor -- Should render without color!
{-# INLINE df1 #-}

renderLogColor :: Log -> BB.Builder
{-# INLINE renderLogColor #-}
renderLogColor = \x ->
  "\027[0m" <>
  renderIso8601 (logTime x) <> " " <>
  renderPathColor (logPath x) <> " " <>
  renderLevelAndMessage ansiColor (logLevel x) (logMessage x) <>
  "\027[0m"

renderPathColor
  :: Path
  -> BB.Builder
renderPathColor = let c = ansiColor in \case
  Attr k v p ->
     renderPathColor p <> " " <> c Normal Cyan Default <> escapeMeta k <>
     c Normal Default Default <> "=" <> escapeMeta v
  Push x p ->
     renderPathColor p <> " " <> c Normal Blue Default <> "/" <>
     c Normal Blue Default <> escapeMeta x <> c Normal Default Default
  Root x ->
     c Normal Blue Default <> "/" <> c Normal Blue Default <> escapeMeta x <>
     c Normal Default Default

-- | Escape metadata such as path names, attribute keys or attribute values.
escapeMeta :: TL.Text -> BB.Builder
{-# INLINABLE escapeMeta #-}
-- TODO use encodeUtf8BuilderEscaped
escapeMeta t = TL.encodeUtf8Builder (TL.concatMap f t)
  where f :: Char -> TL.Text
        f = \case { ' ' -> "%20"; '!'  -> "%21"; '#' -> "%23"; '$' -> "%24";
                    '&' -> "%26"; '\'' -> "%27"; '(' -> "%28"; ')' -> "%29";
                    '*' -> "%2A"; '+'  -> "%2B"; ',' -> "%2C"; '/' -> "%2F";
                    ':' -> "%3A"; ';'  -> "%3B"; '=' -> "%3D"; '?' -> "%3F";
                    '@' -> "%40"; '['  -> "%5B"; ']' -> "%5D";
                    x -> TL.singleton x }

escapeMessage :: TL.Text -> BB.Builder
{-# INLINABLE escapeMessage #-}
-- TODO use encodeUtf8BuilderEscaped
escapeMessage t = TL.encodeUtf8Builder (TL.concatMap f t)
  where f :: Char -> TL.Text
        f = \case { '\n' -> "\\n"; '\r' -> "\\r"; x -> TL.singleton x }

renderLevelAndMessage
  :: (Intensity -> Color -> Color -> BB.Builder)
  -> Level
  -> TL.Text
  -> BB.Builder
{-# INLINABLE renderLevelAndMessage #-}
renderLevelAndMessage c l m = let m' = escapeMessage m in case l of
  Debug     -> c Normal Default Default <> "DEBUG " <> m'
  Info      -> c Normal Default Default <> "INFO " <> m'
  Notice    -> c Normal Green Default <> "NOTICE " <> c Normal Default Default <> m'
  Warning   -> c Normal Yellow Default <> "WARNING " <> m'
  Error     -> c Strong Red Default <> "ERROR " <> m'
  Critical  -> c Strong White Red <> "CRITICAL " <> c Strong Black Red <> m'
  Alert     -> c Strong White Red <> "ALERT " <> c Strong Black Red <> m'
  Emergency -> c Strong White Red <> "EMERGENCY " <> c Strong Black Red <> m'

--------------------------------------------------------------------------------

data Intensity
  = Normal | Strong

data Color
  = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

ansiColor
  :: Intensity
  -> Color  -- ^ Foreground
  -> Color  -- ^ Background
  -> BB.Builder
{-# INLINABLE ansiColor #-}
ansiColor i fg bg =
  "\027[" <> ansiIntensityPart i <> ";" <>
  ansiFgPart fg <> ";" <> ansiBgPart bg <> "m"

ansiIntensityPart :: Intensity -> BB.Builder
{-# INLINABLE ansiIntensityPart #-}
ansiIntensityPart = \case { Normal -> "0"; Strong -> "1" }

ansiFgPart :: Color -> BB.Builder
{-# INLINABLE ansiFgPart #-}
ansiFgPart = \case
  { Black -> "30"; Red -> "31"; Green -> "32"; Yellow -> "33";
    Blue -> "34";  Magenta -> "35";  Cyan -> "36";  White -> "37";
    Default -> "39"; }

ansiBgPart :: Color -> BB.Builder
{-# INLINABLE ansiBgPart #-}
ansiBgPart = \case
  { Black -> "40"; Red -> "41"; Green -> "42"; Yellow -> "43";
    Blue -> "44"; Magenta -> "45"; Cyan -> "46"; White -> "47";
    Default -> "49" }
