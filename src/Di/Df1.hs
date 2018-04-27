{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1
 ( df1
 ) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Prelude hiding (log, filter)

import Di.Misc (renderIso8601)
import Di.Types
 (Log, logTime, logPath, logLevel, logMessage,
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root),
  LogRenderer(TextLogRenderer))

--------------------------------------------------------------------------------

-- | Render in the __df1__ format.
df1 :: LogRenderer
{-# INLINE df1 #-}
df1 = TextLogRenderer $ \case
  True -> renderLogColor
  False -> renderLogColor -- TODO

renderLogColor :: Log -> TB.Builder
{-# INLINE renderLogColor #-}
renderLogColor = \x ->
  "\027[0m" <>
  renderIso8601 (logTime x) <> " " <>
  renderPathColor (logPath x) <> " " <>
  renderLevelAndMessage ansiColor (logLevel x) (logMessage x) <>
  "\027[0m"

renderPathColor
  :: Path
  -> TB.Builder
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
escapeMeta :: TL.Text -> TB.Builder
{-# INLINABLE escapeMeta #-}
escapeMeta t = TB.fromLazyText (TL.concatMap f t)
  where f :: Char -> TL.Text
        f = \case { ' ' -> "%20"; '!'  -> "%21"; '#' -> "%23"; '$' -> "%24";
                    '&' -> "%26"; '\'' -> "%27"; '(' -> "%28"; ')' -> "%29";
                    '*' -> "%2A"; '+'  -> "%2B"; ',' -> "%2C"; '/' -> "%2F";
                    ':' -> "%3A"; ';'  -> "%3B"; '=' -> "%3D"; '?' -> "%3F";
                    '@' -> "%40"; '['  -> "%5B"; ']' -> "%5D";
                    x -> TL.singleton x }

escapeMessage :: TL.Text -> TB.Builder
{-# INLINABLE escapeMessage #-}
escapeMessage t = TB.fromLazyText (TL.concatMap f t)
  where f :: Char -> TL.Text
        f = \case { '\n' -> "\\n"; '\r' -> "\\r"; x -> TL.singleton x }

renderLevelAndMessage
  :: (Intensity -> Color -> Color -> TB.Builder)
  -> Level
  -> TL.Text
  -> TB.Builder
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
  -> TB.Builder
{-# INLINABLE ansiColor #-}
ansiColor i fg bg =
  "\027[" <> ansiIntensityPart i <> ";" <>
  ansiFgPart fg <> ";" <> ansiBgPart bg <> "m"

ansiIntensityPart :: Intensity -> TB.Builder
{-# INLINABLE ansiIntensityPart #-}
ansiIntensityPart = \case { Normal -> "0"; Strong -> "1" }

ansiFgPart :: Color -> TB.Builder
{-# INLINABLE ansiFgPart #-}
ansiFgPart = \case
  { Black -> "30"; Red -> "31"; Green -> "32"; Yellow -> "33";
    Blue -> "34";  Magenta -> "35";  Cyan -> "36";  White -> "37";
    Default -> "39"; }

ansiBgPart :: Color -> TB.Builder
{-# INLINABLE ansiBgPart #-}
ansiBgPart = \case
  { Black -> "40"; Red -> "41"; Green -> "42"; Yellow -> "43";
    Blue -> "44"; Magenta -> "45"; Cyan -> "46"; White -> "47";
    Default -> "49" }
