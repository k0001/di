{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Di.Df1
 ( df1
 ) where

import qualified Data.ByteString.Builder as BB
import Data.Function (fix)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Prelude hiding (log, filter, error)

import Di.Misc (renderIso8601)
import Di.Types
 (Log(Log),
  Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency),
  Path(Attr, Push, Root),
  LogLineRenderer(LogLineRendererUtf8))

--------------------------------------------------------------------------------

-- | Render in the __df1__ format.
df1 :: LogLineRenderer
df1 = LogLineRendererUtf8 $ \case
  True -> renderLogColor
  False -> renderLog
{-# INLINE df1 #-}

-- | Like 'renderLog' but with ANSI colors.

-- This is rather ugly, but whatever.
renderLogColor :: Log -> BB.Builder
{-# INLINABLE renderLogColor #-}
renderLogColor = \(Log syst lvl path msg) ->
  let t = renderIso8601 syst <> space
      pDef = \fg -> renderPathColor fg fgBlue fgCyan path <> space
      pRed = renderPathColor fgBlack fgWhite fgWhite path <> space
      m = space <> escapeMessage msg <> reset
  in case lvl of
      Debug -> reset <> t <> pDef fgDefault <> fgDefault <> debug <> m
      Info -> reset <> t <> pDef fgDefault <> fgDefault <> info <> m
      Notice -> bgDefault <> fgGreen <> t <> pDef fgDefault <> fgGreen <> notice <> m
      Warning -> bgDefault <> fgYellow <> t <> pDef fgDefault <> fgYellow <> warning <> m
      Error -> bgDefault <> fgRed <> t <> pDef fgDefault <> fgRed <> error <> m
      Critical -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> critical <> fgBlack <> m
      Alert -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> alert <> fgBlack <> m
      Emergency -> bgRed <> fgBlack <> t <> pRed <> fgWhite <> emergency <> fgBlack <> m

-- | Like 'renderLogColor', but without color.
renderLog :: Log -> BB.Builder
{-# INLINABLE renderLog #-}
renderLog = \(Log syst lvl path msg) ->
  renderIso8601 syst <> space <> renderPath path <> space <>
  level lvl <> space <> escapeMessage msg

-- | @'renderPathColor' a b c p@ renders @p@ using @a@ as the default color (for
-- things like whitespace or attribute values), @b@ as the color for path names,
-- and @c@ as the color for attribute keys.

-- This is rather ugly, but whatever.
renderPathColor :: BB.Builder -> BB.Builder -> BB.Builder -> Path -> BB.Builder
renderPathColor defc pathc keyc = fix $ \f -> \case
  Attr k v p ->
    f p <> defc <> space <> keyc <> escapeMeta k <>
    defc <> equals <> escapeMeta v
  Push x p -> f p <> defc <> space <> pathc <> slash <> escapeMeta x
  Root x -> pathc <> slash <> escapeMeta x

-- | Like 'renderPathColor', but without color.
renderPath :: Path -> BB.Builder
renderPath = fix $ \f -> \case
  Attr k v p -> f p <> space <> escapeMeta k <> equals <> escapeMeta v
  Push x p -> f p <> space <> slash <> escapeMeta x
  Root x -> slash <> escapeMeta x

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

--------------------------------------------------------------------------------
-- Some hardcoded stuff we use time and time again

debug :: BB.Builder
debug = BB.string7 "DEBUG"

info :: BB.Builder
info = BB.string7 "INFO"

notice :: BB.Builder
notice = BB.string7 "NOTICE"

warning :: BB.Builder
warning = BB.string7 "WARNING"

error :: BB.Builder
error = BB.string7 "ERROR"

critical :: BB.Builder
critical = BB.string7 "CRITICAL"

alert :: BB.Builder
alert = BB.string7 "ALERT"

emergency :: BB.Builder
emergency = BB.string7 "EMERGENCY"

level :: Level -> BB.Builder
{-# INLINE level #-}
level = \case
  { Debug -> debug; Info -> info;
    Notice -> notice; Warning -> warning;
    Error -> error; Critical -> critical;
    Alert -> alert; Emergency -> emergency }

space :: BB.Builder
space = BB.char7 ' '

slash :: BB.Builder
slash = BB.char7 '/'

equals :: BB.Builder
equals = BB.char7 '='

--------------------------------------------------------------------------------
-- ANSI escape codes

-- | Reset all
reset :: BB.Builder
reset = BB.string7 "\x1b[0m"

-- | Default foreground
fgDefault :: BB.Builder
fgDefault = BB.string7 "\x1b[39m"

-- | Reset background
bgDefault :: BB.Builder
bgDefault = BB.string7 "\x1b[49m"

-- | green foreground
fgGreen :: BB.Builder
fgGreen = BB.string7 "\x1b[32m"

-- | green foreground
fgRed :: BB.Builder
fgRed = BB.string7 "\x1b[31m"

-- | Yellow foreground
fgYellow :: BB.Builder
fgYellow = BB.string7 "\x1b[33m"

-- | Cyan foreground
fgCyan :: BB.Builder
fgCyan = BB.string7 "\x1b[36m"

-- | Blue foreground
fgBlue :: BB.Builder
fgBlue = BB.string7 "\x1b[34m"

-- | Black foreground
fgBlack :: BB.Builder
fgBlack = BB.string7 "\x1b[30m"

-- | White foreground
fgWhite :: BB.Builder
fgWhite = BB.string7 "\x1b[37m"

-- | Red background
bgRed :: BB.Builder
bgRed = BB.string7 "\x1b[41m"

