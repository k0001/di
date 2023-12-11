{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Html.Render
  ( log,
   -- * Themes
   --
   -- $themes
  )
where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Df1.Render as DR
import qualified Xmlbf as X
import Prelude hiding (log)

-- | Converts 'D.Log' into a list of 'X.Node's from "Xmlbf" to render it as HTML.
--
-- Example log:
-- @1999-12-20T07:11:39.230553031Z \/foo x=a y=b \/bar \/qux z=c z=d WARNING Something@
--
-- The generated HTML matches the following CSS selectors:
--
-- [@.df1-log.df1-debug@]:
--
-- [@.df1-log.df1-info@]:
--
-- [@.df1-log.df1-notice@]:
--
-- [@.df1-log.df1-warning@]:
--
-- [@.df1-log.df1-error@]:
--
-- [@.df1-log.df1-critical@]:
--
-- [@.df1-log.df1-alert@]:
--
-- [@.df1-log.df1-emergency@]: Top level container for a 'D.Log' entry of a particular 'D.Level'.
--
-- [@.df1-log .df1-time@]: Timestamp - Example: @1999-12-20T07:11:39.230553031Z@
--
-- [@.df1-log .df1-path@]: Full list of 'D.Path's - Example: @\/foo x=a y=b \/bar \/qux z=c z=d@
--
-- [@.df1-log .df1-path .df1-push@]: Single 'D.Push' - Examples: @\/foo@, @\/bar@, @\/qux@
--
-- [@.df1-log .df1-path .df1-push .df1-seg@]: Single 'D.Segment' - Example: @foo@
--
-- [@.df1-log .df1-path .df1-attr@]: Single 'D.Attr' - Example: @x=a@, @y=b@, @z=c@, @z=d@
--
-- [@.df1-log .df1-path .df1-attr .df1-key@]: Single 'D.Key' - Example: @x@, @y@, @z@, @z@
--
-- [@.df1-log .df1-path .df1-attr .df1-value@]: Single 'D.Value' - Example: @a@, @b@, @c@, @d@
--
-- [@.df1-log .df1-level@]: 'D.Level' - Example: @WARNING@
--
-- [@.df1-log .df1-msg@]: 'D.Message' - Example: @Something@
--
log :: D.Log -> [X.Node]
log x =
  X.element "div" [("class", "df1-log " <> levelClass (D.log_level x))] $
    mconcat
      [ timeHtml (D.log_time x),
        X.text " ",
        pathsHtml (D.log_path x),
        X.text " ",
        levelHtml (D.log_level x),
        X.text " ",
        messageHtml (D.log_message x)
      ]

levelClass :: D.Level -> T.Text
levelClass l = "df1-" <> T.toLower (levelToText l)

timeHtml :: Time.SystemTime -> [X.Node]
timeHtml t = spanClass "df1-time" (X.textLazy (textLazyFromBuilder (DR.iso8601 t)))

textLazyFromBuilder :: BB.Builder -> TL.Text
textLazyFromBuilder b = TL.fromStrict (TE.decodeUtf8 (BL.toStrict (BB.toLazyByteString b)))

levelHtml :: D.Level -> [X.Node]
levelHtml l = spanClass "df1-level" (X.text (levelToText l))

levelToText :: D.Level -> T.Text
levelToText l =
  case l of
    D.Debug -> "DEBUG"
    D.Info -> "INFO"
    D.Notice -> "NOTICE"
    D.Warning -> "WARNING"
    D.Error -> "ERROR"
    D.Critical -> "CRITICAL"
    D.Alert -> "ALERT"
    D.Emergency -> "EMERGENCY"

messageHtml :: D.Message -> [X.Node]
messageHtml m = spanClass "df1-msg" (X.textLazy (textLazyFromBuilder (DR.message m)))

pathsHtml :: Seq.Seq D.Path -> [X.Node]
pathsHtml ps = spanClass "df1-path" (intercalate (X.text " ") (fmap pathHtml (toList ps)))

pathHtml :: D.Path -> [X.Node]
pathHtml p = case p of
  D.Push seg -> spanClass "df1-push" (X.text "/" <> segmentHtml seg)
  D.Attr key val -> spanClass "df1-attr" (keyHtml key <> X.text "=" <> valueHtml val)

segmentHtml :: D.Segment -> [X.Node]
segmentHtml s = spanClass "df1-seg" (X.textLazy (textLazyFromBuilder (DR.segment s)))

keyHtml :: D.Key -> [X.Node]
keyHtml k = spanClass "df1-key" (X.textLazy (textLazyFromBuilder (DR.key k)))

valueHtml :: D.Value -> [X.Node]
valueHtml v = spanClass "df1-value" (X.textLazy (textLazyFromBuilder (DR.value v)))

spanClass :: T.Text -> [X.Node] -> [X.Node]
spanClass t = X.element "span" [("class", t)]

-- $themes
--
-- If you need to style the rendered HTML, you can use some of the themes shipped with this library.
--
-- == [theme-solarized-dark.css](https://raw.githubusercontent.com/k0001/di/master/df1-html/theme-solarized-dark.css)
--
-- To use this theme, wrap the @.df1-log@ elements in a @.df1-theme-solarized-dark@ element.
--
-- ![theme-solarized-dark](https://raw.githubusercontent.com/k0001/di/master/df1-html/theme-solarized-dark.png)
--
-- == [theme-solarized-light.css](https://raw.githubusercontent.com/k0001/di/master/df1-html/theme-solarized-light.css)
--
-- To use this theme, wrap the @.df1-log@ elements in a @.df1-theme-solarized-light@ element.
--
-- ![theme-solarized-light](https://raw.githubusercontent.com/k0001/di/master/df1-html/theme-solarized-light.png)
