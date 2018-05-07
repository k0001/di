-- | This module exports tools for typing, parsing, and rendering logs in the
-- /df1/ hierarchical structured logging format.
--
-- Consider this a preview release: The API is likely to stay stable, but
-- extensive testing, formalization and tooling is due.
module Df1
 ( -- * Types
   T.Log(Log, log_time, log_level, log_path, log_message)
 , T.Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , T.Path(Attr, Push)
 , T.Segment, T.unSegment, T.segment
 , T.Key, T.unKey, T.key
 , T.Value, T.unValue, T.value
 , T.Message, T.unMessage, T.message
   -- * Parsing
 , P.parse
   -- * Rendering
 , R.render
 , R.renderColor
 ) where

import qualified Df1.Parse as P
import qualified Df1.Render as R
import qualified Df1.Types as T

