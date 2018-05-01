module Di.Df1
 ( render
 , parse
 ) where

import Di.Df1.Render (renderLogColor, renderLog)
import Di.Df1.Parser (parseLog)
import Di.Types
  (LogLineRenderer(LogLineRendererUtf8), LogLineParser(LogLineParserUtf8))

-- | Render in the __df1__ format.
render :: LogLineRenderer
render = LogLineRendererUtf8 $ \x -> case x of
  True -> renderLogColor
  False -> renderLog
{-# INLINE render #-}

-- | Parse a log in the __df1__ format.
parse :: LogLineParser
parse = LogLineParserUtf8 parseLog
{-# INLINE parse #-}
