module Di.Df1
 ( render
 , parse
 ) where

import qualified Control.Exception as Ex
import qualified Pipes.Attoparsec as Pa

import Di.Df1.Render (renderLogColor, renderLog)
import Di.Df1.Parser (parseLog)
import Di.Types
  (LogLineRenderer(LogLineRendererUtf8), LogLineParser(LogLineParserUtf8))

--------------------------------------------------------------------------------

-- | Render in the __df1__ format.
render :: LogLineRenderer
render = LogLineRendererUtf8 $ \x -> case x of
  True -> renderLogColor
  False -> renderLog
{-# INLINE render #-}

-- | Parse a log in the __df1__ format.
parse :: LogLineParser
{-# INLINE parse #-}
parse = LogLineParserUtf8 $ do
  yel <- Pa.parse parseLog
  pure $ case yel of
     Just (Right l) -> Right l
     Just (Left e) -> Left (Ex.displayException e)
     -- According to 'LogLineParserUtf8', this should never happen.
     Nothing -> Left "Empty input"
