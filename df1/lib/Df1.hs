-- | This module exports tools for typing, parsing, and rendering logs in the
-- /df1/ hierarchical structured logging format.
--
-- Consider this a preview release: The API is likely to stay stable, but
-- extensive testing, formalization and tooling is due.
--
-- Draft [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
-- specification of the /df1/ log line format (TO BE VERIFIED):
--
-- > <log> ::= <timestamp> " " <path> " " <level> " " <message>
-- > <path> ::= <path1> " " <path> | <path1> | ""
-- > <path1> ::= "/" <segment> | <key> "=" <value>
-- > <segment> ::= zero or more characters until " "
-- > <key> ::= zero or more characters until (" " | "=")
-- > <value> ::= zero or more characters until " "
-- > <message> ::= zero or more characters until LF ("\n")
-- > <level> ::= "DEBUG" | "INFO" | "NOTICE" | "WARNING" | "ERROR" | "CRITICAL" | "ALERT" | "EMERGENCY"
-- > <timestamp> ::= <year> "-" <month> "-" <day> "T" <hour> ":" <minute> ":" <second> "." <nanosecond> "Z"
-- > <year> ::= <digit> <digit> <digit> <digit>
-- > <month> ::= <digit> <digit>
-- > <day> ::= <digit> <digit>
-- > <hour> ::= <digit> <digit>
-- > <minute> ::= <digit> <digit>
-- > <second> ::= <digit> <digit>
-- > <nanosecond> ::= <digit> <digit> <digit> <digit> <digit> <digit> <digit> <digit> <digit>
-- > <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
module Df1
 ( -- * Types
   T.Log(Log, log_time, log_level, log_path, log_message)
 , T.Level(Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency)
 , T.Path(Attr, Push)
 , T.Segment, T.unSegment, T.ToSegment(segment)
 , T.Key, T.unKey, T.ToKey(key)
 , T.Value, T.unValue, T.ToValue(value)
 , T.Message, T.unMessage, T.ToMessage(message)
   -- * Parsing
 , P.parse
 ) where

import qualified Df1.Parse as P
import qualified Df1.Types as T

