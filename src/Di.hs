-- |
--
-- Import this module __qualified__ as follows:
--
-- @
-- import qualified Di
-- @
--
-- The /only/ name that you are encouraged to import unqualified is 'MonadDi':
--
-- @
-- import Di (MonadDi)
-- @


module Di
 ( -- * Usage
   -- $usage
   Di
 , new

 , df1
 , stderr
 , handle

 , DiT
 , runDiT

 , MonadDi
 , push
 , attr
 , max
 , flush

 , emergency
 , alert
 , critical
 , error
 , warning
 , notice
 , info
 , debug
 ) where

import Prelude hiding (error, max)

import Di.Core
  (MonadDi, DiT, runDiT, new, flush, push, attr, max,
   emergency, alert, critical, error, warning, notice, info, debug)
import Di.Df1 (df1)
import Di.Types (Di)
import Di.Writer (stderr, handle)


-- $usage
--
-- First, read the documentation for the 'Di' datatype.
--
-- Second, create a base 'Di' value. You will achieve this by using 'new' or,
-- more likely, one of the ready-made 'B.newStringStderr',
-- 'B.newStringHandle', etc.
--
-- At this point, you can start logging messages using 'log'. However, things
-- can be made more interesting.
--
-- Your choice of base 'Di' will mandate particular types for the @level@,
-- @path@ and @msg@ arguments to 'Di'. However, these base types are likely to
-- be very general (e.g., 'String'), so quite likely you'll want to use
-- 'contralevel', 'contrapath' and 'contramsg' to make those types more
-- specific. For example, you can use a precise datatype like the following for
-- associating each log message with a particular importance level:
--
-- @
-- data Level = Error | Info | Debug
--   deriving ('Show')
-- @
--
-- Now, assuming the your base 'Di' @level@ type is 'String', you can use
-- @'contralevel' 'show'@ to convert your @'Di' 'String' path msg@ to a @'Di'
-- 'Level' path msg@. The same approach applies to @path@ and @msg@ as well,
-- through the 'contrapath' and 'contramsg' functions respectively.
--
-- /Hint/: If you are building a library, be sure to export your @Level@
-- datatype so that users of your library can 'contralevel' your 'Level'
-- datatype as necessary.

