module Df1.Cli
  ( main
  ) where

import Control.Monad.Catch as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Options.Applicative as OA
import qualified Test.QuickCheck as QC
import qualified Pipes as P
import qualified Pipes.ByteString as Pb
import qualified System.IO as IO

--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn "hi"
