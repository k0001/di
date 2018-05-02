{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Options.Applicative as OA
import qualified Test.QuickCheck as QC
import qualified Pipes as P
import qualified Pipes.ByteString as Pb

import qualified Di
import qualified Di.Df1
import qualified Di.Gen

--------------------------------------------------------------------------------

main :: IO ()
main = Di.new "df1" $ flip Di.runDiT $ do
   co <- lift getCmdOpts
   case cmdOpts_subCmdOpts co of
      SubCmdQuery -> main_select
      SubCmdFake -> main_fake

--------------------------------------------------------------------------------

data CmdOpts = CmdOpts
  { cmdOpts_subCmdOpts :: !SubCmdOpts
  }

data SubCmdOpts
  = SubCmdQuery
  | SubCmdFake

---

getCmdOpts :: IO CmdOpts
getCmdOpts = OA.execParser $ OA.info (OA.helper <*> pCmdOpts) $ mconcat
  [ OA.fullDesc, OA.progDesc "df1 command-line tool" ]

pCmdOpts :: OA.Parser CmdOpts
pCmdOpts = CmdOpts <$> (OA.helper <*> pSubCmdOpts)

pSubCmdOpts :: OA.Parser SubCmdOpts
pSubCmdOpts = OA.subparser $ mconcat
  [ OA.command "select" $ OA.info
      (OA.helper <*> pure SubCmdQuery)
      (OA.fullDesc <>
       OA.progDesc "Select logs that match the given select")
  , OA.command "fake" $ OA.info
      (OA.helper <*> pure SubCmdFake)
      (OA.fullDesc <>
       OA.progDesc "Generate fake logs")
  ]

--------------------------------------------------------------------------------

main_fake :: (Di.MonadDi m, MonadIO m) => m ()
main_fake = Di.push "fake" $ do
   liftIO $ sample 1000

sample :: Int -> IO ()
sample n = Di.Gen.ioPrintLogs =<< QC.generate (fmap (take n) Di.Gen.genLogs)

--------------------------------------------------------------------------------

main_select :: forall m. (Di.MonadDi m, MonadIO m) => m ()
main_select = Di.push "select" $ do
  P.runEffect $ do
     P.for (Di.runLogLineParser Di.Df1.parse Pb.stdin) $ \case
        Right log' -> liftIO (print (show log'))
        Left e -> Di.push "parser" $ Di.warning (fromString e)
