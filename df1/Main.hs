{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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

import qualified Di
import qualified Di.Df1
import qualified Di.Gen

--------------------------------------------------------------------------------

main :: IO ()
main = Di.new $ flip Di.runDiT $ do
   co <- lift getCmdOpts
   case cmdOpts_subCmd co of
      SubCmdQuery x -> main_query x
      SubCmdFake -> main_fake

--------------------------------------------------------------------------------

data CmdOpts = CmdOpts
  { cmdOpts_subCmd :: !SubCmd
  }

data SubCmd
  = SubCmdQuery !SubCmdQueryOpts
  | SubCmdFake

data SubCmdQueryOpts = SubCmdQueryOpts
  { subCmdQueryOpts_input :: !(Maybe IO.FilePath)
  , subCmdQueryOpts_output :: !(Maybe IO.FilePath)
  }

---

getCmdOpts :: IO CmdOpts
getCmdOpts = OA.execParser $ OA.info (OA.helper <*> pCmdOpts) $ mconcat
  [ OA.fullDesc, OA.progDesc "df1 command-line tool" ]

pCmdOpts :: OA.Parser CmdOpts
pCmdOpts = CmdOpts <$> (OA.helper <*> pSubCmd)

pSubCmd :: OA.Parser SubCmd
pSubCmd = OA.subparser $ mconcat
  [ OA.command "query" $ OA.info
      (OA.helper <*> fmap SubCmdQuery pSubCmdQueryOpts)
      (OA.fullDesc <>
       OA.progDesc "Select logs that match the given query")
  , OA.command "fake" $ OA.info
      (OA.helper <*> pure SubCmdFake)
      (OA.fullDesc <>
       OA.progDesc "Generate fake logs")
  ]

pSubCmdQueryOpts :: OA.Parser SubCmdQueryOpts
pSubCmdQueryOpts = do
  subCmdQueryOpts_input <- OA.option (fmap Just OA.str) $ mconcat
    [ OA.long "input"
    , OA.metavar "FILENAME"
    , OA.value Nothing
    , OA.help "File to use as input. If unspecified, defaults to STDIN." ]
  subCmdQueryOpts_output <- OA.option (fmap Just OA.str) $ mconcat
    [ OA.long "output"
    , OA.metavar "FILENAME"
    , OA.value Nothing
    , OA.help "File to use as output. If unspecified, defaults to STDOUT." ]
  pure (SubCmdQueryOpts {..})


--------------------------------------------------------------------------------

main_fake :: (Di.MonadDi m, MonadIO m) => m ()
main_fake = Di.push "fake" $ do
   liftIO $ sample 1000

sample :: Int -> IO ()
sample n = Di.Gen.ioPrintLogs =<< QC.generate (fmap (take n) Di.Gen.genLogs)

--------------------------------------------------------------------------------

main_query
  :: forall m
  .  (Di.MonadDi m, MonadIO m, Ex.MonadMask m)
  => SubCmdQueryOpts
  -> m ()
main_query opts = Di.push "query" $ do
  withInput (subCmdQueryOpts_input opts) $ \hin ->
     withOutput (subCmdQueryOpts_output opts) $ \hout -> do
        Di.withSink (Di.handleLines True hout Di.Df1.render) $ \write -> do
           let p0 = Di.runLogLineParser Di.Df1.parse (Pb.fromHandle hin)
           P.runEffect $ P.for p0 $ \case
              Right !log' -> liftIO (write log')
              Left e -> Di.push "parser" $ Di.warning (fromString e)

--------------------------------------------------------------------------------

withInput
  :: (MonadIO n, Ex.MonadMask n)
  => Maybe FilePath
  -> (IO.Handle -> n a)
  -> n a
withInput Nothing act = act IO.stdin
withInput (Just fn) act = Ex.bracket
  (liftIO (IO.openBinaryFile fn IO.ReadMode)) (liftIO . IO.hClose) act

withOutput
  :: (MonadIO n, Ex.MonadMask n)
  => Maybe FilePath
  -> (IO.Handle -> n a)
  -> n a
withOutput Nothing act = act IO.stdout
withOutput (Just fn) act = Ex.bracket
  (liftIO (IO.openBinaryFile fn IO.AppendMode)) (liftIO . IO.hClose) act

