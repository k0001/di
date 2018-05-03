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
-- import qualified Di.Gen

--------------------------------------------------------------------------------

main :: IO ()
main = Di.new $ flip Di.runDiT $ do
   co <- lift getCmdOpts
   case cmdOpts_subCmdOpts co of
      SubCmdQuery -> main_select
      SubCmdFake -> main_fake
      SubCmdExample -> main_example

--------------------------------------------------------------------------------

data CmdOpts = CmdOpts
  { cmdOpts_subCmdOpts :: !SubCmdOpts
  }

data SubCmdOpts
  = SubCmdQuery
  | SubCmdFake
  | SubCmdExample

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
  , OA.command "example" $ OA.info
      (OA.helper <*> pure SubCmdExample)
      (OA.fullDesc <>
       OA.progDesc "Generate example logs")
  ]

--------------------------------------------------------------------------------

main_fake :: (Di.MonadDi m, MonadIO m) => m ()
main_fake = Di.push "fake" $ do
   pure ()
   -- liftIO $ sample 1000

-- sample :: Int -> IO ()
-- sample n = Di.Gen.ioPrintLogs =<< QC.generate (fmap (take n) Di.Gen.genLogs)
--
--------------------------------------------------------------------------------

main_select :: forall m. (Di.MonadDi m, MonadIO m) => m ()
main_select = Di.push "select" $ do
   P.runEffect $ do
     P.for (Di.runLogLineParser Di.Df1.parse Pb.stdin) $ \case
        Right log' -> liftIO (print (show log'))
        Left e -> Di.push "parser" $ Di.warning (fromString e)


--------------------------------------------------------------------------------

main_example :: forall m. (Di.MonadDi m, MonadIO m) => m ()
main_example = Di.push "example" $ Di.attr "version" "123.35" $ do
   Di.notice "Hello, welcome to the example project"
   Di.info "Let's initialize some stuff now"
   yport <- initStuff
   case yport of
      Nothing -> do
         Di.critical "Couldn't initialize. Can't do anything useful. Bye"
      Just port -> do
         Di.push "server" $ do
            Di.attr "port" (fromString (show port)) $ do
               Di.notice "All set. Listening for incoming connections."
               handler "yahoo.com" -- just pretending yahoo hits us
         Di.notice "Program finished successfully, we'll exit now."
 where
   initStuff :: m (Maybe Int)
   initStuff = Di.push "init" $ do
      Di.push "frobizer" $ do
        Di.attr "remote-addr" "google.com" $ do
           Di.notice "Connecting to remote frobizer"
           Di.attr "error" "division by 7" $ do
              Di.error "Could not connect."
           Di.warning "Using fallback in memory frobizer"
      Di.push "wat" $ do
        Di.info "The wat has been setup"
        return (Just 1234)
   handler :: String -> m ()
   handler remoteEnd = Di.push "handler" $ do
      Di.attr "remote-end" (fromString remoteEnd) $ do
         Di.notice "Incoming connection"
         Di.warning "Remote host is trying to hack us!"
