{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (liftA2)
import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
  (STM, atomically, TQueue, newTQueue, writeTQueue, tryReadTQueue)
import qualified Data.ByteString.Builder as BB
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Monoid (Sum(Sum, getSum))
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as Pb
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import qualified Di
import qualified Di.Df1
import qualified Di.Types as Di (diLogs)
import qualified Di.Gen

--------------------------------------------------------------------------------

stmSink :: (Di.Log -> STM ()) -> Di.Sink
stmSink m = Di.Sink (pure (pure (), atomically . m))

withStmSink :: (Di.Sink -> IO a) -> IO ([Di.Log], a)
withStmSink k = do
  tq :: TQueue Di.Log <- atomically newTQueue
  a <- k $ stmSink (writeTQueue tq)
  logs <- atomically (drainTQueue tq)
  pure (logs, a)

-- | Like 'runDiT' but outputs the committed 'Di.Log's in the return value,
-- rather than in stderr.
runDiT_dump :: Di.DiT IO a -> IO ([Di.Log], a)
runDiT_dump m = withStmSink $ \sink -> do
  Di.new' sink (\di -> Di.runDiT di (m <* Di.flush))

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "root"
  [ ttDi
  , ttDf1
  ]

ttDi :: Tasty.TestTree
ttDi = Tasty.testGroup "di"
  [ QC.testProperty "Writing to diLogs sends to sink" $ do
      QC.forAll (fmap (take 100) Di.Gen.genLogs) $ \logs0 -> do
        QC.ioProperty $ do
          (logs1, ()) <- runDiT_dump $ do
             di <- Di.ask
             -- NOTE: Don't do this at home, it's only for testing.
             liftIO $ mapM_ (atomically . writeTQueue (Di.diLogs di)) logs0
          logs1 @?= logs0
  ]

ttDf1 :: Tasty.TestTree
ttDf1 = Tasty.testGroup "df1"
  [ QC.testProperty "Render/Parse roundtrip" $ do
      let Di.LogLineRendererUtf8 render = Di.Df1.render
      QC.forAll Di.Gen.genLog $ \log0 -> do
         let bl = BB.toLazyByteString (render False log0)
             p0 = Pb.fromLazy bl
             p1 = Di.runLogLineParser Di.Df1.parse p0
         [Right log0] === P.toList p1
  ]

--------------------------------------------------------------------------------

drainTQueue :: TQueue a -> STM [a]
drainTQueue tq = do
  let go as = maybe (pure as) (\a -> go (a:as)) =<< tryReadTQueue tq
  fmap reverse (go [])

#if !MIN_VERSION_QuickCheck(2,10,0)
instance QC.Testable () where
  property () = QC.property True
#endif
