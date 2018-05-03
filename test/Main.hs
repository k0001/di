{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative (liftA2)
import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
  (STM, atomically, TQueue, newTQueue, writeTQueue, tryReadTQueue)
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Monoid (Sum(Sum, getSum))
import qualified Data.List.NonEmpty as NEL
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import qualified Di
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
tt = Tasty.testGroup "di"
  [ QC.testProperty "Writing to diLogs sends to sink" $ do
      QC.forAll Di.Gen.genLogs $ \logs0 -> do
        let logs1 = take 100 logs0
        QC.ioProperty $ do
          (logs2, ()) <- runDiT_dump $ do
             di <- Di.ask
             -- NOTE: Don't do this at home, it's only for testing.
             liftIO $ mapM_ (atomically . writeTQueue (Di.diLogs di)) logs1
          logs2 @?= logs1
--
--  , QC.testProperty "MonadDi: log" $ do
      -- QC.forAll



{-
  , HU.testCase "push" $ do
       let x = [("","",""), ("","",""), ("","1",""), ("","12",""), ("","12",""), ("","","")]
       expect x $ \di0 -> do
          Di.log di0 "" ""
          -- Identity
          Di.log (Di.push "" di0) "" ""
          -- Composition
          Di.log (Di.push "1" di0) "" ""
          Di.log ((Di.push "2" . Di.push "1") di0) "" ""
          Di.log (Di.push "12" di0) "" ""
          -- Checking that di0 still works
          Di.log di0 "" ""

  , HU.testCase "contralevel" $ do
       let x = [("1","",""), ("1","",""), ("1","",""), ("2","",""), ("2","",""), ("1","","")]
           n = 1 :: Int
       expect x $ \di0 -> do
          Di.log di0 "1" ""
          -- Identity
          Di.log (Di.contralevel id di0) "1" ""
          -- Composition
          Di.log (Di.contralevel show di0) n ""
          Di.log ((Di.contralevel succ . Di.contralevel show) di0) n ""
          Di.log (Di.contralevel (show . succ) di0) n ""
          -- Checking that di0 still works
          Di.log di0 "1" ""

  , HU.testCase "contrapath" $ do
       let x = [("","",""), ("","1",""), ("","1",""), ("","1",""), ("","2",""), ("","2",""), ("","","")]
           n = Sum (1 :: Int)
       expect x $ \di0 -> do
          Di.log di0 "" ""
          Di.log (Di.push "1" di0) "" ""
          -- Identity
          Di.log (Di.push "1" (Di.contrapath id di0)) "" ""
          -- Composition
          Di.log (Di.push n (Di.contrapath (show . getSum) di0)) "" ""
          Di.log (Di.push n ((Di.contrapath (mappend n) . Di.contrapath (show . getSum)) di0)) "" ""
          Di.log (Di.push n (Di.contrapath (show . getSum . mappend n) di0)) "" ""
          -- Checking that di0 still works
          Di.log di0 "" ""

  , HU.testCase "contramsg" $ do
       let x = [("","","1"), ("","","1"), ("","","1"), ("","","2"), ("","","2"),  ("","","1")]
           n = 1 :: Int
       expect x $ \di0 -> do
          Di.log di0 "" "1"
          -- Identity
          Di.log (Di.contramsg id di0) "" "1"
          -- Composition
          Di.log (Di.contramsg show di0) "" n
          Di.log ((Di.contramsg succ . Di.contramsg show) di0) "" n
          Di.log (Di.contramsg (show . succ) di0) "" n
          -- Checking that di0 still works
          Di.log di0 "" "1"

  , HU.testCase "filter" $ do
       let x = [("1","","a"), ("2","","c"), ("1","","d"), ("3","","g"), ("3","","j"), ("3","","m"), ("1","","n")]
       expect x $ \di0 -> do
          Di.log di0 "1" "a"
          -- Predicates
          Di.log (Di.filter (/= "1") di0) "1" "b"
          Di.log (Di.filter (/= "1") di0) "2" "c"
          -- Identity
          Di.log (Di.filter (const True) di0) "1" "d"
          -- Composition
          Di.log ((Di.filter (/= "1") . Di.filter (/= "2")) di0) "1" "e"
          Di.log ((Di.filter (/= "1") . Di.filter (/= "2")) di0) "2" "f"
          Di.log ((Di.filter (/= "1") . Di.filter (/= "2")) di0) "3" "g"
          Di.log (Di.filter (liftA2 (&&) (/= "1") (/= "2")) di0) "1" "h"
          Di.log (Di.filter (liftA2 (&&) (/= "1") (/= "2")) di0) "2" "i"
          Di.log (Di.filter (liftA2 (&&) (/= "1") (/= "2")) di0) "3" "j"
          -- Conmutativity (c.f., "e" "f" "g")
          Di.log ((Di.filter (/= "2") . Di.filter (/= "1")) di0) "1" "k"
          Di.log ((Di.filter (/= "2") . Di.filter (/= "1")) di0) "2" "l"
          Di.log ((Di.filter (/= "2") . Di.filter (/= "1")) di0) "3" "m"
          -- Checking that di0 still works
          Di.log di0 "1" "n"
          -}
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

instance QC.Arbitrary a => QC.Arbitrary (NEL.NonEmpty a) where
  arbitrary = (NEL.:|) <$> QC.arbitrary <*> QC.arbitrary
  shrink (a NEL.:| []) = []
  shrink (a NEL.:| as) = fmap (a NEL.:|) (QC.shrink as)
