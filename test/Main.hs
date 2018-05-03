{-# LANGUAGE CPP #-}
module Main where

import Debug.Trace
import Control.Applicative (liftA2)
import qualified Control.Exception as Ex
import Control.Concurrent.STM
  (STM, atomically, TQueue, newTQueue, writeTQueue, tryReadTQueue)
import Data.Foldable (for_)
import Data.Function (fix)
import Data.Monoid (Sum(Sum, getSum))
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import Di (Di)
import qualified Di

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "di"
  [ QC.testProperty "log" $ do
      QC.forAll QC.arbitrary $ \lpms ->
        QC.ioProperty $ do
          expect lpms $ \di0 -> do
            for_ lpms $ \(l, p, m) -> do
              Di.log (Di.push p di0) l m

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
       let x = [("1","","a"), ("2","","c"), ("1","","d"), ("3","","g"), ("3","","j"), ("3","","m"), ("1", "1", "o"), ("1", "1", "p"), ("1","","q")]
       expect x $ \di0 -> do
          Di.log di0 "1" "a"
          -- Predicates
          Di.log (Di.filter (\l _ _ -> l /= "1") di0) "1" "b"
          Di.log (Di.filter (\l _ _ -> l /= "1") di0) "2" "c"
          -- Identity
          Di.log (Di.filter (\_ _ _ -> True) di0) "1" "d"
          -- Composition
          Di.log ((Di.filter (\l _ _ -> l /= "1") . Di.filter (\l _ _ -> l /= "2")) di0) "1" "e"
          Di.log ((Di.filter (\l _ _ -> l /= "1") . Di.filter (\l _ _ -> l /= "2")) di0) "2" "f"
          Di.log ((Di.filter (\l _ _ -> l /= "1") . Di.filter (\l _ _ -> l /= "2")) di0) "3" "g"
          Di.log (Di.filter (\l _ _ -> l /= "1" && l /= "2") di0) "1" "h"
          Di.log (Di.filter (\l _ _ -> l /= "1" && l /= "2") di0) "2" "i"
          Di.log (Di.filter (\l _ _ -> l /= "1" && l /= "2") di0) "3" "j"
          -- Conmutativity (c.f., "e" "f" "g")
          Di.log ((Di.filter (\l _ _ -> l /= "2") . Di.filter (\l _ _ -> l /= "1")) di0) "1" "k"
          Di.log ((Di.filter (\l _ _ -> l /= "2") . Di.filter (\l _ _ -> l /= "1")) di0) "2" "l"
          Di.log ((Di.filter (\l _ _ -> l /= "2") . Di.filter (\l _ _ -> l /= "1")) di0) "3" "m"
          -- Filter based on path
          Di.log ((Di.filter (\_ p _ -> p == "1") di0)) "1" "n"
          Di.log ((Di.filter (\_ p _ -> p == "1") (Di.push "1" di0))) "1" "o"
          -- Push and filter commute
          Di.log ((Di.push "1" (Di.filter (\_ p _ -> p == "1") di0))) "1" "p"
          -- Checking that di0 still works
          Di.log di0 "1" "q"
  ]

expect :: [(String, String, String)] -> (Di String String String -> IO a) -> IO a
expect as0 k = Ex.bracket
  (do tq <- atomically newTQueue
      di <- Di.mkDi (\_ l p m -> atomically (writeTQueue tq (l, p, m)))
      pure (tq, di))
  (\(tq, di) -> do
      Di.flush di
      as1 <- atomically (drainTQueue tq)
      as1 @?= as0)
  (\(_, di) -> k di)

drainTQueue :: TQueue a -> STM [a]
drainTQueue tq = do
  let go as = maybe (pure as) (\a -> go (a:as)) =<< tryReadTQueue tq
  fmap reverse (go [])

#if !MIN_VERSION_QuickCheck(2,10,0)
instance QC.Testable () where
  property () = QC.property True
#endif
