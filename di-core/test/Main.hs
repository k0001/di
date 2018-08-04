{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (myThreadId, threadDelay)
import qualified Control.Exception as Ex (AsyncException(UserInterrupt, ThreadKilled),
   asyncExceptionFromException, asyncExceptionToException)
import qualified Control.Exception.Safe as Ex
import Control.Concurrent.STM
  (STM, atomically, retry,
   TQueue, newTQueueIO, writeTQueue, tryReadTQueue, flushTQueue)
import Data.Foldable (for_, toList)
import Data.Function (fix)
import qualified Data.List as List
import Data.Monoid (Sum(Sum, getSum))
import qualified Data.Time.Clock.System as Time
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@?=), (@=?))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.Runners as Tasty

import qualified Di.Core as Di

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "di-core"
  [ QC.testProperty "log one" $ do
      QC.forAll genLogInt $ \log0 -> do
        QC.ioProperty $ do
          expect [logMeta log0] $ \di0 -> do
             let (l,ps,m) = logMeta log0
             Di.log (pushFifo ps di0) l m

  , QC.testProperty "log many" $ do
      QC.forAll (QC.vectorOf 10 genLogInt) $ \logs0 -> do
        QC.ioProperty $ do
          expect (map logMeta logs0) $ \di0 -> do
            for_ logs0 $ \log_ -> do
              let (l,ps,m) = logMeta log_
              Di.log (pushFifo ps di0) l m

  , HU.testCase "push" $ do
       let x = [(0,[],0), (0,[0],0), (0,[1],0), (0,[1,2],0),
                (0,[1,2],0), (0,[1,2,3],0), (0,[1,2,3],0), (0,[],0)]
       expect x $ \di0 -> do
          Di.log di0 0 0
          -- Identity
          Di.log (Di.push 0 di0) 0 0
          -- Composition
          Di.log (Di.push 1 di0) 0 0
          Di.log ((Di.push 2 . Di.push 1) di0) 0 0
          Di.log (pushFifo [1,2] di0) 0 0
          Di.log ((Di.push 3 . Di.push 2 . Di.push 1) di0) 0 0
          Di.log (pushFifo [1,2,3] di0) 0 0
          -- Checking that di0 still works
          Di.log di0 0 0

  , HU.testCase "contralevel" $ do
       let x = [("1",[],0), ("1",[],0), ("1",[],0),
                ("2",[],0), ("2",[],0), ("1",[],0)]
           n = 1 :: Int
       expect (x :: [(String, [Int], Int)]) $ \di0 -> do
          Di.log di0 "1" 0
          -- Identity
          Di.log (Di.contralevel id di0) "1" 0
          -- Composition
          Di.log (Di.contralevel show di0) n 0
          Di.log ((Di.contralevel succ . Di.contralevel show) di0) n 0
          Di.log (Di.contralevel (show . succ) di0) n 0
          -- Checking that di0 still works
          Di.log di0 "1" 0

  , HU.testCase "contrapath" $ do
       let x = [(0,[],0), (0,["1"],0), (0,["1"],0), (0,["1"],0),
                (0,["2"],0), (0,["2"],0), (0,[],0)]
           n = 1 :: Int
       expect x $ \di0 -> do
          Di.log di0 0 0
          Di.log (Di.push "1" di0) 0 0
          -- Identity
          Di.log (Di.push "1" (Di.contrapath id di0)) 0 0
          -- Composition
          Di.log (Di.push n (Di.contrapath show di0)) 0 0
          Di.log (Di.push n (Di.contrapath (show . succ) di0)) 0 0
          Di.log (Di.push n ((Di.contrapath succ . Di.contrapath show) di0)) 0 0
          -- Checking that di0 still works
          Di.log di0 0 0

  , HU.testCase "contramsg" $ do
       let x = [(0,[],"1"), (0,[],"1"), (0,[],"1"),
                (0,[],"2"), (0,[],"2"), (0,[],"1")]
           n = 1 :: Int
       expect (x :: [(Int,[Int],String)]) $ \di0 -> do
          Di.log di0 0 "1"
          -- Identity
          Di.log (Di.contramsg id di0) 0 "1"
          -- Composition
          Di.log (Di.contramsg show di0) 0 n
          Di.log ((Di.contramsg succ . Di.contramsg show) di0) 0 n
          Di.log (Di.contramsg (show . succ) di0) 0 n
          -- Checking that di0 still works
          Di.log di0 0 "1"

  , HU.testCase "filter" $ do
       let x = [(1,[],"a"), (1,[],"b2"), (1,[],"b4"), (1,[],"b6"),
                (4,[1],"b8"), (4,[1,2],"b10"), (1,[],"c"),
                (3,[],"g"), (3,[],"j"), (3,[],"m"), (1,[],"n")]
       expect (x :: [(Int,[Int],String)]) $ \di0 -> do
          Di.log di0 1 "a"
          -- Predicates
          Di.log (Di.filter (\l ps m -> l /= 1) di0) 1 "b1"
          Di.log (Di.filter (\l ps m -> l == 1) di0) 1 "b2"

          Di.log (Di.filter (\l ps m -> m /= "b3") di0) 1 "b3"
          Di.log (Di.filter (\l ps m -> m == "b4") di0) 1 "b4"

          Di.log (Di.filter (\l ps m -> ps /= []) di0) 1 "b5"
          Di.log (Di.filter (\l ps m -> ps == []) di0) 1 "b6"

          Di.log (Di.push 1 (Di.filter (\l ps m -> ps /= [1]) di0)) 4 "b7"
          Di.log (Di.push 1 (Di.filter (\l ps m -> ps == [1]) di0)) 4 "b8"

          Di.log (Di.push 2 (Di.push 1 (Di.filter (\l ps m -> ps /= [1,2]) di0))) 4 "b9"
          Di.log (Di.push 2 (Di.push 1 (Di.filter (\l ps m -> ps == [1,2]) di0))) 4 "b10"

          -- Identity
          Di.log (Di.filter (\_ _ _ -> True) di0) 1 "c"

          -- Composition
          Di.log ((Di.filter (\l _ _ -> l /= 1) . Di.filter (\l _ _ -> l /= 2)) di0) 1 "e"
          Di.log ((Di.filter (\l _ _ -> l /= 1) . Di.filter (\l _ _ -> l /= 2)) di0) 2 "f"
          Di.log ((Di.filter (\l _ _ -> l /= 1) . Di.filter (\l _ _ -> l /= 2)) di0) 3 "g"
          Di.log (Di.filter (\l _ _ -> l /= 1 && l /= 2) di0) 1 "h"
          Di.log (Di.filter (\l _ _ -> l /= 1 && l /= 2) di0) 2 "i"
          Di.log (Di.filter (\l _ _ -> l /= 1 && l /= 2) di0) 3 "j"
          -- Conmutativity (c.f., "e" "f" "g")
          Di.log ((Di.filter (\l _ _ -> l /= 2) . Di.filter (\l _ _ -> l /= 1)) di0) 1 "k"
          Di.log ((Di.filter (\l _ _ -> l /= 2) . Di.filter (\l _ _ -> l /= 1)) di0) 2 "l"
          Di.log ((Di.filter (\l _ _ -> l /= 2) . Di.filter (\l _ _ -> l /= 1)) di0) 3 "m"
          -- Checking that di0 still works
          Di.log di0 1 "n"

  , HU.testCase "STM" $ do
       let x = [(2,[],"b"), (3,[],"c")] :: [(Int,[Int],String)]
           n = 1 :: Int
       (logs, a) <- withInMemoryDi $ \di0 -> do
          atomically $
             (Di.log' id di0 1 "a" >> retry) <|>
             (Di.log' id di0 2 "b" >> Di.log' id di0 3 "c")
       -- Check that 'retry' prevents logs from being commited.
       x @=? map logMeta logs
       -- Check that the timestamps are not all the same.
       2 @=? List.length (List.nub (List.sort (map Di.log_time logs)))

  , HU.testCase "Exceptions: Sync in commit" $ do
      let m = Di.new (const throwSyncException)
                     (\(di :: Di.Di () () ()) -> do
                          -- This delay here is to ensure the body has time
                          -- to receive the exception from the commit.
                          threadDelay 100000)
      Ex.tryAny m >>= \case
         Right () -> pure ()
         x -> fail ("Got " ++ show x)

  , HU.testCase "Exceptions: Sync in body" $ do
      let m = Di.new (const (pure ()))
                     (const throwSyncException)
      Ex.tryAny m >>= \case
         Left se | Ex.fromException se == Just (userError "foo") -> pure ()
         x -> fail ("Got " ++ show x)

  , HU.testCase "Exceptions: Async in commit" $ do
      let m = Di.new (const throwAsyncException)
                     (\(di :: Di.Di () () ()) -> do
                          Di.log di () ()
                          -- This delay here is to ensure the body has time
                          -- to receive the exception from the commit.
                          threadDelay 100000)
      Ex.tryAsync m >>= \case
         Left (se :: Ex.SomeException) ->
            case Ex.asyncExceptionFromException se of
               Just (Di.ExceptionInLoggingWorker se')
                  | Ex.asyncExceptionFromException se'
                      == Just (userError "bar") -> pure ()
               x -> fail ("Got " ++ show x)
         x -> fail ("Got " ++ show x)

  , HU.testCase "Exceptions: Async in body" $ do
      let m = Di.new (const (pure ()))
                     (const throwAsyncException)
      Ex.tryAsync m >>= \case
         Left (se :: Ex.SomeException)
           | Ex.asyncExceptionFromException se
                == Just (userError "bar") -> pure ()
         x -> fail ("Got " ++ show x)
  ]

throwSyncException :: Ex.MonadThrow m => m ()
throwSyncException = Ex.throwM (userError "foo")

throwAsyncException :: MonadIO m => m ()
throwAsyncException = liftIO $ do
  me <- myThreadId
  Ex.throwTo me (Ex.asyncExceptionToException (userError "bar"))

--------------------------------------------------------------------------------

withInMemoryDi
  :: (MonadIO m, Ex.MonadMask m)
  => (Di.Di level path msg -> m a)
  -> m ([Di.Log level path msg], a)
withInMemoryDi k = do
  tq <- liftIO newTQueueIO
  a <- Di.new (atomically . writeTQueue tq) k
  logs <- liftIO (atomically (flushTQueue tq))
  pure (logs, a)

expect
  :: (MonadIO m, Ex.MonadMask m,
      Eq level, Eq path, Eq msg,
      Show level, Show path, Show msg)
  => [(level, [path], msg)]
  -- ^ Each of the elements matches 'logMeta'.
  -> (Di.Di level path msg -> m a)
  -> m a
expect metas0 k = do
  (logs1, a) <- withInMemoryDi k
  liftIO (metas0 @=? map logMeta logs1)
  pure a

-- | Repeatedly run 'Di.push' on the given paths.
pushFifo
  :: [path]
  -- ^ Paths are in FIFO order. That is, the leftmost @path@ is the
  -- root path.
  -> Di.Di level path msg
  -> Di.Di level path msg
pushFifo [] = id
pushFifo (p:ps) = pushFifo ps . Di.push p

logMeta
  :: Di.Log level path msg
  -> (level, [path], msg)
  -- ^ Paths are in FIFO order. That is, the leftmost @path@ is the
  -- root path.
logMeta x = (Di.log_level x , toList (Di.log_path x) , Di.log_message x)


genLogInt :: QC.Gen (Di.Log Int Int Int)
genLogInt = Di.Log <$> QC.arbitrary <*> QC.arbitrary
                   <*> QC.arbitrary <*> QC.arbitrary

instance QC.Arbitrary Time.SystemTime where
  arbitrary = do
    a <- QC.choose (0, 253402300799) -- up to 4 digit years
    b <- QC.choose (0, 1000000000)
    pure (Time.MkSystemTime a b)

