{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings#-}

module Main where

import qualified Test.QuickCheck as QC

import qualified Di
import qualified Di.Gen

--------------------------------------------------------------------------------

main :: IO ()
main = do
  sample 1000

--------------------------------------------------------------------------------

sample :: Int -> IO ()
sample n = Di.Gen.ioPrintLogs =<< QC.generate (fmap (take n) Di.Gen.genLogs)

