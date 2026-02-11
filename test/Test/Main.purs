module Test.Main where

import Prelude

import Data.Array as A
import Effect (Effect)
import Effect.Console (log)
import Test.Crypto.SHA3 as SHA3Tests
import Test.Crypto.SHA3.Bench as Bench

foreign import argv :: Array String

main :: Effect Unit
main = do
  let args = argv
  SHA3Tests.main
  if A.elem "--bench" args then do
    log ""
    Bench.benchSuite
  else
    log "\n(run with --bench to include benchmarks)"