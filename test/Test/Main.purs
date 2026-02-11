module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.SHA3 as SHA3Tests
import Test.SHA3.Bench as Bench

main :: Effect Unit
main = do
  SHA3Tests.main
  log ""
  Bench.benchSuite