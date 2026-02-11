module Test.Crypto.SHA3.Bench where

import Prelude

import Crypto.SHA3 (SHA3(..), hash, shake128, shake256)
import Crypto.SHA3.Keccak as Keccak
import Data.Array as A
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.Buffer (Buffer)

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

foreign import performanceNow :: Effect Number

-- | Defer a pure computation into Effect so it is re-evaluated each call.
foreign import defer :: forall a. (Unit -> a) -> Effect a

-------------------------------------------------------------------------------
-- Timing Helpers
-------------------------------------------------------------------------------

-- | Run an action N times and return total elapsed milliseconds.
timeN :: Int -> Effect Unit -> Effect Number
timeN n action = do
  t0 <- performanceNow
  go 0
  t1 <- performanceNow
  pure (t1 - t0)
  where
  go i
    | i >= n = pure unit
    | otherwise = action *> go (i + 1)

-- | Format a benchmark result line.
report :: String -> Int -> Int -> Number -> Effect Unit
report label iterations inputBytes ms = do
  let
    throughputMBs =
      if ms > 0.0 then
        (toNumber (iterations * inputBytes) / 1048576.0) / (ms / 1000.0)
      else 0.0
    opsPerSec =
      if ms > 0.0 then toNumber iterations / (ms / 1000.0)
      else 0.0
  log $ "  " <> label
    <> "  " <> show iterations <> " iters"
    <> "  " <> showFixed ms <> " ms"
    <> "  " <> showFixed opsPerSec <> " ops/s"
    <> "  " <> showFixed throughputMBs <> " MB/s"

showFixed :: Number -> String
showFixed = show

-------------------------------------------------------------------------------
-- Input Generators
-------------------------------------------------------------------------------

-- | Create a Buffer of N zero bytes.
zeroBuffer :: Int -> Effect Buffer
zeroBuffer n = Buffer.fromArray (A.replicate n 0)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchSuite :: Effect Unit
benchSuite = do
  log "═══════════════════════════════════════════════════════════"
  log "  SHA-3 Benchmarks"
  log "═══════════════════════════════════════════════════════════"

  -- Small inputs (empty, short messages)
  log "\n── SHA3-256 (small inputs) ─────────────────────────────"
  let iters = 500

  do
    buf <- zeroBuffer 0
    ms <- timeN iters (void $ defer \_ -> hash SHA3_256 buf)
    report "empty (0 B)" iters 0 ms

  do
    buf <- zeroBuffer 32
    ms <- timeN iters (void $ defer \_ -> hash SHA3_256 buf)
    report "32 B       " iters 32 ms

  do
    buf <- zeroBuffer 64
    ms <- timeN iters (void $ defer \_ -> hash SHA3_256 buf)
    report "64 B       " iters 64 ms

  do
    buf <- zeroBuffer 136
    ms <- timeN iters (void $ defer \_ -> hash SHA3_256 buf)
    report "136 B (1×r)" iters 136 ms

  -- Medium inputs (multi-block)
  log "\n── SHA3-256 (multi-block) ──────────────────────────────"
  let itersM = 100

  do
    buf <- zeroBuffer 512
    ms <- timeN itersM (void $ defer \_ -> hash SHA3_256 buf)
    report "512 B      " itersM 512 ms

  do
    buf <- zeroBuffer 1024
    ms <- timeN itersM (void $ defer \_ -> hash SHA3_256 buf)
    report "1 KiB      " itersM 1024 ms

  do
    buf <- zeroBuffer 4096
    ms <- timeN itersM (void $ defer \_ -> hash SHA3_256 buf)
    report "4 KiB      " itersM 4096 ms

  -- Large inputs
  log "\n── SHA3-256 (large inputs) ─────────────────────────────"
  let itersL = 10

  do
    buf <- zeroBuffer 65536
    ms <- timeN itersL (void $ defer \_ -> hash SHA3_256 buf)
    report "64 KiB     " itersL 65536 ms

  do
    buf <- zeroBuffer 1048576
    ms <- timeN itersL (void $ defer \_ -> hash SHA3_256 buf)
    report "1 MiB      " itersL 1048576 ms

  -- Compare all SHA3 variants on same input
  log "\n── All SHA3 variants (256 B input) ─────────────────────"
  let itersV = 200

  do
    buf <- zeroBuffer 256
    ms224 <- timeN itersV (void $ defer \_ -> hash SHA3_224 buf)
    report "SHA3-224   " itersV 256 ms224
    ms256 <- timeN itersV (void $ defer \_ -> hash SHA3_256 buf)
    report "SHA3-256   " itersV 256 ms256
    ms384 <- timeN itersV (void $ defer \_ -> hash SHA3_384 buf)
    report "SHA3-384   " itersV 256 ms384
    ms512 <- timeN itersV (void $ defer \_ -> hash SHA3_512 buf)
    report "SHA3-512   " itersV 256 ms512

  -- SHAKE XOFs
  log "\n── SHAKE XOFs (256 B input) ────────────────────────────"

  do
    buf <- zeroBuffer 256
    ms128 <- timeN itersV (void $ defer \_ -> shake128 32 buf)
    report "SHAKE128/32" itersV 256 ms128
    ms256 <- timeN itersV (void $ defer \_ -> shake256 64 buf)
    report "SHAKE256/64" itersV 256 ms256

  -- Keccak permutation alone
  log "\n── Keccak-f[1600] permutation (raw) ────────────────────"
  let itersK = 1000
  do
    let emptyState = A.replicate 25 { hi: 0, lo: 0 }
    ms <- timeN itersK (void $ defer \_ -> Keccak.keccakF1600 emptyState)
    report "keccakF1600" itersK 200 ms

  log "\n═══════════════════════════════════════════════════════════"
  log "  Done."
  log "═══════════════════════════════════════════════════════════"