-- | Internal Keccak-f[1600] permutation and sponge construction.
-- |
-- | Optimized JavaScript FFI implementation of the core algorithms
-- | from NIST FIPS 202. Not intended for direct use — see `Crypto.SHA3`
-- | for the public API.
module Crypto.SHA3.Keccak
  ( sponge
  , keccakF1600
  , Lane
  , State
  ) where

-------------------------------------------------------------------------------
-- Types (exported for testing/benchmarking)
-------------------------------------------------------------------------------

-- | A 64-bit lane, split into two 32-bit halves (lo = bits 0–31, hi = 32–63).
type Lane = { hi :: Int, lo :: Int }

-- | The Keccak state: 25 lanes indexed by (x + 5*y).
type State = Array Lane

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

-- | Full sponge construction: absorb + squeeze.
-- | sponge rateBytes suffixByte outputBytes message
foreign import sponge :: Int -> Int -> Int -> Array Int -> Array Int

-- | Keccak-f[1600] permutation on a 25-lane state.
-- | Primarily exposed for benchmarking; the hot path uses `sponge` directly.
foreign import keccakF1600 :: State -> State