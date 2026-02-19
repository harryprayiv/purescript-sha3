-- | Internal Keccak-f[1600] permutation and sponge construction.
-- |
-- | This module delegates to optimized Chez Scheme FFI for the hot path
-- | (mutable vectors, bytevectors) while keeping the PureScript API pure.
module Crypto.Keccak
  ( sponge
  , keccakF1600
  , ByteArray
  , spongeNativeBv
  ) where

import Crypto.Word64 (Word64)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Bytes = Array Int
type State = Array Word64

-- | Opaque byte array — on Chez this IS a native bytevector, zero wrapping.
foreign import data ByteArray :: Type

-------------------------------------------------------------------------------
-- FFI — optimized Chez Scheme implementations
-------------------------------------------------------------------------------

-- | Optimized sponge (legacy): converts flexvector → bytevector internally.
foreign import spongeOptimized :: Int -> Int -> Int -> Bytes -> Bytes

-- | Native bytevector sponge: zero conversion overhead.
-- | Takes and returns raw bytevectors.
foreign import spongeNativeBv :: Int -> Int -> Int -> ByteArray -> ByteArray

-- | Optimized keccakF1600.
foreign import keccakF1600Optimized :: State -> State

-- Expose under the original names so existing code doesn't change.

sponge :: Int -> Int -> Int -> Bytes -> Bytes
sponge = spongeOptimized

keccakF1600 :: State -> State
keccakF1600 = keccakF1600Optimized