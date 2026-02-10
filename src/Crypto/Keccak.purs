-- | Internal Keccak-f[1600] permutation and sponge construction.
-- |
-- | This module implements the core algorithms from NIST FIPS 202.
-- | Targeting the purescm (Chez Scheme) backend, we use native 64-bit
-- | integers for Keccak lanes — no more hi/lo splitting!
-- |
-- | It is not intended for direct use — see `Crypto.SHA3` for the public API.
module Crypto.Keccak
  ( sponge
  , keccakF1600
  ) where

import Prelude

import Crypto.Word64
  ( Word64
  , w64xor
  , w64and
  , w64complement
  , w64rotL
  , w64zero
  , w64fromBytesLE
  , w64toBytesLE
  )
import Data.Array as A
import Data.Array ((!!))
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Bytes = Array Int

-- | The Keccak state: 25 × 64-bit lanes indexed by (x + 5*y).
-- | Each lane is a single native integer — Chez Scheme handles this
-- | with fixnums/bignums transparently.
type State = Array Word64

-------------------------------------------------------------------------------
-- State Helpers
-------------------------------------------------------------------------------

at :: State -> Int -> Int -> Word64
at st x y = fromMaybe w64zero (st !! (x + 5 * y))

stateFromFn :: (Int -> Int -> Word64) -> State
stateFromFn f = do
  y <- A.range 0 4
  x <- A.range 0 4
  pure (f x y)

emptyState :: State
emptyState = A.replicate 25 w64zero

-------------------------------------------------------------------------------
-- State ↔ Bytes
-------------------------------------------------------------------------------

xorBytesIntoState :: Bytes -> Int -> State -> State
xorBytesIntoState block rateBytes st =
  let numLanes = rateBytes / 8
  in A.mapWithIndex
      ( \i lane ->
          if i < numLanes then w64xor lane (w64fromBytesLE block (i * 8))
          else lane
      )
      st

extractBytes :: Int -> State -> Bytes
extractBytes rateBytes st =
  let numLanes = rateBytes / 8
  in A.take rateBytes (A.concatMap w64toBytesLE (A.take numLanes st))

-------------------------------------------------------------------------------
-- Round Constants (FIPS 202 §3.2.5)
--
-- These are the actual 64-bit RC values. On Chez Scheme we can
-- express them directly as integer literals — no hi/lo dance!
-------------------------------------------------------------------------------

foreign import roundConstants :: Array Word64

-------------------------------------------------------------------------------
-- ρ Offsets (FIPS 202 §3.2.2, Table 2)
-------------------------------------------------------------------------------

rhoOffsets :: Array Int
rhoOffsets =
  [  0,  1, 62, 28, 27
  , 36, 44,  6, 55, 20
  ,  3, 10, 43, 25, 39
  , 41, 45, 15, 21,  8
  , 18,  2, 61, 56, 14
  ]

-------------------------------------------------------------------------------
-- Step Mappings (FIPS 202 §3.2)
-------------------------------------------------------------------------------

theta :: State -> State
theta st =
  let
    c :: Int -> Word64
    c x = at st x 0 `w64xor` at st x 1 `w64xor` at st x 2
           `w64xor` at st x 3 `w64xor` at st x 4

    cArr = [ c 0, c 1, c 2, c 3, c 4 ]

    cAt :: Int -> Word64
    cAt i = fromMaybe w64zero (cArr !! i)

    d :: Int -> Word64
    d x = cAt ((x + 4) `mod` 5) `w64xor` w64rotL (cAt ((x + 1) `mod` 5)) 1

    dArr = [ d 0, d 1, d 2, d 3, d 4 ]

    dAt :: Int -> Word64
    dAt i = fromMaybe w64zero (dArr !! i)
  in
    stateFromFn (\x y -> at st x y `w64xor` dAt x)

rho :: State -> State
rho st =
  A.mapWithIndex
    (\i lane -> w64rotL lane (fromMaybe 0 (rhoOffsets !! i)))
    st

pi :: State -> State
pi st = stateFromFn (\x y -> at st ((x + 3 * y) `mod` 5) x)

chi :: State -> State
chi st = stateFromFn \x y ->
  at st x y `w64xor`
    (w64complement (at st ((x + 1) `mod` 5) y)
      `w64and` at st ((x + 2) `mod` 5) y)

iota :: Int -> State -> State
iota ir st =
  let
    rc = fromMaybe w64zero (roundConstants !! ir)
    lane0 = fromMaybe w64zero (st !! 0)
  in
    fromMaybe st (A.updateAt 0 (w64xor lane0 rc) st)

-------------------------------------------------------------------------------
-- Keccak-f[1600] (FIPS 202 §3.3)
-------------------------------------------------------------------------------

round :: Int -> State -> State
round ir = iota ir <<< chi <<< pi <<< rho <<< theta

keccakF1600 :: State -> State
keccakF1600 st = foldl (\s ir -> round ir s) st (A.range 0 23)

-------------------------------------------------------------------------------
-- Padding: pad10*1 (FIPS 202 §5.1)
-------------------------------------------------------------------------------

padMessage :: Int -> Int -> Bytes -> Bytes
padMessage suffixByte rateBytes message =
  let
    msgLen = A.length message
    q = rateBytes - (msgLen `mod` rateBytes)
  in
    if q == 1 then
      message <> [ suffixByte `orInt` 0x80 ]
    else
      message <> [ suffixByte ] <> A.replicate (q - 2) 0 <> [ 0x80 ]

-- | Bitwise OR on plain Int (for padding bytes, not lanes).
foreign import orInt :: Int -> Int -> Int

-------------------------------------------------------------------------------
-- Sponge (FIPS 202 §4)
-------------------------------------------------------------------------------

sponge :: Int -> Int -> Int -> Bytes -> Bytes
sponge rateBytes suffixByte outputBytes message =
  let
    padded = padMessage suffixByte rateBytes message
    numBlocks = A.length padded / rateBytes

    absorb :: State -> Int -> State
    absorb st blockIdx =
      let
        block = A.slice (blockIdx * rateBytes) ((blockIdx + 1) * rateBytes) padded
        xored = xorBytesIntoState block rateBytes st
      in
        keccakF1600 xored

    absorbed = foldl absorb emptyState (A.range 0 (numBlocks - 1))

    initialSqueeze = { out: extractBytes rateBytes absorbed, st: absorbed }

    squeezed = squeezeLoop outputBytes rateBytes initialSqueeze
  in
    A.take outputBytes squeezed.out

squeezeLoop
  :: Int
  -> Int
  -> { out :: Bytes, st :: State }
  -> { out :: Bytes, st :: State }
squeezeLoop needed rateBytes acc
  | A.length acc.out >= needed = acc
  | otherwise =
      let
        newSt = keccakF1600 acc.st
        extracted = extractBytes rateBytes newSt
      in
        squeezeLoop needed rateBytes { out: acc.out <> extracted, st: newSt }