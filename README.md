# SHA-3 for PureScript — Chez Scheme Backend

A pure PureScript implementation of SHA-3 (FIPS 202) targeting the
[purescm](https://github.com/purescm/purescm) Chez Scheme backend.

## Why Rewrite?

The JavaScript backend limits PureScript's `Int` to 32-bit signed range
`[-2^31, 2^31-1]`. Keccak-f[1600] operates on 25 × **64-bit** lanes,
so the JS version had to split every lane into `{ hi :: Int, lo :: Int }`
pairs and thread two-halves arithmetic through every operation:

```purescript
-- JS backend: every operation is doubled
type Lane = { hi :: Int, lo :: Int }

xorLane :: Lane -> Lane -> Lane
xorLane a b = { hi: xor a.hi b.hi, lo: xor a.lo b.lo }

rotL :: Lane -> Int -> Lane
rotL lane n
  | n < 32  = { hi: (lane.hi `shl` n) .|. (lane.lo `zshr` (32 - n))
              , lo: (lane.lo `shl` n) .|. (lane.hi `zshr` (32 - n)) }
  | ...     -- 4 more cases
```

On Chez Scheme, integers are arbitrary-precision with hardware fixnums
up to 61 bits and seamless bignum promotion. All of that collapses to:

```purescript
-- Chez backend: one native integer per lane
type Word64 = Int

foreign import w64xor  :: Word64 -> Word64 -> Word64
foreign import w64rotL :: Word64 -> Int -> Word64
```

The Scheme FFI is trivial:

```scheme
(define w64xor
  (lambda (a) (lambda (b)
    (logand (logxor a b) #xFFFFFFFFFFFFFFFF))))

(define w64rotL
  (lambda (x) (lambda (n)
    (logand (logior (ash x n) (ash x (- n 64)))
            #xFFFFFFFFFFFFFFFF))))
```

## What Changed

| Aspect | JS Backend | Chez Backend |
|--------|-----------|--------------|
| Lane representation | `{ hi :: Int, lo :: Int }` record | Single `Int` (= Word64) |
| Bitwise ops | Manual hi/lo threading, 4+ cases for rotL | Direct `logxor`/`logand`/`logior`/`ash` |
| Round constants | 24 × `{ hi, lo }` records with `b31` helper | 24 × native hex literals like `#x8000000080008008` |
| Buffer FFI | Node.js `Buffer` via 6 JS FFI functions | Chez `string->utf8` + bytevector ops |
| `Node.Buffer` dependency | Required | Eliminated |
| `Data.Int.Bits` dependency | Required (`shl`, `zshr`, `xor`, `.&.`, `.|.`) | Eliminated (ops in Scheme FFI) |

## Architecture

```
src/
  Crypto/
    SHA3.purs          -- Public API (Hashable, Digest, SHA3 variants, SHAKE)
    SHA3.ss            -- Chez FFI: stringToUtf8, bytesToHex, hexToBytes
    Keccak.purs        -- Sponge construction + Keccak-f[1600] permutation
    Keccak.ss          -- Chez FFI: roundConstants (24 × 64-bit), orInt
    Word64.purs        -- Word64 type + operations (PureScript interface)
    Word64.ss          -- Chez FFI: w64xor, w64and, w64rotL, etc.
test/
  Test/
    SHA3.purs          -- NIST test vectors (identical expected values)
```

## Building

Requires `purescm` (the Chez Scheme backend for PureScript) and Chez Scheme.

```bash
spago build
purescm run
```

## Test Vectors

All test vectors are from NIST FIPS 202 and match the JS backend exactly:
SHA3-224, SHA3-256, SHA3-384, SHA3-512, SHAKE128, SHAKE256, multi-block inputs.

## Notes on Chez Scheme's Integer Model

- **Fixnums**: On 64-bit Chez, fixnums cover `[-2^60, 2^60-1]` (61-bit,
  3 tag bits). Most Keccak intermediate values fit in fixnums.
- **Bignums**: Values exceeding fixnum range (like round constants with
  bit 63 set, e.g. `#x8000000080008008`) promote to bignums transparently.
  Chez's bignum arithmetic is highly optimized.
- **Masking**: We mask with `#xFFFFFFFFFFFFFFFF` after complement and
  shift operations that could produce values outside `[0, 2^64-1]`.
  XOR and AND of two 64-bit values stay in range naturally.

## FFI Library Naming Convention

Each `.ss` file uses the purescm convention:

```scheme
(library (Crypto.Word64 foreign)
  (export ...)
  (import (chezscheme))
  ...)
```

The library name is `(<ModuleName> foreign)` where `<ModuleName>` matches
the PureScript module name. All functions must be curried (nested lambdas)
to match PureScript's calling convention.