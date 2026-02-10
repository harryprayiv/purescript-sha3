-- | SHA-3 (FIPS 202) cryptographic hash functions and extendable-output functions.
-- |
-- | Pure PureScript implementation of the Keccak-f[1600] permutation and
-- | sponge construction, as specified in NIST FIPS 202 (August 2015).
-- |
-- | This version targets the purescm (Chez Scheme) backend, using native
-- | 64-bit integers for Keccak lanes instead of the JS backend's hi/lo pairs.
-- |
-- | Usage:
-- | ```purescript
-- | import Crypto.SHA3 (SHA3(..), hash, toString)
-- |
-- | digest = hash SHA3_256 "hello world"
-- | hex    = toString digest
-- | ```
module Crypto.SHA3
  ( SHA3(..)
  , Digest
  , class Hashable
  , hash
  , sha3_224
  , sha3_256
  , sha3_384
  , sha3_512
  , shake128
  , shake256
  , toBytes
  , toString
  , fromHex
  , fromBytes
  ) where

import Prelude

import Crypto.Keccak as Keccak
import Data.Maybe (Maybe(..))

-------------------------------------------------------------------------------
-- FFI (Chez Scheme)
-------------------------------------------------------------------------------

-- | Convert a String to an array of UTF-8 byte values.
foreign import stringToUtf8 :: String -> Array Int

-- | Encode an array of bytes as a lowercase hex string.
foreign import bytesToHex :: Array Int -> String

-- | Decode a hex string to an array of bytes, or Nothing if invalid.
foreign import hexToBytes_ :: (Array Int -> Maybe (Array Int))
                           -> Maybe (Array Int)
                           -> String
                           -> Maybe (Array Int)

hexToBytes :: String -> Maybe (Array Int)
hexToBytes = hexToBytes_ Just Nothing

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | SHA-3 hash function variants.
data SHA3 = SHA3_224 | SHA3_256 | SHA3_384 | SHA3_512

-- | The output of a SHA-3 hash function, stored as raw bytes.
newtype Digest = Digest (Array Int)

instance eqDigest :: Eq Digest where
  eq (Digest a) (Digest b) = a == b

instance showDigest :: Show Digest where
  show d = "(Digest " <> toString d <> ")"

-------------------------------------------------------------------------------
-- Hashable
-------------------------------------------------------------------------------

-- | Types that can be hashed with a SHA-3 function.
class Hashable a where
  hash :: SHA3 -> a -> Digest

instance hashableString :: Hashable String where
  hash variant value = hashBytes variant (stringToUtf8 value)

instance hashableBytes :: Hashable (Array Int) where
  hash = hashBytes

hashBytes :: SHA3 -> Array Int -> Digest
hashBytes variant bytes =
  let
    rateBytes = variantRate variant
    outBytes  = variantLength variant
    result    = Keccak.sponge rateBytes 0x06 outBytes bytes
  in
    Digest result

-------------------------------------------------------------------------------
-- SHA-3 Hash Functions
-------------------------------------------------------------------------------

-- | SHA3-224: 224-bit (28-byte) digest, rate = 1152 bits.
sha3_224 :: Array Int -> Digest
sha3_224 = hash SHA3_224

-- | SHA3-256: 256-bit (32-byte) digest, rate = 1088 bits.
sha3_256 :: Array Int -> Digest
sha3_256 = hash SHA3_256

-- | SHA3-384: 384-bit (48-byte) digest, rate = 832 bits.
sha3_384 :: Array Int -> Digest
sha3_384 = hash SHA3_384

-- | SHA3-512: 512-bit (64-byte) digest, rate = 576 bits.
sha3_512 :: Array Int -> Digest
sha3_512 = hash SHA3_512

-------------------------------------------------------------------------------
-- SHA-3 Extendable-Output Functions (XOFs)
-------------------------------------------------------------------------------

-- | SHAKE128: 128-bit security, variable output length.
-- | First argument is the desired output length in bytes.
shake128 :: Int -> Array Int -> Array Int
shake128 outputBytes bytes =
  Keccak.sponge 168 0x1F outputBytes bytes

-- | SHAKE256: 256-bit security, variable output length.
-- | First argument is the desired output length in bytes.
shake256 :: Int -> Array Int -> Array Int
shake256 outputBytes bytes =
  Keccak.sponge 136 0x1F outputBytes bytes

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- | Extract the raw bytes from a digest.
toBytes :: Digest -> Array Int
toBytes (Digest bs) = bs

-- | Wrap raw bytes as a digest. No validation is performed on length.
fromBytes :: Array Int -> Maybe Digest
fromBytes = Just <<< Digest

-- | Hex-encode a digest.
toString :: Digest -> String
toString (Digest bs) = bytesToHex bs

-- | Decode a hex string to a digest.
fromHex :: String -> Maybe Digest
fromHex s = map Digest (hexToBytes s)

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

variantRate :: SHA3 -> Int
variantRate SHA3_224 = 144
variantRate SHA3_256 = 136
variantRate SHA3_384 = 104
variantRate SHA3_512 = 72

variantLength :: SHA3 -> Int
variantLength SHA3_224 = 28
variantLength SHA3_256 = 32
variantLength SHA3_384 = 48
variantLength SHA3_512 = 64