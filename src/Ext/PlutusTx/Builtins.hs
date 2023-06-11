{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ext.PlutusTx.Builtins where

import Ext.PlutusTx.List (replicate)
import PlutusTx.Prelude

-- | Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
-- Can be used to compute inverse as if gcd(a,b) == 1 then ax == 1 mod b
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a b
  | b == 0 = (1, 0, a)
  | otherwise =
    let (q, r) = a `quotRem` b
        (s, t, g) = gcdExt b r
     in (t, s - q * t, g)
{-# INLINEABLE gcdExt #-}

-- | Convert byte sized integer to bits
byte2Bits :: Integer -> [Bool]
byte2Bits n
  | n == 0 = zeroArr
  | otherwise = take (8 - length binaryN) zeroArr <> binaryN
  where
    zeroArr = replicate 8 False
    binaryN = reverse $ go n
    go k
      | k == 0 = []
      | otherwise =
        let (d, m) = divMod k 2
         in if m == 0
              then False : go d
              else True : go d
{-# INLINEABLE byte2Bits #-}

-- | Convert bits to number
-- uses little-endian system (stores the least-significant bit at the smallest address)
bits2Num :: [Bool] -> Integer
bits2Num = fst . foldl add (0, 1)
  where
    add (num, powOf2) bit = (toI bit * powOf2 + num, 2 * powOf2)
    toI b = if b then 1 else 0
{-# INLINEABLE bits2Num #-}

-- | Convert bytes to number
bytes2Num :: [Integer] -> Integer
bytes2Num = bits2Num . foldMap byte2Bits
{-# INLINEABLE bytes2Num #-}

-- | Convert ByteString of arbitrary size to integer
byteString2Integer :: Integer -> BuiltinByteString -> Integer
byteString2Integer size bs = bytes2Num $ indexByteString bs <$> indexFrom 0
  where
    indexFrom i
      | i == size = []
      | otherwise = i : indexFrom (succ i)
{-# INLINEABLE byteString2Integer #-}
