{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Ext.PlutusTx.Numeric where

import PlutusTx.Prelude

infixr 8 ^

{-# INLINEABLE (^) #-}
(^) :: (MultiplicativeMonoid a) => a -> Integer -> a
x0 ^ y0
  | y0 < 0 = traceError "Negative exponent"
  | y0 == 0 = one
  | otherwise = f x0 y0
  where
    -- f : x0 ^ y0 = x ^ y
    f x y
      | even y = f (x * x) (y `quotient` 2)
      | y == 1 = x
      | otherwise = g (x * x) (y `quotient` 2) x
    -- g : x0 ^ y0 = (x ^ y) * z
    g x y z
      | even y = g (x * x) (y `quotient` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) (y `quotient` 2) (x * z)
