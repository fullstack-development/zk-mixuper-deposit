{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Ext.PlutusTx.List where

import PlutusTx.Prelude

{-# INLINEABLE replicate #-}
replicate :: Integer -> a -> [a]
replicate n a =
  if n <= 0
    then []
    else a : replicate (n - 1) a
