{-# LANGUAGE NoImplicitPrelude #-}

module Ext.PlutusTx.List where

import PlutusTx.Prelude

{-# INLINEABLE replicate #-}
replicate :: Integer -> a -> [a]
replicate n a =
  if n <= 0
    then []
    else a : replicate (n - 1) a
