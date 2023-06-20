{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ext.PlutusTx.List where

import PlutusTx.Prelude

{-# INLINEABLE replicate #-}
replicate :: Integer -> a -> [a]
replicate !n a =
  if n <= 0
    then []
    else goReplicate n
  where
    goReplicate !k =
      if k == 0
        then []
        else a : goReplicate (k - 1)

{-# INLINEABLE drop #-}
drop :: Integer -> [a] -> [a]
drop !n ts =
  if n <= 0
    then ts
    else goDrop n ts
    where
      goDrop !k xs =
        if k == 0
          then xs
          else case xs of
            [] -> []
            _ : rs -> goDrop (k - 1) rs
