{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ext.Plutus.V2.Ledger.Contexts where

import Ledger.Value (valueOf)
import Plutus.V2.Ledger.Api
  ( CurrencySymbol,
    TokenName,
    TxInInfo (txInInfoResolved),
    TxOut (txOutValue),
  )
import PlutusTx.Prelude

{-# INLINEABLE filterInputsByToken #-}
filterInputsByToken :: CurrencySymbol -> TokenName -> [TxInInfo] -> [TxInInfo]
filterInputsByToken cur tn = filter findToken
  where
    findToken i =
      let input = txInInfoResolved i
       in valueOf (txOutValue input) cur tn == 1

{-# INLINEABLE filterOutputsByToken #-}
filterOutputsByToken :: CurrencySymbol -> TokenName -> [TxOut] -> [TxOut]
filterOutputsByToken cur tn = filter findToken
  where
    findToken o = valueOf (txOutValue o) cur tn == 1
