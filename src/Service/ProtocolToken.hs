{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- Interface to Plutus.Contracts.Currency
module Service.ProtocolToken where

import Ledger.Value (AssetClass (unAssetClass), assetClass, assetClassValueOf)
import qualified Plutus.Contracts.Currency as Currency
import Plutus.V2.Ledger.Api
  ( Datum (getDatum),
    FromData,
    OutputDatum (OutputDatum),
    ScriptContext,
    TokenName,
    TxOut (txOutDatum, txOutValue),
    TxOutRef (TxOutRef),
    Value,
  )
import Plutus.V2.Ledger.Contexts (getContinuingOutputs)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude

mkProtocolToken :: TxOutRef -> TokenName -> AssetClass
mkProtocolToken (TxOutRef h i) tn =
  let currency =
        Currency.OneShotCurrency
          { Currency.curRefTransactionOutput = (h, i),
            Currency.curAmounts = AssocMap.singleton tn 1
          }
   in assetClass (Currency.currencySymbol currency) tn

mkCurrency :: TxOutRef -> AssetClass -> Currency.OneShotCurrency
mkCurrency (TxOutRef h i) token =
  Currency.OneShotCurrency
    { Currency.curRefTransactionOutput = (h, i),
      Currency.curAmounts = AssocMap.singleton (snd $ unAssetClass token) 2
    }

{-# INLINEABLE getNextState #-}
getNextState ::
  (FromData d) =>
  AssetClass ->
  ScriptContext ->
  (d, Value)
getNextState protocolToken ctx = case getContinuingOutputs ctx of
  [o] -> (getNextStateDatum o, getNextStateValue o)
  _ -> traceError "Exacly one script output should be created for next script state"
  where
    getNextStateValue o =
      let val = txOutValue o
       in if containsOneProtocolToken val protocolToken
            then val
            else traceError "Script output does not have Protocol Token"
    getNextStateDatum o = case txOutDatum o of
      OutputDatum d -> case PlutusTx.fromBuiltinData . getDatum $ d of
        Just r -> r
        Nothing -> traceError msg
      _ -> traceError msg
    msg = "Script output inline datum not found"

{-# INLINEABLE containsOneProtocolToken #-}
containsOneProtocolToken :: Value -> AssetClass -> Bool
containsOneProtocolToken val token = 1 == assetClassValueOf val token
