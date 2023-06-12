{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- Interface to Plutus.Contracts.Currency
module Service.ProtocolToken where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger.Value (AssetClass (..), assetClass, assetClassValueOf)
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Api as Ledger
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
            Currency.curAmounts = AssocMap.singleton tn 2
          }
   in assetClass (Currency.currencySymbol currency) tn

mkCurrency :: TxOutRef -> TokenName -> Currency.OneShotCurrency
mkCurrency (TxOutRef h i) tn =
  Currency.OneShotCurrency
    { Currency.curRefTransactionOutput = (h, i),
      Currency.curAmounts = AssocMap.singleton tn 2
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

plutusScript :: Currency.OneShotCurrency -> Ledger.Script
plutusScript =
  Ledger.unMintingPolicyScript . Currency.curPolicy

validator :: Currency.OneShotCurrency -> Ledger.Validator
validator = Ledger.Validator . plutusScript

scriptAsCbor :: Currency.OneShotCurrency -> LB.ByteString
scriptAsCbor = serialise . validator

cardanoApiMintingScript :: Currency.OneShotCurrency -> PlutusScript PlutusScriptV1
cardanoApiMintingScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

scriptShortBs :: Currency.OneShotCurrency -> SBS.ShortByteString
scriptShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
