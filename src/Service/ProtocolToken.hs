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
import Ext.Plutus.V2.Ledger.Contexts (filterOutputsByToken)
import Ledger.Value (CurrencySymbol, valueOf)
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Api as Ledger
import Plutus.V2.Ledger.Api
  ( Datum (getDatum),
    FromData,
    OutputDatum (OutputDatum),
    TokenName,
    TxOut (txOutDatum, txOutValue),
    TxOutRef (TxOutRef),
    Value,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude

mkCurrency :: TxOutRef -> [TokenName] -> Currency.OneShotCurrency
mkCurrency (TxOutRef h i) names =
  Currency.OneShotCurrency
    { Currency.curRefTransactionOutput = (h, i),
      Currency.curAmounts = AssocMap.fromList $ fmap (,1) names
    }

{-# INLINEABLE getNextState #-}
getNextState ::
  (FromData d) =>
  CurrencySymbol ->
  TokenName ->
  [TxOut] ->
  (d, Value)
getNextState cur tn outs = case filterOutputsByToken cur tn outs of
  [o] -> (getNextStateDatum o, txOutValue o)
  _ -> traceError "Exacly one script output should be created for next script state"
  where
    getNextStateDatum o = case txOutDatum o of
      OutputDatum d -> case PlutusTx.fromBuiltinData . getDatum $ d of
        Just r -> r
        Nothing -> traceError msg
      _ -> traceError msg
    msg = "Script output inline datum not found"

{-# INLINEABLE containsOneProtocolToken #-}
containsOneProtocolToken :: Value -> CurrencySymbol -> TokenName -> Bool
containsOneProtocolToken val cur tn = 1 == valueOf val cur tn

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
