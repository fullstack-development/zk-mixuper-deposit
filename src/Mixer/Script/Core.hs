{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Mixer.Script.Core where

import Ext.Plutus.V2.Ledger.Contexts (filterInputsByToken)
import Ext.PlutusTx.Builtins (byteString2Integer)
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts (mkUntypedValidator)
import Ledger.Value (valueOf)
import Mixer.Datum (Commitment, DepositDatum (..), MixerConfig (..), MixerDatum (..), MixerRedeemer (..))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api
  ( ScriptContext,
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoOutputs),
    TxOut (txOutValue),
    Value,
    scriptContextTxInfo,
    txInfoInputs,
  )
import Plutus.V2.Ledger.Contexts (findOwnInput)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Service.MerkleTree as T
import Service.ProtocolToken (getNextState)

data MixerScript

instance Scripts.ValidatorTypes MixerScript where
  type RedeemerType MixerScript = MixerRedeemer
  type DatumType MixerScript = MixerDatum

depositScript :: MixerConfig -> Scripts.TypedValidator MixerScript
depositScript cfg =
  Scripts.mkTypedValidator @MixerScript
    ( $$(PlutusTx.compile [||validatorLogic||])
        `PlutusTx.applyCode` PlutusTx.liftCode cfg
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @ScriptContext @MixerDatum @MixerRedeemer

{-# INLINEABLE validatorLogic #-}
validatorLogic ::
  MixerConfig ->
  MixerDatum ->
  MixerRedeemer ->
  ScriptContext ->
  Bool
validatorLogic _ _ magicNumber _ =
  traceIfFalse "Wrong redeemer" (magicNumber == 42)
