{-# LANGUAGE TemplateHaskell #-}

module Mixer.Script.Core where

import qualified Ledger.Typed.Scripts as Scripts (mkUntypedValidator)
import Mixer.Datum (DepositConfig (..), DepositDatum (..), DepositRedeemer (..))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude

data DepositScript

instance Scripts.ValidatorTypes DepositScript where
  type RedeemerType DepositScript = DepositRedeemer
  type DatumType DepositScript = DepositDatum

depositScript :: DepositConfig -> Scripts.TypedValidator DepositScript
depositScript cfg =
  Scripts.mkTypedValidator @DepositScript
    ( $$(PlutusTx.compile [||validatorLogic||])
        `PlutusTx.applyCode` PlutusTx.liftCode cfg
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @ScriptContext @DepositDatum @DepositRedeemer

{-# INLINEABLE validatorLogic #-}
validatorLogic ::
  DepositConfig ->
  DepositDatum ->
  DepositRedeemer ->
  ScriptContext ->
  Bool
validatorLogic conf d r ctx = traceError "Not implemented"
