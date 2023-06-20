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

-- Allowed transitions:
{-# INLINEABLE validatorLogic #-}
validatorLogic ::
  MixerConfig ->
  MixerDatum ->
  MixerRedeemer ->
  ScriptContext ->
  Bool
-- Main (merkle tree) deposit validator:
validatorLogic conf (DepositTree inputState) (Deposit commit) ctx =
  traceIfFalse "Deposit tree output value changed" (treeOutputValue == treeInputValue)
    && traceIfFalse "Vault output datum changed" isVault
    && validateDeposit conf inputState outputState inputValue outputValue commit
  where
    msg = "Incorrect next state"
    !info = scriptContextTxInfo ctx
    !outputs = txInfoOutputs info
    !protocolCurr = protocolCurrency conf
    !(nextStateDatum :: MixerDatum, !treeOutputValue) = getNextState protocolCurr (depositTreeTokenName conf) outputs
    !outputState = case nextStateDatum of
      DepositTree dd -> dd
      Vault -> traceError msg
    !treeInputValue = case findOwnInput ctx of
      Just input -> txOutValue $ txInInfoResolved input
      Nothing -> traceError "Can't find own input"
    !vaultInput = case uniqueElement $ filterInputsByToken protocolCurr (vaultTokenName conf) (txInfoInputs info) of
      Just i -> i
      Nothing -> traceError "Can't find vault input"
    inputValue = txOutValue . txInInfoResolved $ vaultInput
    !(vaultStateDatum :: MixerDatum, outputValue) = getNextState protocolCurr (vaultTokenName conf) outputs
    !isVault = case vaultStateDatum of
      DepositTree _ -> False
      Vault -> True
-- Vault deposit validator:
validatorLogic conf Vault Topup ctx = traceIfFalse "Does not spend deposit tree UTxO" hasTreeInput
  where
    !protocolCurr = protocolCurrency conf
    !inputs = txInfoInputs $ scriptContextTxInfo ctx
    !hasTreeInput = isJust $ uniqueElement $ filterInputsByToken protocolCurr (depositTreeTokenName conf) inputs
-- Vault withdraw validator:
validatorLogic conf Vault Withdraw ctx = traceIfFalse "Does not spend nullifier store UTxO" hasStoreInput
  where
    !protocolCurr = protocolCurrency conf
    !inputs = txInfoInputs $ scriptContextTxInfo ctx
    !hasStoreInput = isJust $ uniqueElement $ filterInputsByToken protocolCurr (nullifierStoreTokenName conf) inputs
-- Disallowed transitions:
validatorLogic _ _ _ _ = traceError "Disallowed transition"

{-# INLINEABLE validateDeposit #-}
validateDeposit :: MixerConfig -> DepositDatum -> DepositDatum -> Value -> Value -> Commitment -> Bool
validateDeposit conf !inputState !outputState !inputValue !outputValue !commit =
  traceIfFalse "Commitment has been submitted before" isFreshCommit
    && traceIfFalse "Nominal amount should be paid to script" (poolNominal conf == depositedAdaAmount)
    && traceIfFalse "Incorrect Merkle Tree state update" (newTreeState == treeInsert)
    && traceIfFalse "Merkle Tree root is not correct" (merkleTreeRoot outputState == rootTrunc)
  where
    !treeConf = merkleTreeConfig conf
    !currentTreeState = merkleTreeState inputState
    !newTreeState = merkleTreeState outputState
    !rootTrunc = byteString2Integer 31 . takeByteString 31 <$> T.getRoot treeConf (T.tree newTreeState)
    !depositedAdaAmount = valueOf outputValue Ada.adaSymbol Ada.adaToken - valueOf inputValue Ada.adaSymbol Ada.adaToken
    !treeInsert = T.insert treeConf commit currentTreeState

    !isFreshCommit = notElem commit $ T.nonEmptyLeafs $ T.tree currentTreeState
