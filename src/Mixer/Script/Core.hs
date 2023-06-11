{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Mixer.Script.Core where

import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts (mkUntypedValidator)
import Ledger.Value (valueOf)
import Mixer.Datum (Commitment, DepositConfig (..), DepositDatum (..), DepositRedeemer (..))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api
  ( ScriptContext,
    TxInInfo (txInInfoResolved),
    TxOut (txOutValue),
    Value,
  )
import Plutus.V2.Ledger.Contexts (findOwnInput)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Service.MerkleTree as T
import Service.ProtocolToken (getNextState)
import Ext.PlutusTx.Builtins (byteString2Integer)

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
validatorLogic conf inputState r ctx = validateDeposit conf inputState outputState inputValue outputValue $ commitment r
  where
    (outputState :: DepositDatum, outputValue) = getNextState (protocolToken conf) ctx
    inputValue = case findOwnInput ctx of
      Just input -> txOutValue $ txInInfoResolved input
      Nothing -> traceError "Can't find own input"

{-# INLINEABLE validateDeposit #-}
validateDeposit :: DepositConfig -> DepositDatum -> DepositDatum -> Value -> Value -> Commitment -> Bool
validateDeposit conf inputState outputState inputValue outputValue commit =
  and
    [ traceIfFalse "Commitment has been submitted before" $ notElem commit $ T.nonEmptyLeafs $ T.tree currentTreeState,
      traceIfFalse "Nominal amount should be paid to script" $ poolNominal conf == depositedAdaAmount,
      traceIfFalse "Incorrect Merkle Tree state update" $ newTreeState == T.insert treeConf commit currentTreeState,
      traceIfFalse "Merkle Tree root is not correct" $ merkleTreeRoot outputState == rootTrunc
    ]
  where
    treeConf = merkleTreeConfig conf
    currentTreeState = merkleTreeState inputState
    newTreeState = merkleTreeState outputState
    rootTrunc = byteString2Integer 31 . takeByteString 31 <$> T.getRoot treeConf (T.tree newTreeState)
    depositedAdaAmount = valueOf outputValue Ada.adaSymbol Ada.adaToken - valueOf inputValue Ada.adaSymbol Ada.adaToken
