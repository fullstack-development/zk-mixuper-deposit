module Mixer.Script where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Mixer.Datum (DepositConfig)
import Mixer.Script.Core (depositScript)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Prelude

depositValidator :: DepositConfig -> Scripts.Validator
depositValidator = Scripts.validatorScript . depositScript

scriptAsCbor :: DepositConfig -> LB.ByteString
scriptAsCbor = serialise . depositValidator

cardanoApiScript :: DepositConfig -> PlutusScript PlutusScriptV2
cardanoApiScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

scriptShortBs :: DepositConfig -> SBS.ShortByteString
scriptShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
