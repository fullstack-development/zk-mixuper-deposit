module Mixer.Script where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Mixer.Datum (MixerConfig)
import Mixer.Script.Core (depositScript)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Prelude
import Plutonomy (optimizeUPLC)

-- validator size for merkle tree height 7: 6102 bytes
depositValidator :: MixerConfig -> Scripts.Validator
depositValidator = Scripts.validatorScript . depositScript

-- validator size for merkle tree height 7: 4809 bytes
depositValidatorOptimized :: MixerConfig -> Scripts.Validator
depositValidatorOptimized = optimizeUPLC . depositValidator

scriptAsCbor :: MixerConfig -> LB.ByteString
scriptAsCbor = serialise . depositValidatorOptimized

cardanoApiScript :: MixerConfig -> PlutusScript PlutusScriptV2
cardanoApiScript = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

scriptShortBs :: MixerConfig -> SBS.ShortByteString
scriptShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
