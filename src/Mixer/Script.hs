{-# LANGUAGE TemplateHaskell #-}

module Mixer.Script where

import Mixer.Datum
import Mixer.Script.Core
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Prelude

depositValidator :: DepositConfig -> Scripts.Validator
depositValidator = Scripts.validatorScript . depositScript
