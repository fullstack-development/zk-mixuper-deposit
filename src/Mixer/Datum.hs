{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Mixer.Datum where

import GHC.Generics (Generic)
import Ledger (CurrencySymbol)
import Ledger.Value (TokenName)
import qualified PlutusTx
import PlutusTx.Prelude
import Service.MerkleTree (Hash, MerkleTreeConfig, MerkleTreeState)
import qualified Prelude as Haskell

-- | A sha-256 digest of (nullifier <> secret)
type Commitment = Hash

type Lovelace = Integer

type MixerConfig = Integer

type MixerRedeemer = Integer

type MixerDatum = ()
