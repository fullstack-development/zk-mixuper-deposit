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

data MixerConfig = MixerConfig
  { protocolCurrency :: CurrencySymbol,
    depositTreeTokenName :: TokenName,
    vaultTokenName :: TokenName,
    nullifierStoreTokenName :: TokenName,
    poolNominal :: Integer,
    merkleTreeConfig :: MerkleTreeConfig
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MixerConfig

PlutusTx.makeIsDataIndexed
  ''MixerConfig
  [('MixerConfig, 0)]

data MixerRedeemer
  = Deposit Commitment
  | Topup
  | Withdraw
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MixerRedeemer

PlutusTx.makeIsDataIndexed
  ''MixerRedeemer
  [('Deposit, 0), ('Topup, 1), ('Withdraw, 2)]

data DepositDatum = DepositDatum
  { merkleTreeState :: MerkleTreeState,
    merkleTreeRoot :: Maybe Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''DepositDatum

PlutusTx.makeIsDataIndexed
  ''DepositDatum
  [('DepositDatum, 0)]

data MixerDatum
  = DepositTree DepositDatum
  | Vault
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MixerDatum

PlutusTx.makeIsDataIndexed
  ''MixerDatum
  [('DepositTree, 0), ('Vault, 1)]
