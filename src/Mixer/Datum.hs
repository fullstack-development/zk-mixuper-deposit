{-# LANGUAGE TemplateHaskell #-}

module Mixer.Datum where

import GHC.Generics (Generic)
import Ledger (AssetClass)
import qualified PlutusTx
import PlutusTx.Prelude
import Service.MerkleTree (Hash, MerkleTreeConfig, MerkleTreeState)
import qualified Prelude as Haskell

-- | A sha-256 digest of (nullifier <> secret)
type Commitment = Hash

type Lovelace = Integer

data DepositConfig = DepositConfig
  { protocolToken :: AssetClass,
    poolNominal :: Integer,
    merkleTreeConfig :: MerkleTreeConfig
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''DepositConfig

PlutusTx.makeIsDataIndexed
  ''DepositConfig
  [('DepositConfig, 0)]

newtype DepositRedeemer = DepositRedeemer
  { commitment :: Commitment
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''DepositRedeemer

PlutusTx.makeIsDataIndexed
  ''DepositRedeemer
  [('DepositRedeemer, 0)]

data DepositDatum = DepositDatum
  { merkleTreeState :: MerkleTreeState
  , merkleTreeRoot :: Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''DepositDatum

PlutusTx.makeIsDataIndexed
  ''DepositDatum
  [('DepositDatum, 0)]
