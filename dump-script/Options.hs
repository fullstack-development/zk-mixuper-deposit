module Options where

import Data.ByteString (ByteString)
import Options.Applicative
import Prelude

data MixerOpts = MixerOpts
  { currencySymbol :: ByteString,
    depositTreeTokenName :: String,
    vaultTokenName :: String,
    nullifierStoreTokenName :: String,
    poolNominal :: Integer,
    merkleTreeHeight :: Integer,
    merkleTreeZeroLeaf :: ByteString,
    scriptPath :: FilePath,
    datumPath :: FilePath
  }
  deriving stock (Show, Eq)

mixerParser :: Parser MixerOpts
mixerParser =
  MixerOpts
    <$> strOption
      ( long "currency-symbol"
          <> short 's'
          <> metavar "CURRENCY_SYMBOL"
          <> help "Currency symbol of protocol thread token"
      )
    <*> strOption
      ( long "tree-token-name"
          <> value "Deposit Tree Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of deposit tree protocol thread token"
      )
    <*> strOption
      ( long "vault-token-name"
          <> value "Vault Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of vault protocol thread token"
      )
    <*> strOption
      ( long "store-token-name"
          <> value "Nullifier Store Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of nullifier store protocol thread token"
      )
    <*> option
      auto
      ( long "pool-nominal"
          <> short 'n'
          <> value 100_000_000
          <> help "Nominal value of the mixer pool in lovelace"
          <> metavar "INT"
      )
    <*> option
      auto
      ( long "merkle-tree-height"
          <> short 'h'
          <> help "Merkle tree height, which affects how many commitments are allowed"
          <> metavar "INT"
      )
    <*> strOption
      ( long "merkle-tree-zero-leaf"
          <> short 'z'
          <> value "6e045b8f5eaa4bdc8f8a44797255d03f4e2aac366e32859c5d07cd8de46c2ea3"
          <> metavar "MERKLE_TREE_ZERO_LEAF"
          <> help "Commitment which is considered empty, default is echo -n \"tornado.cash on cardano\" | sha256sum"
      )
    <*> strOption
      ( long "script-path"
          <> help "Path where the script is written"
          <> showDefault
          <> value "compiled/depositScript.plutus"
          <> metavar "SCRIPT_PATH"
      )
    <*> strOption
      ( long "datum-path"
          <> help "Path where the deposit tree datum is written"
          <> showDefault
          <> value "compiled/depositTree.datum"
          <> metavar "DATUM_PATH"
      )

mixerOpts :: ParserInfo MixerOpts
mixerOpts =
  info
    (mixerParser <**> helper)
    ( fullDesc
        <> progDesc "Dump script byte code to a file"
        <> header "dump-script - write plutus script to a file"
    )
