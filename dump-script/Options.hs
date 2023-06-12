module Options where

import Data.ByteString (ByteString)
import Options.Applicative
import Prelude

data DepositOpts = DepositOpts
  { currencySymbol :: ByteString,
    tokenName :: String,
    poolNominal :: Integer,
    merkleTreeHeight :: Integer,
    merkleTreeZeroLeaf :: ByteString,
    scriptPath :: FilePath
  }
  deriving stock (Show, Eq)

depositParser :: Parser DepositOpts
depositParser =
  DepositOpts
    <$> strOption
      ( long "currency-symbol"
          <> short 's'
          <> metavar "CURRENCY_SYMBOL"
          <> help "Currency symbol of protocol thread token"
      )
    <*> strOption
      ( long "token-name"
          <> short 't'
          <> value "Mixer Protocol Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of protocol thread token"
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
      ( long "path"
          <> help "Path where the script is written"
          <> showDefault
          <> value "compiled/depositScript.plutus"
          <> metavar "SCRIPT_PATH"
      )

depositOpts :: ParserInfo DepositOpts
depositOpts =
  info
    (depositParser <**> helper)
    ( fullDesc
        <> progDesc "Dump script byte code to a file"
        <> header "dump-script - write plutus script to a file"
    )
