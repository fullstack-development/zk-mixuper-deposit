module Options where

import Data.ByteString (ByteString)
import Options.Applicative
import Prelude

data PolicyOpts = PolicyOpts
  { depositTreeTokenName :: String,
    vaultTokenName :: String,
    nullifierStoreTokenName :: String,
    transactionId :: ByteString,
    transactionIndex :: Integer,
    policyPath :: FilePath
  }
  deriving stock (Show, Eq)

policyParser :: Parser PolicyOpts
policyParser =
  PolicyOpts
    <$> strOption
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
    <*> strOption
      ( long "tx-id"
          <> short 'h'
          <> metavar "TX_ID"
          <> help "Unique transaction id"
      )
    <*> option
      auto
      ( long "tx-index"
          <> short 'x'
          <> help "Index of transaction output"
          <> metavar "INT"
      )
    <*> strOption
      ( long "path"
          <> help "Path where the policy is written"
          <> showDefault
          <> value "compiled/mintingPolicy.plutus"
          <> metavar "SCRIPT_PATH"
      )

policyOpts :: ParserInfo PolicyOpts
policyOpts =
  info
    (policyParser <**> helper)
    ( fullDesc
        <> progDesc "Dump policy byte code to a file"
        <> header "dump-policy - write plutus policy to a file"
    )
