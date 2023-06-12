module Options where

import Data.ByteString (ByteString)
import Options.Applicative
import Prelude

data PolicyOpts = PolicyOpts
  { tokenName :: String,
    transactionId :: ByteString,
    transactionIndex :: Integer,
    policyPath :: FilePath
  }
  deriving stock (Show, Eq)

policyParser :: Parser PolicyOpts
policyParser =
  PolicyOpts
    <$> strOption
      ( long "token-name"
          <> short 't'
          <> value "Mixer Protocol Token"
          <> metavar "TOKEN_NAME"
          <> help "Name of protocol thread token"
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
