module Options where

import Data.ByteString (ByteString)
import Options.Applicative
import Prelude

data MixerOpts = MixerOpts
  { nonce :: Integer,
    scriptPath :: FilePath
  }
  deriving stock (Show, Eq)

mixerParser :: Parser MixerOpts
mixerParser =
  MixerOpts
    <$> option
      auto
      ( long "nonce"
          <> short 'n'
          <> value 454897
          <> help "Dummy script parameter to make it unique"
          <> metavar "INT"
      )
    <*> strOption
      ( long "script-path"
          <> help "Path where the script is written"
          <> showDefault
          <> value "compiled/depositScript.plutus"
          <> metavar "SCRIPT_PATH"
      )

mixerOpts :: ParserInfo MixerOpts
mixerOpts =
  info
    (mixerParser <**> helper)
    ( fullDesc
        <> progDesc "Dump script byte code to a file"
        <> header "dump-script - write plutus script to a file"
    )
