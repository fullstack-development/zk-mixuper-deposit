{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.String (IsString (..))
import Mixer.Datum (MixerConfig (..))
import qualified Mixer.Script as MixerScript
import Options
  ( MixerOpts
      ( MixerOpts,
        currencySymbol,
        depositTreeTokenName,
        merkleTreeHeight,
        merkleTreeZeroLeaf,
        nullifierStoreTokenName,
        poolNominal,
        scriptPath,
        vaultTokenName
      ),
    mixerOpts,
  )
import Options.Applicative (execParser)
import Plutus.V1.Ledger.Bytes (fromHex)
import Plutus.V1.Ledger.ProtocolVersions (vasilPV)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), getLedgerBytes, toData)
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified PlutusCore as Plutus
import Service.MerkleTree (MerkleTreeConfig (..), calculateZeroRoot)
import Prelude

main :: IO ()
main = do
  MixerOpts {..} <- execParser mixerOpts
  ledgerCurrSymbol :: CurrencySymbol <- either (error . show) (pure . CurrencySymbol . getLedgerBytes) $ fromHex currencySymbol
  zeroLeaf <- either (error . show) (pure . getLedgerBytes) $ fromHex merkleTreeZeroLeaf
  let config =
        MixerConfig
          { protocolCurrency = ledgerCurrSymbol,
            depositTreeTokenName = fromString depositTreeTokenName,
            vaultTokenName = fromString vaultTokenName,
            nullifierStoreTokenName = fromString nullifierStoreTokenName,
            poolNominal = poolNominal,
            merkleTreeConfig =
              MerkleTreeConfig
                { zeroRoot = calculateZeroRoot merkleTreeHeight zeroLeaf,
                  zeroLeaf = zeroLeaf,
                  height = merkleTreeHeight
                }
          }
  costParams <- maybe (error "defaultCostModelParams failed") pure Plutus.defaultCostModelParams
  evalContext <- either (error . show) pure $ Plutus.mkEvaluationContext costParams
  let scriptParams = [toData config]
  let (logout, e) = Plutus.evaluateScriptCounting vasilPV Plutus.Verbose evalContext (MixerScript.scriptShortBs config) scriptParams
  putStrLn "Log output: " >> print logout
  case e of
    Left evalErr -> putStrLn "Eval Error: " >> print evalErr
    Right exbudget -> putStrLn "Ex Budget: " >> print exbudget
  result <- writeFileTextEnvelope scriptPath Nothing $ MixerScript.cardanoApiScript config
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> putStrLn $ "Script written to " <> scriptPath
