{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.String (IsString (..))
import Ledger.Value (assetClass)
import Mixer.Datum (DepositConfig (..))
import qualified Mixer.Script as MixerScript
import Options
  ( DepositOpts
      ( DepositOpts,
        currencySymbol,
        merkleTreeHeight,
        merkleTreeZeroLeaf,
        poolNominal,
        scriptPath,
        tokenName
      ),
    depositOpts,
  )
import Options.Applicative (execParser)
import Plutus.V1.Ledger.Bytes (fromHex)
import Plutus.V1.Ledger.ProtocolVersions (vasilPV)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), TokenName, getLedgerBytes, toData)
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified PlutusCore as Plutus
import Service.MerkleTree (MerkleTreeConfig (..), calculateZeroRoot)
import Prelude

main :: IO ()
main = do
  DepositOpts {..} <- execParser depositOpts
  let ledgerTokenName :: TokenName = fromString tokenName
  ledgerCurrSymbol :: CurrencySymbol <- either (error . show) (pure . CurrencySymbol . getLedgerBytes) $ fromHex currencySymbol
  zeroLeaf <- either (error . show) (pure . getLedgerBytes) $ fromHex merkleTreeZeroLeaf
  let config =
        DepositConfig
          { protocolToken = assetClass ledgerCurrSymbol ledgerTokenName,
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
