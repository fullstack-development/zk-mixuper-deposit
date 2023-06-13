{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Data.String (IsString (..))
import Ledger (TxId (..), TxOutRef (..))
import Options
  ( PolicyOpts (..),
    policyOpts,
  )
import Options.Applicative (execParser)
import Plutus.V1.Ledger.Api (TokenName, getLedgerBytes, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Bytes (fromHex)
import Plutus.V1.Ledger.ProtocolVersions (vasilPV)
import qualified PlutusCore as Plutus
import qualified Service.ProtocolToken as ProtocolToken
import Prelude

main :: IO ()
main = do
  PolicyOpts {..} <- execParser policyOpts
  let tokens :: [TokenName] = fromString <$> [depositTreeTokenName, vaultTokenName, nullifierStoreTokenName]
  ledgerTxId :: TxId <- either (error . show) (pure . TxId . getLedgerBytes) $ fromHex transactionId
  let txRef = TxOutRef ledgerTxId transactionIndex
  let currency = ProtocolToken.mkCurrency txRef tokens
  costParams <- maybe (error "defaultCostModelParams failed") pure Plutus.defaultCostModelParams
  evalContext <- either (error . show) pure $ Plutus.mkEvaluationContext costParams
  let scriptParams = [toData (txRef, tokens)]
  let (logout, e) = Plutus.evaluateScriptCounting vasilPV Plutus.Verbose evalContext (ProtocolToken.scriptShortBs currency) scriptParams
  putStrLn "Log output: " >> print logout
  case e of
    Left evalErr -> putStrLn "Eval Error: " >> print evalErr
    Right exbudget -> putStrLn "Ex Budget: " >> print exbudget
  result <- writeFileTextEnvelope policyPath Nothing $ ProtocolToken.cardanoApiMintingScript currency
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> putStrLn $ "Script written to " <> policyPath
