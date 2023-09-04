{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Codec.Serialise (serialise)
import Data.ByteString.Builder (byteStringHex, hPutBuilder)
import qualified Data.ByteString.Lazy as BSL
import Data.String (IsString (..))
import Mixer.Datum (DepositDatum (..), MixerConfig (..), MixerDatum (..))
import qualified Mixer.Script as MixerScript
import Options
  ( MixerOpts (..),
    mixerOpts,
  )
import Options.Applicative (execParser)
import Plutus.V1.Ledger.Bytes (fromHex)
import Plutus.V1.Ledger.ProtocolVersions (vasilPV)
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol), getLedgerBytes, toData)
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified PlutusCore as Plutus
import Service.MerkleTree
  ( Hash,
    MerkleTreeConfig (MerkleTreeConfig, height, zeroLeaf, zeroRoot),
    MerkleTreeState (MerkleTreeState, nextLeaf, tree),
    calculateZeroRoot,
    mkEmptyMT,
  )
import System.IO (IOMode (WriteMode), withFile)
import Prelude

main :: IO ()
main = do
  MixerOpts {..} <- execParser mixerOpts
  costParams <- maybe (error "defaultCostModelParams failed") pure Plutus.defaultCostModelParams
  evalContext <- either (error . show) pure $ Plutus.mkEvaluationContext costParams
  let scriptParams = [toData nonce]
  let (logout, e) = Plutus.evaluateScriptCounting vasilPV Plutus.Verbose evalContext (MixerScript.scriptShortBs nonce) scriptParams
  putStrLn "Log output: " >> print logout
  case e of
    Left evalErr -> putStrLn "Eval Error: " >> print evalErr
    Right exbudget -> putStrLn "Ex Budget: " >> print exbudget
  result <- writeFileTextEnvelope scriptPath Nothing $ MixerScript.cardanoApiScript nonce
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> putStrLn $ "Script written to " <> scriptPath

writeLazyByteStringToFile :: FilePath -> BSL.ByteString -> IO ()
writeLazyByteStringToFile filePath lbs =
  withFile filePath WriteMode $ \handle ->
    hPutBuilder handle (byteStringHex (BSL.toStrict lbs))
