{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Codec.Serialise (deserialise, serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
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
import Data.Maybe (fromJust)
import qualified Service.MerkleTree as T

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
  writeDepositTreeDatum datumPath merkleTreeHeight zeroLeaf
  putStrLn $ "Datum written to " <> datumPath

writeDepositTreeDatum :: FilePath -> Integer -> Hash -> IO ()
writeDepositTreeDatum path height zeroLeaf = do
  let depositTree =
        DepositTree $
          DepositDatum
            { merkleTreeState =
                MerkleTreeState
                  { nextLeaf = 0,
                    tree = mkEmptyMT height zeroLeaf
                  },
              merkleTreeRoot = Nothing
            }
  let serializedMT = serialise $ toData depositTree
  writeLazyByteStringToFile path serializedMT
  pure ()

writeLazyByteStringToFile :: FilePath -> BSL.ByteString -> IO ()
writeLazyByteStringToFile filePath lbs =
  withFile filePath WriteMode $ \handle ->
    hPutBuilder handle (byteStringHex (BSL.toStrict lbs))

readLazyByteStringHexFromFile :: FilePath -> IO BSL.ByteString
readLazyByteStringHexFromFile filePath = do
  hexString <- BS.readFile filePath
  bytes <- either error pure $ Hex.decode hexString
  pure $ BSL.fromStrict bytes

deser :: BSL.ByteString -> Plutus.Data
deser = deserialise

mkTreeConfig :: Integer -> BS.ByteString -> MerkleTreeConfig
mkTreeConfig merkleTreeHeight merkleTreeZeroLeaf =
  let zeroLeaf = either error getLedgerBytes $ fromHex merkleTreeZeroLeaf
  in
    MerkleTreeConfig
      { zeroRoot = calculateZeroRoot merkleTreeHeight zeroLeaf,
        zeroLeaf = zeroLeaf,
        height = merkleTreeHeight
      }

test01 :: IO Bool
test01 = do
  bs0 <- readLazyByteStringHexFromFile "compiled/depositTree.datum"
  bs1 <- readLazyByteStringHexFromFile "compiled/datumOut.cbor"
  let commit = either error getLedgerBytes $ fromHex "a6412338645d14a7782c4ef186ae0deae1d3efb6c140b62bb8a5f1238cdcd93f"
  let conf = mkTreeConfig 7 "cd4ecd8b80466c7325e9d2f76fce6eb8a236667734eb1646bcfdcb51"
  let d0 = deser bs0
  let d1 = deser bs1
  let DepositTree md0 = fromJust $ Plutus.fromData @MixerDatum d0
  let DepositTree md1 = fromJust $ Plutus.fromData @MixerDatum d1
  let mts0 = merkleTreeState md0
  let mts1 = merkleTreeState md1
  let ns = T.insert conf commit mts0
  pure $ ns == mts1
