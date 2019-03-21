module Main
  ( main
  )
where

import Cardano.Prelude
import Cardano.Chain.Txp (UTxO (..))

import Prelude (error)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import GHC.DataSize

main :: IO ()
main = do
  let fname = "utxo.json"
  mbUtxoMap <- (\x -> UTxO <$> decode x) <$> BL.readFile fname
  case mbUtxoMap of
    Nothing -> error $ "couldn't json decode " <> fname
    Just (UTxO utxoMap) -> do
      print utxoMap
      size <- recursiveSizeNF utxoMap
      putStrLn $ "recursiveSizeNF says: " <> show size <> " bytes."
