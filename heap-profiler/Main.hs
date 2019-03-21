module Main
  ( main
  )
where

import Cardano.Prelude
import Cardano.Chain.Txp (UTxO (..))

import Prelude (error)
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  let fname = "utxo.json"
  mbUtxoMap <- (\x -> UTxO <$> decode x) <$> BL.readFile fname
  case mbUtxoMap of
    Nothing -> error $ "couldn't json decode " <> fname
    Just (UTxO utxoMap) -> print utxoMap >> threadDelay 1000000
