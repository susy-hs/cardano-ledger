module Main
  ( main
  )
where

import Cardano.Prelude

import qualified Hedgehog.Gen as Gen

import Cardano.Chain.Txp (UTxO (..))

import Test.Cardano.Chain.Txp.Gen (genUTxO)

main :: IO ()
main = do
  UTxO utxo <- Gen.sample genUTxO
  print utxo
