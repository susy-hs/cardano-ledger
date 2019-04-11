module Main
  ( main
  )
where

import Cardano.Prelude
import Cardano.Chain.Txp (UTxO (..))

import Prelude (error)
import Control.Concurrent (threadDelay)
import Data.Aeson
import System.Exit (exitFailure)
import Text.Read (readEither)

main :: IO ()
main = do
  pure ()
  -- let fname = "utxo.txt"
  -- eutxo <- readEither . show <$> readFile fname
  -- case eutxo of
  --   Left err -> print err >> exitFailure
  --   Right (UTxO utxoMap) -> print utxoMap >> threadDelay 1000000
