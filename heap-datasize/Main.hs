{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main
  ( main
  )
where

import           Cardano.Prelude
import           Cardano.Chain.Txp (UTxO (..))

import           Prelude (error)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import           GHC.DataSize

main :: IO ()
main = do
  printSizes ([] :: [Int64])
  printSizes ([0] :: [Int64])
  printSizes ([0,1] :: [Int64])
  printSizes ([0,1,2] :: [Int64])
  printSizes (1 :: Int64)
  printSizes ('c')
  printSizes ("str")
  printSizes (M.empty :: M.Map Int64 Int64)
  printSizes (M.fromList [(2,3)] :: M.Map Int64 Int64)
  printSizes ex0
  printSizes ex1
  printSizes ex2

printSizes :: (Show a, NFData a) => a -> IO ()
printSizes x = do
  putStrLn "\n----"
  print x
  putStrLn "----"
  print =<< (closureSize     $!! x)
  print =<< (recursiveSize   $!! x)
  print =<< (recursiveSizeNF $!! x)


data MyMap k a = Bin Int64 k a (MyMap k a) (MyMap k a)
               | Tip
               | Tip2
               deriving (Eq, Ord, Generic, Show)
               deriving anyclass NFData

ex0 = Tip :: MyMap Int64 Int64
ex1 = Bin 1 2 3 Tip Tip2 :: MyMap Int64 Int64
ex2 = Bin 1 2 3 Tip Tip :: MyMap Int64 Int64

  -- let fname = "utxo.json"
  -- mbUtxoMap <- (\x -> UTxO <$> decode x) <$> BL.readFile fname
  -- case mbUtxoMap of
  --   Nothing -> error $ "couldn't json decode " <> fname
  --   Just (UTxO utxoMap) -> do
  --     print utxoMap
  --     size <- recursiveSizeNF utxoMap
  --     putStrLn $ "recursiveSizeNF says: " <> show size <> " bytes."
