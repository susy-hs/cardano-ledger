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
import           Data.Tree
import           GHC.DataSize

main :: IO ()
main = do
  printTree ('c')
  printTree ("str")
  printTree (M.empty :: M.Map Int64 Int64)
  printTree (M.fromList [(2,3)] :: M.Map Int64 Int64)
  printTree (M.singleton 2 3 :: M.Map Int64 Int64)
  -- printTree ex0
  -- printTree ex1
  -- printTree ex2
  -- printTree A
  -- printTree (B 1)
  -- printTree (C 2)
  -- printTree X
  -- printTree Y
  -- printTree ([] :: [Int64])
  -- printTree ([0] :: [Int64])
  -- printTree ([0,1] :: [Int64])
  -- printTree ([0,1,2] :: [Int64])
  -- printTree (1 :: Int64)

printTree :: (Show a, NFData a) => a -> IO ()
printTree x = do
  putStrLn "\n----"
  printSizes x
  putStrLn ""
  putStrLn =<< (renderTree $!! x)
  putStrLn "----"

printSizes :: (Show a, NFData a) => a -> IO ()
printSizes x = do
  print x
  print =<< (closureSize     $!! x)
  print =<< (recursiveSize   $!! x)
  print =<< (recursiveSizeNF $!! x)

data Foo = A | B Int | C {-# UNPACK #-} !Int
         deriving (Eq, Ord, Generic, Show)
         deriving anyclass NFData

data Bar = X | Y
         deriving (Eq, Ord, Generic, Show)
         deriving anyclass NFData

data MyMap k a = Bin Size k a (MyMap k a) (MyMap k a)
               | Tip
               | Tip2
               deriving (Eq, Ord, Generic, Show)
               deriving anyclass NFData

type Size = Int

ex0 = Tip :: MyMap Int64 Int64
ex1 = Bin 4 2 3 Tip Tip2 :: MyMap Int64 Int64
ex2 = Bin 1 2 3 Tip Tip :: MyMap Int64 Int64

  -- let fname = "utxo.json"
  -- mbUtxoMap <- (\x -> UTxO <$> decode x) <$> BL.readFile fname
  -- case mbUtxoMap of
  --   Nothing -> error $ "couldn't json decode " <> fname
  --   Just (UTxO utxoMap) -> do
  --     print utxoMap
  --     size <- recursiveSizeNF utxoMap
  --     putStrLn $ "recursiveSizeNF says: " <> show size <> " bytes."
