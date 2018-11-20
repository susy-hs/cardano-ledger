{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds                  #-}
module Ledger where

import           Capability.State
import Control.Lens
import Control.State.Transition
import Control.State.Transition.Generator
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Ledger.Abstract
import Ledger.Simple (UTXO, utxoInductive)
import qualified Ledger.Simple
import UTxO
import           Numeric.Natural

-- | UTXO transition system
data UTXOW

instance STS UTXOW where
  type State UTXOW = UTxO
  type Signal UTXOW = TxWits
  type Environment UTXOW = Ledger.Simple.ProtocolConstants
  data PredicateFailure UTXOW
    = UtxoFailure [PredicateFailure UTXO]
    | InsufficientWitnesses
    deriving (Eq, Show)

  rules =
    [ Rule [] $ Base (UTxO Map.empty)
    , Rule
      [ SubTrans st1
      , Predicate $ \(pc, utxo, tw) -> witnessed tw utxo
      ]
      ( Extension . Transition $
        \jc -> subTransResult jc st1
      )
    ]
    where
      st1 = EmbeddedTransition (to $ \(env, st, sig) -> (env, st, body sig)) utxoInductive

instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> hash key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> UTxO -> PredicateResult UTXOW
witnessed (TxWits tx wits) utxo =
  if Set.size wits == Set.size ins && all (hasWitness wits) ins
    then Passed
    else Failed InsufficientWitnesses
 where
  ins = inputs tx
  hasWitness witnesses input =
    isJust $ find (isWitness tx input utxo) witnesses
  isWitness tx' input unspent (Wit key sig) =
    verify key tx' sig && authTxin key input unspent

----------------------------------------------------------------------------------------
-- Ledger generator
----------------------------------------------------------------------------------------

instance ProgressiveGen UTXOW where
  data GenState UTXOW = GenState
    { -- | Potential witnesses for transations.
      witnesses :: [Wit]
      -- | Addresses
    , addresses :: [KeyPair]
    }

sigGen :: GenM UTXOW TxWits
sigGen = undefined

-- | Generator for a natural number between 'lower' and 'upper'.
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

-- | Generate a transaction
genTx :: GenM UTXOW Tx
genTx = undefined

-- | Generator for List of 'Coin' values. Generates between 'lower' and 'upper'
-- coins, with values between 'minCoin' and 'maxCoin'.
genCoinList :: Natural -> Natural -> Int -> Int -> Gen [Value]
genCoinList minCoin maxCoin lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.exponential minCoin maxCoin)
  return (Value <$> xs)

-- | Generate transaction inputs
genTxIn :: GenM UTXOW (Set.Set TxIn)
genTxIn = do
  (UTxO m) <- get @"bkState"
  let utxoInputs = Map.keys m
      addr inp   = getTxOutAddr $ m Map.! inp

  -- select payer
  selectedInputs <- Gen.shuffle utxoInputs
  let !selectedAddr    = addr $ head selectedInputs
  let !selectedUTxO    = Map.filter (\(TxOut a _) -> a == selectedAddr) m
  let !selectedBalance = balance $ UTxO selectedUTxO

  -- select receipients, distribute balance of selected UTxO set
  addrs <- gets @"bkGenState" addresses
  n <- genNatural 1 10
  receipients <- take (fromIntegral n) <$> Gen.shuffle addrs
  let realN                = length receipients
  let perReceipient = selectedBalance / realN
  let !receipientAddrs      = fmap
          (\(p, d) -> AddrTxin (hashKey $ vKey p) (hashKey $ vKey d)) receipients
  let !txbody = Tx
           (Map.keysSet selectedUTxO)
           ((\r -> TxOut r perReceipient) <$> receipientAddrs)
           Set.empty
  let !txwit = makeWitness selectedKeyPair txbody
  pure $ TxWits txbody $ Set.fromList [txwit]

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Coin
-- value is generated.
genTxOut :: GenM UTXOW [TxOut]
genTxOut = do
  addrs <- gets @"bkGenState" addresses
  ys <- liftGenM $ genCoinList 1 100 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)


----------------------------------------------------------------------------------------
-- Ledger goblins
----------------------------------------------------------------------------------------
