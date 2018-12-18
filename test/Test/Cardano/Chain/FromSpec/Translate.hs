{-# LANGUAGE PackageImports #-}
module Test.Cardano.Chain.FromSpec.Translate
  ()
  where

import qualified Cardano.Chain.Block as Real
import qualified "cs-blockchain" Types as Spec
import Cardano.Prelude

type TranslateM = IO

translateBlock
  :: Spec.Block
  -> TranslateM Real.Block
translateBlock = undefined
