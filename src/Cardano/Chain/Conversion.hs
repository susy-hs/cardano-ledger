{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Conversion
  ( convertConfig
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Except (ExceptT, except, runExceptT)

import Cardano.Chain.Common (parseReqNetworkMag)
import Cardano.Chain.Genesis.Config as Ledger
import Cardano.Shell.Constants.Types as Shell

-- | This module converts `CardanoConfiguration` to `Genesis.Config`

--data ConfigConversionError = NullError

convertConfig :: Shell.CardanoConfiguration -> ExceptT Ledger.ConfigurationError IO Ledger.Config
convertConfig cc = do
  let mainnetGenFp = geSrc . coGenesis $ ccCore cc
  let reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc
  config <- runExceptT $ mkConfigFromFile reqNetworkMagic mainnetGenFp Nothing
  except config
