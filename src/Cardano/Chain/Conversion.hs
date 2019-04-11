{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Conversion
  ( convertConfig
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.EitherR (fmapLT)
import Data.Time (UTCTime)

import Cardano.Chain.Common (parseReqNetworkMag)
import Cardano.Chain.Genesis.Config (Config, ConfigurationError, mkConfigFromFile)--, mkConfigFromStaticConfig)
import Cardano.Shell.Constants.Types as Shell

-- | This module converts `CardanoConfiguration` to `Genesis.Config`

data ConversionError = GenesisConversionError ConfigurationError

data Switch
  = FromSrc
  -- ^ Convert configuration based on `mainnet-genesis.json`
  | FromSpec (Maybe UTCTime) (Maybe Integer)
  -- ^ Convert configuration based on 'geSpec'/`GCSpec`
  --   If we provide a seed ('Maybe Integer') this will override
  --   the seed and we will also need to provide a
  --   start time (Maybe UTCTime)
  -- TODO: Check if this logic above it correct^^


-- | Converts 'CardanoConfiguration' (from 'cardano-shell') into a `Config` (from 'cardano-ledger')
convertConfig :: Switch -> Shell.CardanoConfiguration -> ExceptT ConversionError IO Config
convertConfig swtch cc =
    case swtch of
      FromSrc -> do
                   let mainnetGenFp = geSrc . coGenesis $ ccCore cc
                   -- TODO: Get genesis hash from cc
                   -- let genHash = geFileHash cc
                   -- TODO: Create better parser
                   -- TODO: Shouldn't we have an unsafe mkConfig from file
                   -- that ignores the hash of the file?
                   config <- runExceptT . fmapLT GenesisConversionError $ mkConfigFromFile reqNetworkMagic mainnetGenFp Nothing
                   except config
      FromSpec _ _ -> panic "Placeholder"
      --       mSystemStart mSeed_
        --TODO: Left off here. You need to create a 'GSpec' value from 'CardanoConfiguration'
        --mkConfigFromStaticConfig reqNetworkMagic mSystemStart mSeed (GCSpec genSpec)
  where
    reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc

