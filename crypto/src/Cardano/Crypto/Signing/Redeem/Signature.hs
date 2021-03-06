{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Crypto.Signing.Redeem.Signature
  ( RedeemSignature(..)
  , redeemSign
  , redeemSignRaw
  , verifyRedeemSig
  , verifyRedeemSigDecoded
  , verifyRedeemSigRaw
  )
where

import Cardano.Prelude

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Coerce (coerce)
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary
  (Annotated, Decoded(..), FromCBOR, Raw, ToCBOR, serialize')
import Cardano.Crypto.Orphans ()
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Redeem.PublicKey (RedeemPublicKey(..))
import Cardano.Crypto.Signing.Redeem.SecretKey (RedeemSecretKey(..))
import Cardano.Crypto.Signing.Tag (SignTag, signTag, signTagDecoded)


-- | Wrapper around 'Ed25519.Signature'
newtype RedeemSignature a =
  RedeemSignature Ed25519.Signature
  deriving (Eq, Ord, Show, Generic, NFData, FromCBOR, ToCBOR)

instance B.Buildable (RedeemSignature a) where
  build _ = "<redeem signature>"

deriveJSON defaultOptions ''RedeemSignature

-- | Encode something with 'ToCBOR' and sign it
redeemSign
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> RedeemSecretKey
  -> a
  -> RedeemSignature a
redeemSign pm tag k = coerce . redeemSignRaw pm (Just tag) k . serialize'

-- | Alias for constructor
redeemSignRaw
  :: ProtocolMagicId
  -> Maybe SignTag
  -> RedeemSecretKey
  -> ByteString
  -> RedeemSignature Raw
redeemSignRaw pm mbTag (RedeemSecretKey k) x =
  RedeemSignature $ Ed25519.sign k (Ed25519.toPublic k) $ tag <> x
  where tag = maybe mempty (signTag pm) mbTag

-- | Verify a redeem signature
verifyRedeemSig
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> RedeemPublicKey
  -> a
  -> RedeemSignature a
  -> Bool
verifyRedeemSig pm tag k x s =
  verifyRedeemSigRaw k (signTag pm tag <> serialize' x) (coerce s)

verifyRedeemSigDecoded
  :: Decoded t
  => Annotated ProtocolMagicId ByteString
  -> SignTag
  -> RedeemPublicKey
  -> t
  -> RedeemSignature (BaseType t)
  -> Bool
verifyRedeemSigDecoded pm tag k x s =
  verifyRedeemSigRaw k (signTagDecoded pm tag <> recoverBytes x) (coerce s)

-- | Verify raw 'ByteString'
verifyRedeemSigRaw
  :: RedeemPublicKey
  -> ByteString
  -> RedeemSignature Raw
  -> Bool
verifyRedeemSigRaw (RedeemPublicKey k) x (RedeemSignature s) =
  Ed25519.verify k x s
