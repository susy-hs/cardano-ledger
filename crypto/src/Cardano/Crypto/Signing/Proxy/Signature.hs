{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Crypto.Signing.Proxy.Signature
  ( AProxySignature(..)
  , ProxySignature
  , validateProxySignature
  , proxySign
  , proxyVerify
  , proxyVerifyDecoded
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import Data.ByteArray (ScrubbedBytes)
import Formatting (bprint, build, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated
  , ByteSpan
  , Decoded(..)
  , FromCBOR(..)
  , ToCBOR(..)
  , encodeListLen
  , enforceSize
  , serialize'
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagicId)
import Cardano.Crypto.Signing.Proxy.VerificationKey
  ( AProxyVerificationKey(..)
  , ProxyVerificationKey
  , pskOmega
  , validateProxyVerificationKey
  )
import Cardano.Crypto.Signing.PublicKey (PublicKey(..))
import Cardano.Crypto.Signing.SecretKey (SecretKey(..), toPublic)
import Cardano.Crypto.Signing.Signature (fromCBORXSignature, toCBORXSignature)
import Cardano.Crypto.Signing.Tag (SignTag, signTag, signTagDecoded)


-- | Delegate signature made with certificate-based permission. @w@ stays for
--   message type used in proxy (ω in the implementation notes), @a@ for type of
--   message signed.
--
--   We add whole psk as a field because otherwise we can't verify sig in
--   heavyweight psk transitive delegation: i → x → d, we have psk from x to d,
--   slot leader is i.
type ProxySignature w s = AProxySignature w s ()

data AProxySignature w s a = AProxySignature
  { psigPsk :: AProxyVerificationKey w a
  , psigSig :: CC.XSignature
  } deriving (Eq, Ord, Show, Generic, Functor)
    deriving anyclass NFData


instance B.Buildable w => B.Buildable (AProxySignature w s a) where
  build psig = bprint ("Proxy signature { psk = " . build . " }") (psigPsk psig)

instance (Typeable s, ToCBOR w) => ToCBOR (ProxySignature w s) where
  toCBOR psig =
    encodeListLen 2 <> toCBOR (psigPsk psig) <> toCBORXSignature (psigSig psig)

instance (Typeable s, FromCBOR w) => FromCBOR (ProxySignature w s) where
  fromCBOR = void <$> fromCBOR @(AProxySignature w s ByteSpan)

instance (Typeable s, FromCBOR w) => FromCBOR (AProxySignature w s ByteSpan) where
  fromCBOR =
    AProxySignature
      <$  enforceSize "ProxySignature" 2
      <*> fromCBOR
      <*> fromCBORXSignature

validateProxySignature
  :: MonadError Text m
  => Annotated ProtocolMagicId ByteString
  -> AProxySignature w a ByteString
  -> m ()
validateProxySignature pm psig = validateProxyVerificationKey pm (psigPsk psig)


-- | Make a proxy delegate signature with help of certificate. If the delegate
--   secret key passed doesn't pair with delegate public key in certificate
--   inside, we panic. Please check this condition outside of this function.
proxySign
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> SecretKey
  -> ProxyVerificationKey w
  -> a
  -> ProxySignature w a
proxySign pm t sk@(SecretKey delegateSk) psk m
  | toPublic sk /= pskDelegatePk psk = panic $ sformat
    ( "proxySign called with irrelevant certificate "
    . "(psk delegatePk: "
    . build
    . ", real delegate pk: "
    . build
    . ")"
    )
    (pskDelegatePk psk)
    (toPublic sk)
  | otherwise = AProxySignature {psigPsk = psk, psigSig = sigma}
 where
  PublicKey issuerPk = pskIssuerPk psk
  -- It's safe to put the tag after issuerPk because `CC.unXPub issuerPk` always
  -- takes 64 bytes
  sigma = CC.sign (mempty :: ScrubbedBytes) delegateSk
    $ mconcat ["01", CC.unXPub issuerPk, signTag pm t, serialize' m]

-- | Verify delegated signature given issuer's pk, signature, message
--   space predicate and message itself.
proxyVerifyDecoded
  :: Decoded t
  => Annotated ProtocolMagicId ByteString
  -> SignTag
  -> (w -> Bool)
  -> t
  -> ProxySignature w (BaseType t)
  -> Bool
proxyVerifyDecoded pm t omegaPred m psig = predCorrect && sigValid
 where
  psk         = psigPsk psig
  PublicKey issuerPk        = pskIssuerPk psk
  PublicKey pdDelegatePkRaw = pskDelegatePk psk
  predCorrect = omegaPred (pskOmega psk)
  sigValid    = CC.verify
    pdDelegatePkRaw
    (mconcat ["01", CC.unXPub issuerPk, signTagDecoded pm t, recoverBytes m])
    (psigSig psig)

-- | Verify delegated signature given issuer's pk, signature, message space
--   predicate and message
proxyVerify
  :: ToCBOR a
  => ProtocolMagicId
  -> SignTag
  -> (w -> Bool)
  -> a
  -> ProxySignature w a
  -> Bool
proxyVerify pm t omegaPred m psig = predCorrect && sigValid
 where
  psk         = psigPsk psig
  PublicKey issuerPk        = pskIssuerPk psk
  PublicKey pdDelegatePkRaw = pskDelegatePk psk
  predCorrect = omegaPred (pskOmega psk)
  sigValid    = CC.verify
    pdDelegatePkRaw
    (mconcat ["01", CC.unXPub issuerPk, signTag pm t, serialize' m])
    (psigSig psig)
