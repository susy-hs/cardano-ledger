{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Hierarchical derivation interface
--
--   This module provides basic operations with HD wallets.
--   You can read HD wallets overall description in docs/hd.md.

module Cardano.Crypto.HD
  ( HDPassphrase(..)
  , HDAddressPayload(..)
  , ShouldCheckPassphrase(..)
  , packHDAddressAttr
  , unpackHDAddressAttr
  , deriveHDVerificationKey
  , deriveHDSigningKey
  , deriveHDPassphrase
  , decryptChaChaPoly
  , encryptChaChaPoly
  , toEither
  , firstHardened
  , firstNonHardened
  , isHardened
  )
where

import Cardano.Prelude

import Cardano.Crypto.Wallet
  (DerivationScheme(..), deriveXPrv, deriveXPub, unXPub)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import Crypto.Error
import Crypto.Hash (SHA512(..))
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.MAC.Poly1305 as Poly
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.ByteArray as BA (convert)
import Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import Data.ByteString.Char8 as B

import Cardano.Binary
  (FromCBOR(..), ToCBOR(..), decodeBytesCanonical, decodeFull', serialize')
import Cardano.Crypto.Signing (VerificationKey(..))
import Cardano.Crypto.Signing.Safe
  (EncryptedSigningKey(..), PassPhrase, checkPassMatches)


-- | Passphrase is a hash of root verification key.
data HDPassphrase =
  HDPassphrase !ByteString
  deriving (Eq, Show)

-- | HDAddressPayload consists of
--
--   * serialiazed and encrypted using HDPassphrase derivation path from the
--   root key to given descendant key (using ChaChaPoly1305 algorithm)
--
--   * cryptographic tag
--
--   For more information see 'packHDAddressAttr' and 'encryptChaChaPoly'.
newtype HDAddressPayload = HDAddressPayload
  { getHDAddressPayload :: ByteString
  } deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToCBOR, HeapWords)
    deriving anyclass NFData

instance FromCBOR HDAddressPayload where
  fromCBOR = HDAddressPayload <$> decodeBytesCanonical

instance FromJSON HDAddressPayload where
  parseJSON v = HDAddressPayload . getByteString64 <$> parseJSON v

instance ToJSON HDAddressPayload where
  toJSON = toJSON . makeByteString64 . getHDAddressPayload

-- | Compute passphrase as hash of the root verification key
deriveHDPassphrase :: VerificationKey -> HDPassphrase
deriveHDPassphrase (VerificationKey vk) = HDPassphrase $ PBKDF2.generate
  (PBKDF2.prfHMAC SHA512)
  -- Parameters for the hashing function. 500 iter of PBDKF2 with HMAC-SHA256
  (PBKDF2.Parameters 500 passLen)
  (unXPub vk)
  ("address-hashing" :: ByteString)
 where
    -- Password length in bytes
  passLen :: Int
  passLen = 32

-- | Direct children of node are numbered from 0 to 2^32-1.
--   Indices less than @firstHardened@ are non-hardened indices.
firstHardened :: Word32
firstHardened = 2 ^ (31 :: Word32)

firstNonHardened :: Word32
firstNonHardened = 0

isHardened :: Word32 -> Bool
isHardened = (>= firstHardened)

-- | Derive verification key from verification key in non-hardened (normal) way.
--   If you try to pass an 'isHardened' index, error will be called.
deriveHDVerificationKey :: VerificationKey -> Word32 -> VerificationKey
deriveHDVerificationKey (VerificationKey xpub) childIndex
  | isHardened childIndex
  = panic "Wrong index for non-hardened derivation"
  | otherwise
  = maybe (panic "deriveHDVerificationKey: deriveXPub failed") VerificationKey
    $ deriveXPub DerivationScheme1 xpub childIndex

-- | Whether to call 'checkPassMatches'
newtype ShouldCheckPassphrase = ShouldCheckPassphrase Bool

-- | Derive child's signing key from parent's signing key using user's passphrase
deriveHDSigningKey
  :: ShouldCheckPassphrase
  -> PassPhrase
  -> EncryptedSigningKey
  -> Word32
  -> Maybe EncryptedSigningKey
deriveHDSigningKey (ShouldCheckPassphrase checkPass) passPhrase encSK@(EncryptedSigningKey xprv pph) childIndex
  = do
    when checkPass $ checkPassMatches passPhrase encSK
    pure $ EncryptedSigningKey
      (deriveXPrv DerivationScheme1 passPhrase xprv childIndex)
      pph

addrAttrNonce :: ByteString
addrAttrNonce = "serokellfore"

-- | Serialize tree path and encrypt it using HDPassphrase via ChaChaPoly1305.
packHDAddressAttr :: HDPassphrase -> [Word32] -> HDAddressPayload
packHDAddressAttr (HDPassphrase passphrase) path = do
  let !pathSer = serialize' path
  let !packCF = encryptChaChaPoly addrAttrNonce passphrase "" pathSer
  case packCF of
    CryptoFailed er -> panic $ "Error in packHDAddressAttr: " <> show er
    CryptoPassed p  -> HDAddressPayload p

-- | Try to decrypt 'HDAddressPayload' using 'HDPassphrase'
unpackHDAddressAttr :: HDPassphrase -> HDAddressPayload -> Maybe [Word32]
unpackHDAddressAttr (HDPassphrase passphrase) (HDAddressPayload payload) = do
  let !unpackCF = decryptChaChaPoly addrAttrNonce passphrase "" payload
  case unpackCF of
    Left  _ -> Nothing
    Right p -> case decodeFull' p of
      Left  _    -> Nothing
      Right path -> pure path

-- | Take HDPassphrase as symmetric key and serialized derivation path
--   and encrypt it using ChaChaPoly1305 scheme
encryptChaChaPoly
  :: ByteString
  -- ^ Nonce (12 random bytes)
  -> ByteString
  -- ^ Symmetric key (must be 32 bytes)
  -> ByteString
  -- ^ Encryption header
  --
  --   Header is chunk of data we want to transfer unecncrypted but still want
  --   it to be part of tag digest. So tag verifies validity of both encrypted
  --   data and unencrypted header.
  -> ByteString
  -- ^ Input plaintext to be encrypted
  -> CryptoFailable ByteString
  -- ^ Ciphertext with a 128-bit crypto-tag appended.
encryptChaChaPoly nonce key header plaintext = do
  st1 <- C.nonce12 nonce >>= C.initialize key
  let st2 = C.finalizeAAD $ C.appendAAD header st1
  let (out, st3) = C.encrypt plaintext st2
  let auth       = C.finalize st3
  pure $ out <> BA.convert auth

toEither :: CryptoFailable a -> Either Text a
toEither (CryptoPassed x ) = pure x
toEither (CryptoFailed er) = Left $ show er

-- | Take HDPassphrase as symmetric key and encrypted derivation path (aka
--   HDPayload) and try to decrypt it. Verify that appended crypto-tag is the
--   same as got in result of work ChaChaPoly1305 algorithm.
decryptChaChaPoly
  :: ByteString
  -- ^ Nonce (12 random bytes)
  -> ByteString
  -- ^ Symmetric key
  -> ByteString
  -- ^ Encryption header, optional associated data.
  -> ByteString
  -- ^ Input plaintext to be decrypted
  -> Either Text ByteString
  -- ^ Decrypted text or error
decryptChaChaPoly nonce key header encDataWithTag = do
  let tagSize = 16 :: Int
  let l = B.length encDataWithTag
  (l >= tagSize)
    `orThrowError` "Length of encrypted text must be at least "
    <> show tagSize
  let (encData, rawTag) = B.splitAt (l - 16) encDataWithTag
  tag <- toEither (Poly.authTag rawTag)
  st1 <- toEither (C.nonce12 nonce >>= C.initialize key)
  let st2 = C.finalizeAAD $ C.appendAAD header st1
  let (out, st3) = C.decrypt encData st2
  (C.finalize st3 == tag) `orThrowError` "Crypto-tag mismatch"
  -- is it free from mem leaks?
  pure out
