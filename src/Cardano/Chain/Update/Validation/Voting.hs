{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Validation rules for registering votes and confirming proposals
--
--   This is an implementation of the rules defined in the Byron ledger
--   specification
module Cardano.Chain.Update.Validation.Voting
  ( Environment(..)
  , RegistrationEnvironment(..)
  , State(..)
  , Error(..)
  , registerVoteWithConfirmation
  )
where

import Cardano.Prelude hiding (State)

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Cardano.Binary (Annotated)
import Cardano.Chain.Common (StakeholderId, mkStakeholderId)
import qualified Cardano.Chain.Delegation as Delegation
import Cardano.Chain.Slotting (FlatSlotId)
import Cardano.Chain.Update.Proposal (UpId)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.Vote
  ( AVote(..)
  , recoverSignedBytes
  , proposalId
  )
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignUSVote)
  , verifySignatureDecoded
  )


-- | Environment used to register votes and confirm proposals
data Environment = Environment
  { veCurrentSlot                   :: !FlatSlotId
  , veProtocolParameters            :: !ProtocolParameters
  , veVotingRegistrationEnvironment :: !RegistrationEnvironment
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

-- | Environment required to validate and register a vote
data RegistrationEnvironment = RegistrationEnvironment
 { vreRegisteredUpdateProposal :: !(Set UpId)
 , vreDelegationMap            :: !Delegation.Map
 } deriving (Eq, Show, Generic)
   deriving anyclass NFData

-- | State keeps track of registered votes and confirmed proposals
data State = State
  { vsVotes              :: !RegisteredVotes
  , vsConfirmedProposals :: !(Map UpId FlatSlotId)
  }

type RegisteredVotes = Map UpId (Set StakeholderId)

-- | Error captures the ways in which vote registration could fail
data Error
  = VotingInvalidSignature
  | VotingProposalNotRegistered UpId
  | VotingVoterNotDelegate StakeholderId
  deriving (Eq, Show)


-- | Register a vote and confirm the corresponding proposal if it passes the
--   voting threshold. This corresponds to the @UPVOTE@ rules in the spec.
registerVoteWithConfirmation
  :: MonadError Error m
  => Annotated ProtocolMagicId ByteString
  -> Environment
  -> State
  -> AVote ByteString
  -> m State
registerVoteWithConfirmation pm votingEnv vs vote = do

  -- Register the vote ignoring proposal confirmation
  votes' <- registerVote pm voteRegEnv votes vote

  -- Confirm the proposal if it passes the threshold and isn't confirmed
  let
    confirmedProposals' = if pastThreshold votes' && not (isConfirmed upId)
      then M.insert upId slot confirmedProposals
      else confirmedProposals

  -- Return the new state with additional vote and maybe confirmation
  pure $ State
    { vsVotes = votes'
    , vsConfirmedProposals = confirmedProposals'
    }
 where
  Environment slot _ voteRegEnv  = votingEnv
  State votes confirmedProposals = vs

  -- | This is the number of genesis keys that need to support a proposal
  threshold :: Int
  threshold = 4

  pastThreshold :: RegisteredVotes -> Bool
  pastThreshold votes' =
    length (M.findWithDefault Set.empty upId votes') >= threshold

  isConfirmed = flip M.member confirmedProposals

  upId        = proposalId vote


-- | Validate and register a vote
--
--   We check that
--
--   1) The vote is for a registered proposal
--   2) There is at least one genesis key delegating to the voter
--   3) The signature is valid
--
--   This corresponds to the `ADDVOTE` rule in the spec.
registerVote
  :: MonadError Error m
  => Annotated ProtocolMagicId ByteString
  -> RegistrationEnvironment
  -> RegisteredVotes
  -> AVote ByteString
  -> m RegisteredVotes
registerVote pm vre votes vote = do
  -- Check that the proposal being voted on is registered
  (upId `Set.member` registeredProposals)
    `orThrowError` VotingProposalNotRegistered upId

  -- Check that the set of genesis keys is not empty
  delegator <- case Delegation.lookupR voter delegationMap of
    Nothing -> throwError (VotingVoterNotDelegate voter)
    Just d  -> pure d

  -- Check that the signature is valid
  verifySignatureDecoded pm SignUSVote voterPK signedBytes signature
    `orThrowError` VotingInvalidSignature

  -- Add the delegators to the set of votes for this proposal
  pure $ M.insertWith Set.union upId (Set.singleton delegator) votes
 where
  RegistrationEnvironment registeredProposals delegationMap = vre

  UnsafeVote { voterPK, signature } = vote

  voter       = mkStakeholderId voterPK

  upId        = proposalId vote

  signedBytes = recoverSignedBytes vote
