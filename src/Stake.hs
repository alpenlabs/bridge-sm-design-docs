{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use newtype instead of data" -}

module Stake
  ( StakeState
  , processStakeData
  , processUnstakingNonces
  , processUnstakingPartials
  , processStakeConfirmed
  , processPreimageRevealed
  , processUnstaking
  , notifyNewBlock
  , hasStaked
  , isUnstaked
  , getPreimage
  , lastProcessedBlock
  , processNagTick
  , processRetryTick
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Word (Word32)

-- Prelude
type U32 = Word32
type OperatorIdx = U32
type BitcoinBlockHeight = U32
type StakeData = String -- Placeholder for actual stake inputs (required to generate the entire staking/unstaking graph)
type Preimage = String -- Placeholder for actual preimage data (would probabl be a 32-byte array)
type Txid = String -- Placeholder for actual transaction ID
type Transaction = String -- Placeholder for transaction
type OutPoint = (Txid, U32) -- Placeholder for actual outpoint
type PubNonce = String -- Placeholder for public nonce (used for MuSig2 signing)
type AggNonce = String -- Placeholder for aggregated nonce (used for MuSig2 signing)
type PartialSig = String -- Placeholder for partial signature (used for MuSig2 signing)
type Signature = String -- Placeholder for aggregated signature
type SchnorrKey = String -- Placeholder for Schnorr public key
type P2PKey = String -- Placeholder for P2P public key

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

expectedStakeTxidFromStakeData :: StakeData -> Txid
expectedStakeTxidFromStakeData _ = "expected_stake_txid_placeholder" -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation

-- Parameters
-- Numbers are chosen arbitrarily
unstakingTimelock :: U32
unstakingTimelock = 3024 -- maximum duration (in Bitcoin blocks) for the withdrawal game

-- State
-- Represents the state of the operator stakes
-- Each such state is recognized uniquely by the operator index
data StakeState
  = Created -- state where the state has been initialized (must happen at genesis/setup)
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      , blockHeight :: BitcoinBlockHeight -- the height of the last Bitcoin block observed by this state machine
      }
  | StakeGraphGenerated -- state where the data required to generate the entire graph has been generated/received
      { operatorIdx :: OperatorIdx
      , blockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData -- data required to construct a stake transaction
      , nonces :: Map.Map OperatorIdx (NonEmpty PubNonce) -- nonces collected from operators for MuSig2 signing (flattened)
      }
  | UnstakingNoncesCollected -- state where the nonces required to sign the unstaking transactions have been collected
      { operatorIdx :: OperatorIdx
      , blockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , nonces :: Map.Map OperatorIdx (NonEmpty PubNonce)
      , aggNonces :: NonEmpty AggNonce -- aggregated nonces for the unstaking transaction
      , partials :: Map.Map OperatorIdx (NonEmpty PartialSig) -- partial signatures collected from operators (flattened)
      }
  | UnstakingSigned -- state where the unstaking transactions have been signed
      { operatorIdx :: OperatorIdx
      , blockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , expectedStakeTxid :: Txid -- the expected txid of the staking transaction (this can be kept in state or computed on the fly)
      , signatures :: NonEmpty Signature -- aggregated signatures for the unstaking transactions (flattened)
      }
  | Confirmed -- state where the stake transaction has been confirmed on-chain
      { operatorIdx :: OperatorIdx
      , blockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , stakeTxid :: Txid -- the txid of the confirmed staking transaction (this can be kept in state or computed on the fly)
      }
  | PreimageRevealed -- state where the unstaking preimage has been revealed via the unstaking intent transaction posted on-chain
      { operatorIdx :: OperatorIdx
      , blockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , preimage :: Preimage -- the unstaking preimage revealed by the operator
      , unstakingIntentBlockHeight :: BitcoinBlockHeight -- the block height at which the unstaking intent transaction was confirmed
      , expectedUnstakingTxid :: Txid -- the expected txid of the unstaking transaction
      }
  | Unstaked -- state where the unstaking transaction has been confirmed on-chain
      { operatorIdx :: OperatorIdx
      , preimage :: Preimage
      , unstakingTxid :: Txid -- the txid of the unstaking transaction
      }
  deriving (Show, Eq, Ord)

-- Duties
-- Tasks that need to be executed on every state transition
data StakeDuty
  = PublishStakeData
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      }
  | PublishUnstakingNonces
      { stakeData :: StakeData -- data required to construct the unstaking graph
      }
  | PublishUnstakingPartials
      { stakeData :: StakeData -- data required to construct the unstaking graph
      , aggNonces :: NonEmpty AggNonce -- aggregated nonces for the unstaking transactions
      }
  | PublishUnstakingIntent
      { stakeData :: StakeData -- data required to construct the unstaking graph
      }
  | PublishStake
      { stakeTx :: Transaction -- the unsigned stake transaction ready to be signed and published
      }
  | PublishUnstakingTx
      { stakeData :: StakeData -- data required to construct the unstaking graph
      }
  | Nag
      { duty :: NagDuty -- specific nag duty
      }
  deriving (Show, Eq, Ord)

data NagDuty
  = NagStakeData
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      }
  | NagUnstakingNonces
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      }
  | NagUnstakingPartials
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      }
  deriving (Show, Eq, Ord)

-- Output
-- Represents the output of the state machine after processing an event
data StakeTransitionOutput = StakeTransitionOutput
  { duty :: Maybe StakeDuty
  }
  deriving (Show, Eq, Ord)

emptyOutput :: StakeTransitionOutput
emptyOutput = StakeTransitionOutput {duty = Nothing}

-- Additional Types and Helpers
data OperatorTable where
  OperatorTable
    :: { operators
           :: Set.Set
                ( OperatorIdx
                , SchnorrKey
                , P2PKey
                )
       }
    -> OperatorTable
  deriving (Show, Eq, Ord)

opCardinality :: OperatorTable -> Int
opCardinality OperatorTable {..} = Set.size operators

-- Placeholder verification for partial signatures
-- For real MuSig2 verification, requires: individual nonces, aggregated nonces, and partial signature
verifyPartialSig :: OperatorTable -> OperatorIdx -> NonEmpty PubNonce -> NonEmpty AggNonce -> PartialSig -> Bool
verifyPartialSig _opTable _operatorIdx _pubNonces _aggNonces _partialSig =
  True -- Placeholder: accept all signatures for now

-- Helper to verify all partials in a collection
verifyAllPartials
  :: OperatorTable -> OperatorIdx -> NonEmpty PubNonce -> NonEmpty AggNonce -> NonEmpty PartialSig -> Bool
verifyAllPartials opTable opIdx pubNonces aggNonces partials =
  all (verifyPartialSig opTable opIdx pubNonces aggNonces) (NonEmpty.toList partials)

-- State Transition Functions
-- Functions to handle state transitions based on events
-- Declarations
processStakeData :: StakeState -> StakeData -> (StakeState, StakeTransitionOutput) -- Created -> StakeGraphGenerated
processUnstakingNonces
  :: StakeState -> OperatorTable -> OperatorIdx -> NonEmpty PubNonce -> (StakeState, StakeTransitionOutput) -- StakeGraphGenerated -> UnstakingNoncesCollected
processUnstakingPartials
  :: StakeState -> OperatorTable -> OperatorIdx -> NonEmpty PartialSig -> (StakeState, StakeTransitionOutput) -- UnstakingNoncesCollected -> UnstakingSigned
processStakeConfirmed :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput) -- UnstakingSigned -> Confirmed
processPreimageRevealed :: StakeState -> Transaction -> BitcoinBlockHeight -> (StakeState, StakeTransitionOutput) -- Confirmed -> PreimageRevealed
processUnstaking :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput) -- PreimageRevealed -> Unstaked
notifyNewBlock :: StakeState -> BitcoinBlockHeight -> (StakeState, StakeTransitionOutput) -- PreimageRevealed -> PreimageRevealed (with duty to publish unstaking tx)

-- Definitions
processStakeData Created {..} stakeData =
  let newState = StakeGraphGenerated {stakeData = stakeData, nonces = Map.empty, ..}
      output = StakeTransitionOutput {duty = Just (PublishUnstakingNonces {stakeData = stakeData})}
  in  (newState, output)
processStakeData StakeGraphGenerated {} _ = error "Duplicate: Stake data has already been processed"
processStakeData state _ = error $ "Rejected: Invalid state for receiving state data: " ++ show state

processUnstakingNonces StakeGraphGenerated {..} opTable operatorIdx' pubNonces =
  let updatedNonces =
        if isNothing $ Map.lookup operatorIdx' nonces
          then Map.insert operatorIdx' pubNonces nonces
          else error $ "Duplicate: Unstaking nonces have already been received from operator: " ++ show operatorIdx'
  in  if Map.size updatedNonces == opCardinality opTable
        then
          let aggNonces = "agg_nonce_placeholder" :| [] -- In a real implementation, this would be computed from the collected nonces
              newState = UnstakingNoncesCollected {nonces = updatedNonces, aggNonces = aggNonces, partials = Map.empty, ..}
              output = StakeTransitionOutput {duty = Just (PublishUnstakingPartials {stakeData = stakeData, aggNonces = aggNonces})}
          in  (newState, output)
        else
          let newState = StakeGraphGenerated {nonces = updatedNonces, ..}
              output = emptyOutput
          in  (newState, output)
processUnstakingNonces UnstakingNoncesCollected {} _ _ _ = error "Duplicate: Unstaking nonces have already been collected"
processUnstakingNonces state _ _ _ = error $ "Rejected: Invalid state for collecting unstaking nonces: " ++ show state

processUnstakingPartials UnstakingNoncesCollected {..} opTable operatorIdx' partialSig =
  let operatorNonces = case Map.lookup operatorIdx' nonces of
        Just ns -> ns
        Nothing -> error "Rejected: Operator not found"
      updatedPartials =
        if isNothing $ Map.lookup operatorIdx' partials
          then
            if verifyAllPartials opTable operatorIdx' operatorNonces aggNonces partialSig
              then Map.insert operatorIdx' partialSig partials
              else error $ "Rejected: Partial signature verification failed for operator: " ++ show operatorIdx'
          else error $ "Duplicate: Unstaking partial signatures received from operator: " ++ show operatorIdx'
  in  if Map.size updatedPartials == opCardinality opTable
        then
          let signatures = "signature_placeholder" :| [] -- In a real implementation, this would be computed from the collected partial signatures and agg nonce
              expectedStakeTxid = expectedStakeTxidFromStakeData stakeData
              newState = UnstakingSigned {expectedStakeTxid = expectedStakeTxid, signatures = signatures, ..}
          in  (newState, emptyOutput)
        else
          let newState = UnstakingNoncesCollected {partials = updatedPartials, ..}
          in  (newState, emptyOutput)
processUnstakingPartials UnstakingSigned {} _ _ _ = error "Duplicate: Unstaking partials have already been collected"
processUnstakingPartials state _ _ _ = error $ "Rejected: Invalid state for collecting unstaking partials: " ++ show state

processStakeConfirmed UnstakingSigned {..} tx
  | txid tx == expectedStakeTxid =
      let newState = Confirmed {stakeTxid = expectedStakeTxid, ..}
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Unexpected transaction for stake confirmation"
processStakeConfirmed UnstakingNoncesCollected {..} tx
  | txid tx == expectedStakeTxidFromStakeData stakeData =
      let newState = Confirmed {stakeTxid = expectedStakeTxidFromStakeData stakeData, ..}
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Unexpected transaction for stake confirmation"
processStakeConfirmed Confirmed {} _ = error "Duplicate: Stake has already been confirmed"
processStakeConfirmed state _ = error $ "Rejected: Invalid state for stake confirmation: " ++ show state

processPreimageRevealed Confirmed {..} tx btcBlockHeight
  | (stakeTxid, 0) == NonEmpty.head (inpoints tx) =
      let revealedPreimage = "preimage" -- In a real implementation, this would be derived from the transaction witness
      in  let newState =
                PreimageRevealed
                  { preimage = revealedPreimage
                  , unstakingIntentBlockHeight = btcBlockHeight
                  , expectedUnstakingTxid = "expected_unstaking_txid_placeholder" -- In a real implementation, this would be derived from the stake data
                  , ..
                  }
              output = StakeTransitionOutput {duty = Nothing}
          in  (newState, output)
  | otherwise = error "Rejected: Transaction does not match expected unstaking intent transaction"
processPreimageRevealed PreimageRevealed {} _ _ = error "Duplicate: Preimage has already been revealed"
processPreimageRevealed Unstaked {} _ _ = error "Rejected: Terminal state"
processPreimageRevealed state _ _ = error $ "Invalid Event: Invalid state for preimage revelation: " ++ show state

processUnstaking PreimageRevealed {..} tx
  | txid tx == expectedUnstakingTxid =
      let newState = Unstaked {unstakingTxid = expectedUnstakingTxid, ..}
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Unexpected transaction for unstaking"
processUnstaking Unstaked {} _ = error "Rejected: Unstaking has already been processed"
processUnstaking state _ = error $ "Invalid Event: Invalid state for unstaking: " ++ show state

notifyNewBlock state newHeight
  | isJust (lastProcessedBlock state) && fromJust (lastProcessedBlock state) >= newHeight =
      error "Rejected: Rejecting already processed block height"
notifyNewBlock state@PreimageRevealed {..} btcBlockHeight
  | btcBlockHeight > unstakingIntentBlockHeight + unstakingTimelock =
      let output = StakeTransitionOutput {duty = Just PublishUnstakingTx {stakeData = stakeData}}
      in  (state {blockHeight = btcBlockHeight}, output)
  | otherwise = (PreimageRevealed {blockHeight = btcBlockHeight, ..}, emptyOutput)
notifyNewBlock Unstaked {} _ = error "Rejected: terminal state"
notifyNewBlock state btcBlockHeight = (state {blockHeight = btcBlockHeight}, emptyOutput)

-- Introspection Functions
hasStaked :: StakeState -> Bool
hasStaked Created {} = False
hasStaked StakeGraphGenerated {} = False
hasStaked UnstakingNoncesCollected {} = False
hasStaked UnstakingSigned {} = False
hasStaked _ = True

isUnstaked :: StakeState -> Bool
isUnstaked Unstaked {} = True
isUnstaked _ = False

-- since the preimage information is static once revealed, any higher-level module needs to extract this information at every block
-- this is not information that will change over time, so it is not emitted as a signal in response to events
getPreimage :: StakeState -> Maybe Preimage
getPreimage PreimageRevealed {..} = Just preimage
getPreimage Unstaked {..} = Just preimage
getPreimage _ = Nothing

lastProcessedBlock :: StakeState -> Maybe BitcoinBlockHeight
lastProcessedBlock state = case state of
  Unstaked {} -> Nothing
  _ -> Just (blockHeight state)

-- Retry Handlers
-- Declarations
processNagTick :: StakeState -> OperatorTable -> Set.Set StakeDuty
processRetryTick :: StakeState -> OperatorTable -> Set.Set StakeDuty
-- Definitions
processNagTick state opTable =
  let expectedIds = Set.map (\(idx, _, _) -> idx) $ operators opTable
      presentIds = case state of
        Created {} -> expectedIds -- full set so that the diff is null and we calculate the nag duty based on stake owner
        StakeGraphGenerated {..} -> Map.keysSet nonces
        UnstakingNoncesCollected {..} -> Map.keysSet partials
        _ -> Set.empty
      missingIds = Set.difference expectedIds presentIds
  in  Set.fromList
        $ mapMaybe
          ( \opIdx -> case state of
              Created {operatorIdx} -> Just Nag {duty = NagStakeData {operatorIdx}}
              StakeGraphGenerated {} -> Just Nag {duty = NagUnstakingNonces {operatorIdx = opIdx}}
              UnstakingNoncesCollected {} -> Just Nag {duty = NagUnstakingPartials {operatorIdx = opIdx}}
              _ -> Nothing
          )
        $ Set.toList missingIds

processRetryTick state _ = case state of
  UnstakingSigned {} -> Set.singleton PublishStake {stakeTx = "unsigned_stake_tx_placeholder"} -- In a real implementation, this would be the actual unsigned stake transaction ready to be signed and published
  _ -> Set.empty
