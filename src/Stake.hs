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
  , isAvailable
  , isUnstaked
  , getPreimage
  , lastProcessedBlock
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (isNothing)
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

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation

-- Parameters
-- Numbers are chosen arbitrarily
maxGameDuration :: U32
maxGameDuration = 3024 -- maximum duration (in Bitcoin blocks) for the withdrawal game

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
  deriving (Show, Eq)

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
  | PublishUnstakingTx
      { stakeData :: StakeData -- data required to construct the unstaking graph
      }
  deriving (Show, Eq)

-- Output
-- Represents the output of the state machine after processing an event
data StakeTransitionOutput = StakeTransitionOutput
  { duty :: Maybe StakeDuty
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

opCardinality :: OperatorTable -> Int
opCardinality OperatorTable {..} = Set.size operators

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
processStakeData StakeGraphGenerated {} _ = error "Stake data has already been processed"
processStakeData state _ = error $ "Received stale stake data event in state: " ++ show state

processUnstakingNonces StakeGraphGenerated {..} opTable operatorIdx' pubNonces =
  let updatedNonces =
        if isNothing $ Map.lookup operatorIdx' nonces
          then Map.insert operatorIdx' pubNonces nonces
          else nonces -- Ignore duplicate nonces from the same operator
  in  if Map.size updatedNonces == opCardinality opTable
        then
          let aggNonces = "agg_nonce_placeholder" :| [] -- In a real implementation, this would be computed from the collected nonces
              newState = UnstakingNoncesCollected {aggNonces = aggNonces, partials = Map.empty, ..}
              output = StakeTransitionOutput {duty = Just (PublishUnstakingPartials {stakeData = stakeData, aggNonces = aggNonces})}
          in  (newState, output)
        else
          let newState = StakeGraphGenerated {nonces = updatedNonces, ..}
              output = emptyOutput
          in  (newState, output)
processUnstakingNonces UnstakingNoncesCollected {} _ _ _ = error "Unstaking nonces have already been collected"
processUnstakingNonces state _ _ _ = error $ "Invalid state for collecting unstaking nonces: " ++ show state

processUnstakingPartials UnstakingNoncesCollected {..} opTable operatorIdx' partialSig =
  let updatedPartials =
        if isNothing $ Map.lookup operatorIdx' partials
          then Map.insert operatorIdx' partialSig partials
          else partials -- Ignore duplicate partial signatures from the same operator
  in  if Map.size updatedPartials == opCardinality opTable
        then
          let signatures = "signature_placeholder" :| [] -- In a real implementation, this would be computed from the collected partial signatures and agg nonce
              expectedStakeTxid = "expected_stake_txid_placeholder" -- In a real implementation, this would be derived from the stake data
              newState = UnstakingSigned {expectedStakeTxid = expectedStakeTxid, signatures = signatures, ..}
          in  (newState, emptyOutput)
        else
          let newState = UnstakingNoncesCollected {partials = updatedPartials, ..}
          in  (newState, emptyOutput)
processUnstakingPartials UnstakingSigned {} _ _ _ = error "Unstaking partials have already been collected"
processUnstakingPartials state _ _ _ = error $ "Invalid state for collecting unstaking partials: " ++ show state

processStakeConfirmed UnstakingSigned {..} tx
  | txid tx == expectedStakeTxid =
      let newState = Confirmed {stakeTxid = expectedStakeTxid, ..}
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Unexpected transaction for stake confirmation"
processStakeConfirmed state _ = (state, emptyOutput)

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
  | otherwise = error "Transaction does not match expected unstaking intent transaction"
processPreimageRevealed state@PreimageRevealed {} _ _ = (state, emptyOutput)
processPreimageRevealed state _ _ = error $ "Invalid state for preimage revelation: " ++ show state

processUnstaking PreimageRevealed {..} tx
  | txid tx == expectedUnstakingTxid =
      let newState = Unstaked {unstakingTxid = expectedUnstakingTxid, ..}
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Unexpected transaction for unstaking"
processUnstaking state@Unstaked {} _ = (state, StakeTransitionOutput {duty = Nothing}) -- re-emit the signal if already unstaked to maintain idempotency
processUnstaking state _ = (state, emptyOutput)

notifyNewBlock state@PreimageRevealed {..} btcBlockHeight
  | btcBlockHeight > unstakingIntentBlockHeight + maxGameDuration =
      let output = StakeTransitionOutput {duty = Just PublishUnstakingTx {stakeData = stakeData}}
      in  (state {blockHeight = btcBlockHeight}, output)
  | otherwise = (PreimageRevealed {blockHeight = btcBlockHeight, ..}, emptyOutput)
notifyNewBlock state@Unstaked {} _ = (state, emptyOutput) -- does not need any more updates
notifyNewBlock state btcBlockHeight = (state {blockHeight = btcBlockHeight}, emptyOutput)

-- Introspection Functions
isAvailable :: StakeState -> Bool
isAvailable Created {} = False
isAvailable StakeGraphGenerated {} = False
isAvailable UnstakingNoncesCollected {} = False
isAvailable UnstakingSigned {} = False
isAvailable _ = True

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
