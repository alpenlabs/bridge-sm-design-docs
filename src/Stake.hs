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
  , processSlashConfirmed
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
import Data.Word (Word32, Word8)

-- Prelude
type U32 = Word32
type OperatorIdx = U32
type BitcoinBlockHeight = U32
type StakeData = String -- Placeholder for actual stake inputs (required to generate the entire staking/unstaking graph)
type StakeFunctor a = NonEmpty a -- Placeholder for per-transaction graph data
type Preimage = [Word8] -- Placeholder for actual preimage data
type Txid = String -- Placeholder for actual transaction ID
type Transaction = String -- Placeholder for transaction
type PubNonce = String -- Placeholder for public nonce (used for MuSig2 signing)
type AggNonce = String -- Placeholder for aggregated nonce (used for MuSig2 signing)
type PartialSig = String -- Placeholder for partial signature (used for MuSig2 signing)
type Signature = String -- Placeholder for aggregated signature
type SchnorrKey = String -- Placeholder for Schnorr public key
type P2PKey = String -- Placeholder for P2P public key
type OutPoint = (Txid, U32) -- Placeholder for Bitcoin transaction outpoint (txid and output index)

data StakeGraphSummary = StakeGraphSummary
  { stake :: Txid
  , unstakingIntent :: Txid
  , unstaking :: Txid
  }
  deriving (Show, Eq, Ord)

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation for input outpoints (head :| tail)

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

stakeGraphSummaryFromStakeData :: StakeData -> StakeGraphSummary
stakeGraphSummaryFromStakeData _ =
  StakeGraphSummary
    { stake = "stake_txid_placeholder"
    , unstakingIntent = "unstaking_intent_txid_placeholder"
    , unstaking = "unstaking_txid_placeholder"
    } -- Placeholder implementation

-- Parameters
-- Numbers are chosen arbitrarily
unstakingTimelock :: U32
unstakingTimelock = 3024 -- maximum duration (in Bitcoin blocks) for the withdrawal game

-- Constants
stakeOutputIndex :: U32
stakeOutputIndex = 1 -- the output index in the stake transaction that is used as input

-- State
-- Represents the state of the operator stakes
-- Each such state is recognized uniquely by the operator index
data StakeState
  = Created -- state where the state has been initialized (must happen at genesis/setup)
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      , lastBlockHeight :: BitcoinBlockHeight -- the height of the last Bitcoin block observed by this state machine
      }
  | StakeGraphGenerated -- state where the data required to generate the entire graph has been generated/received
      { operatorIdx :: OperatorIdx
      , lastBlockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData -- data required to construct a stake transaction
      , summary :: StakeGraphSummary -- collection of all txids in the stake graph
      , pubNonces :: Map.Map OperatorIdx (StakeFunctor PubNonce) -- public nonces collected from operators
      }
  | UnstakingNoncesCollected -- state where the nonces required to sign the unstaking transactions have been collected
      { operatorIdx :: OperatorIdx
      , lastBlockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , summary :: StakeGraphSummary
      , pubNonces :: Map.Map OperatorIdx (StakeFunctor PubNonce)
      , aggNonces :: StakeFunctor AggNonce -- aggregated nonces for the unstaking transaction
      , partialSignatures :: Map.Map OperatorIdx (StakeFunctor PartialSig) -- partial signatures collected from operators
      }
  | UnstakingSigned -- state where the unstaking transactions have been signed
      { operatorIdx :: OperatorIdx
      , lastBlockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , summary :: StakeGraphSummary
      , unstakingSignatures :: StakeFunctor Signature -- aggregated signatures for the unstaking transactions
      }
  | Confirmed -- state where the stake transaction has been confirmed on-chain
      { operatorIdx :: OperatorIdx
      , lastBlockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , summary :: StakeGraphSummary
      , signatures :: Maybe (StakeFunctor Signature) -- signatures may be absent if collection finished too late
      }
  | PreimageRevealed -- state where the unstaking preimage has been revealed via the unstaking intent transaction posted on-chain
      { operatorIdx :: OperatorIdx
      , lastBlockHeight :: BitcoinBlockHeight
      , stakeData :: StakeData
      , summary :: StakeGraphSummary
      , preimage :: Preimage -- the unstaking preimage revealed by the operator
      , unstakingIntentBlockHeight :: BitcoinBlockHeight -- the block height at which the unstaking intent transaction was confirmed
      , signatures :: Maybe (StakeFunctor Signature) -- signatures may be absent if collection finished too late
      }
  | Slashed -- state where the operator's stake has been slashed by another operator
      { operatorIdx :: OperatorIdx
      , summary :: StakeGraphSummary
      , preimage' :: Maybe Preimage -- the preimage revealed if transition occurs from the `PreimageRevealed` state (needed for UnstakingBurn)
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
      { -- the unsigned unstaking intent transaction ready to be signed and published
        -- this is unsigned because it requires a preimage for finalization which needs to be queried from an external service.
        unsignedUnstakingIntentTx :: Transaction
      }
  | PublishStake
      { stakeTx :: Transaction -- the unsigned stake transaction ready to be signed and published
      }
  | PublishUnstakingTx
      { unstakingTx :: Transaction -- the signed finalized unstaking transaction ready to be published
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

-- Placeholder verification for partial signatures.
-- In the real MuSig2 flow, each partial is checked per input against that operator's pubnonce,
-- the aggregated nonce, and the signing context; only after every operator passes verification
-- are the final signatures aggregated.
verifyPartialSig :: OperatorTable -> OperatorIdx -> NonEmpty PubNonce -> NonEmpty AggNonce -> PartialSig -> Bool
verifyPartialSig _opTable _operatorIdx _pubNonces _aggNonces _partialSig =
  True -- Placeholder: accept all signatures for now

-- Helper to verify all partials in a collection
verifyAllPartials
  :: OperatorTable -> OperatorIdx -> NonEmpty PubNonce -> NonEmpty AggNonce -> NonEmpty PartialSig -> Bool
verifyAllPartials opTable opIdx pubNonces aggNonces partials =
  all (verifyPartialSig opTable opIdx pubNonces aggNonces) (NonEmpty.toList partials)

isSlashTx :: StakeGraphSummary -> Transaction -> Bool
-- Spends the stake output but is not the unstaking transaction
isSlashTx summary tx = ((stake summary, stakeOutputIndex) `elem` inpoints tx) && txid tx /= unstaking summary

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
processSlashConfirmed :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput) -- (Confirmed | PreimageRevealed) -> Slashed
processUnstaking :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput) -- PreimageRevealed -> Unstaked
notifyNewBlock :: StakeState -> BitcoinBlockHeight -> (StakeState, StakeTransitionOutput) -- PreimageRevealed -> PreimageRevealed (with duty to publish unstaking tx)

-- Definitions
processStakeData Created {..} stakeData =
  let newState =
        StakeGraphGenerated
          { operatorIdx = operatorIdx
          , lastBlockHeight = lastBlockHeight
          , stakeData = stakeData
          , summary = stakeGraphSummaryFromStakeData stakeData
          , pubNonces = Map.empty
          }
      output = StakeTransitionOutput {duty = Just (PublishUnstakingNonces {stakeData = stakeData})}
  in  (newState, output)
processStakeData StakeGraphGenerated {} _ = error "Duplicate: Stake data has already been processed"
processStakeData state _ = error $ "Rejected: Invalid state for receiving state data: " ++ show state

processUnstakingNonces StakeGraphGenerated {..} opTable operatorIdx' operatorPubNonces =
  let updatedNonces =
        if isNothing $ Map.lookup operatorIdx' pubNonces
          then Map.insert operatorIdx' operatorPubNonces pubNonces
          else error $ "Duplicate: Unstaking nonces have already been received from operator: " ++ show operatorIdx'
  in  if Map.size updatedNonces == opCardinality opTable
        then
          let aggNonces = "agg_nonce_placeholder" :| [] -- In a real implementation, this would be computed from the collected nonces
              newState =
                UnstakingNoncesCollected
                  { operatorIdx = operatorIdx
                  , lastBlockHeight = lastBlockHeight
                  , stakeData = stakeData
                  , summary = summary
                  , pubNonces = updatedNonces
                  , aggNonces = aggNonces
                  , partialSignatures = Map.empty
                  }
              output = StakeTransitionOutput {duty = Just (PublishUnstakingPartials {stakeData = stakeData, aggNonces = aggNonces})}
          in  (newState, output)
        else
          let newState = StakeGraphGenerated {pubNonces = updatedNonces, ..}
              output = emptyOutput
          in  (newState, output)
processUnstakingNonces UnstakingNoncesCollected {} _ _ _ = error "Duplicate: Unstaking nonces have already been collected"
processUnstakingNonces state _ _ _ = error $ "Rejected: Invalid state for collecting unstaking nonces: " ++ show state

processUnstakingPartials UnstakingNoncesCollected {..} opTable operatorIdx' partialSig =
  let operatorNonces = case Map.lookup operatorIdx' pubNonces of
        Just ns -> ns
        Nothing -> error "Rejected: Operator not found"
      updatedPartials =
        if isNothing $ Map.lookup operatorIdx' partialSignatures
          then
            if verifyAllPartials opTable operatorIdx' operatorNonces aggNonces partialSig
              then Map.insert operatorIdx' partialSig partialSignatures
              else error $ "Rejected: Partial signature verification failed for operator: " ++ show operatorIdx'
          else error $ "Duplicate: Unstaking partial signatures received from operator: " ++ show operatorIdx'
  in  if Map.size updatedPartials == opCardinality opTable
        then
          let signatures = "signature_placeholder" :| [] -- In a real implementation, this would be computed from the collected partial signatures and agg nonce
              newState =
                UnstakingSigned
                  { operatorIdx = operatorIdx
                  , lastBlockHeight = lastBlockHeight
                  , stakeData = stakeData
                  , summary = summary
                  , unstakingSignatures = signatures
                  }
          in  (newState, emptyOutput)
        else
          let newState = UnstakingNoncesCollected {partialSignatures = updatedPartials, ..}
          in  (newState, emptyOutput)
processUnstakingPartials UnstakingSigned {} _ _ _ = error "Duplicate: Unstaking partials have already been collected"
processUnstakingPartials state _ _ _ = error $ "Rejected: Invalid state for collecting unstaking partials: " ++ show state

processStakeConfirmed UnstakingSigned {..} tx
  | txid tx == stake summary =
      let newState =
            Confirmed
              { operatorIdx = operatorIdx
              , lastBlockHeight = lastBlockHeight
              , stakeData = stakeData
              , summary = summary
              , signatures = Just unstakingSignatures
              }
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Unexpected transaction for stake confirmation"
processStakeConfirmed UnstakingNoncesCollected {..} tx
  | txid tx == stake summary =
      let newState =
            Confirmed
              { operatorIdx = operatorIdx
              , lastBlockHeight = lastBlockHeight
              , stakeData = stakeData
              , summary = summary
              , signatures = Nothing
              }
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Unexpected transaction for stake confirmation"
processStakeConfirmed Confirmed {} _ = error "Duplicate: Stake has already been confirmed"
processStakeConfirmed state _ = error $ "Rejected: Invalid state for stake confirmation: " ++ show state

processPreimageRevealed Confirmed {..} tx btcBlockHeight
  | txid tx /= unstakingIntent summary =
      error "Rejected: Transaction does not match expected unstaking intent transaction"
  | otherwise =
      let revealedPreimage = replicate 32 0 -- Placeholder for extracting the first witness stack item preimage
          newState =
            PreimageRevealed
              { operatorIdx = operatorIdx
              , lastBlockHeight = btcBlockHeight
              , preimage = revealedPreimage
              , unstakingIntentBlockHeight = btcBlockHeight
              , ..
              }
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
processPreimageRevealed PreimageRevealed {} _ _ = error "Duplicate: Preimage has already been revealed"
processPreimageRevealed Unstaked {} _ _ = error "Rejected: Terminal state"
processPreimageRevealed state _ _ = error $ "Invalid Event: Invalid state for preimage revelation: " ++ show state

processSlashConfirmed Confirmed {..} tx
  | isSlashTx summary tx =
      let newState =
            Slashed
              { preimage' = Nothing
              , ..
              }
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Transaction does not match expected slash transaction"
processSlashConfirmed PreimageRevealed {..} tx
  | isSlashTx summary tx =
      let newState =
            Slashed
              { preimage' = Just preimage
              , ..
              }
          output = StakeTransitionOutput {duty = Nothing}
      in  (newState, output)
  | otherwise = error "Rejected: Transaction does not match expected slash transaction"
processSlashConfirmed Slashed {} _ = error "Duplicate: Stake has already been slashed"
processSlashConfirmed Unstaked {} _ = error "Rejected: Terminal state"
processSlashConfirmed state _ = error $ "Invalid Event: Invalid state for slash confirmation: " ++ show state

processUnstaking PreimageRevealed {..} tx
  | txid tx == unstaking summary =
      let newState =
            Unstaked
              { operatorIdx = operatorIdx
              , preimage = preimage
              , unstakingTxid = unstaking summary
              }
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
      let unstakingTx = "signed_finalized_unstaking_tx_placeholder" -- In a real implementation, this would be the fully signed finalized unstaking transaction ready for broadcast.
          output = StakeTransitionOutput {duty = Just PublishUnstakingTx {unstakingTx}}
      in  (state {lastBlockHeight = btcBlockHeight}, output)
  | otherwise = (PreimageRevealed {lastBlockHeight = btcBlockHeight, ..}, emptyOutput)
notifyNewBlock Slashed {} _ = error "Rejected: terminal state"
notifyNewBlock Unstaked {} _ = error "Rejected: terminal state"
notifyNewBlock state btcBlockHeight = (state {lastBlockHeight = btcBlockHeight}, emptyOutput)

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
  _ -> Just (lastBlockHeight state)

-- Retry Handlers
-- Declarations
processNagTick :: StakeState -> OperatorTable -> Set.Set StakeDuty
processRetryTick :: StakeState -> OperatorTable -> Set.Set StakeDuty
-- Definitions
processNagTick state opTable =
  let expectedIds = Set.map (\(idx, _, _) -> idx) $ operators opTable
      presentIds = case state of
        Created {} -> expectedIds -- full set so that the diff is null and we calculate the nag duty based on stake owner
        StakeGraphGenerated {..} -> Map.keysSet pubNonces
        UnstakingNoncesCollected {..} -> Map.keysSet partialSignatures
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
