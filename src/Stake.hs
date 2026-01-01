{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Stake (
  StakeState,
  processStakeConfirmed,
  processPreimageRevealed,
  processUnstaking,
  isUnstaked,
  getPreimage,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word32)

-- Prelude
type U32 = Word32
type OperatorIdx = U32
type StakeData = String -- Placeholder for actual stake inputs
type Preimage = String -- Placeholder for actual preimage data (would probabl be a 32-byte array)
type Txid = String -- Placeholder for actual transaction ID
type Transaction = String -- Placeholder for transaction
type OutPoint = (Txid, U32) -- Placeholder for actual outpoint

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation

-- State
-- Represents the state of the operator stakes
data StakeState
  = Created
      { operatorIdx :: OperatorIdx -- index of the operator who owns the stake
      , stakeData :: StakeData -- data required to construct a stake transaction
      , expectedStakeTxid :: Txid -- the expected txid of the staking transaction
      }
  | Confirmed
      { operatorIdx :: OperatorIdx
      , stakeData :: StakeData
      , stakeTxid :: Txid -- the txid of the confirmed staking transaction
      }
  | PreimageRevealed
      { operatorIdx :: OperatorIdx
      , preimage :: Preimage -- the unstaking preimage revealed by the operator
      , expectedUnstakingTxid :: Txid -- the expected txid of the unstaking transaction
      }
  | Unstaked
      { operatorIdx :: OperatorIdx
      , preimage :: Preimage
      , unstakingTxid :: Txid -- the txid of the unstaking transaction
      }
  deriving (Show, Eq)

-- Duties
-- Tasks that need to be executed on every state transition
data StakeDuty
  = CreateStake
  | PublishUnstakingIntent
  | PublishUnstakingTx
  deriving (Show, Eq)

-- Signals
-- Messages that need to be passed to other state machines
data StakeSignal
  = -- signal that the operator has successfully unstaked (and should be removed from the system)
    -- this is passed to the Operator State Machine to indicate that the operator is no longer active,
    -- however, their graphs should still be kept around in case they submit claims
    PreimageRevealedSignal OperatorIdx Preimage
  | UnstakedSignal OperatorIdx
  deriving (Show, Eq)

-- Output
-- Represents the output of the state machine after processing an event
data StakeTransitionOutput = StakeTransitionOutput
  { signal :: Maybe StakeSignal
  , duty :: Maybe StakeDuty
  }
  deriving (Show, Eq)

emptyOutput :: StakeTransitionOutput
emptyOutput = StakeTransitionOutput{signal = Nothing, duty = Nothing}

-- State Transition Functions
-- Functions to handle state transitions based on events
-- Declarations
processStakeConfirmed :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput)
processPreimageRevealed :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput)
processUnstaking :: StakeState -> Transaction -> (StakeState, StakeTransitionOutput)
-- Definitions
processStakeConfirmed Created{..} tx
  | txid tx == expectedStakeTxid =
      let newState = Confirmed{stakeTxid = expectedStakeTxid, ..}
          output = StakeTransitionOutput{signal = Nothing, duty = Nothing}
       in (newState, output)
  | otherwise = error "Unexpected transaction for stake confirmation"
processStakeConfirmed state _ = (state, emptyOutput)

processPreimageRevealed Confirmed{..} tx
  | (stakeTxid, 0) == NonEmpty.head (inpoints tx) =
      let revealedPreimage = "preimage" -- In a real implementation, this would be derived from the transaction witness
       in let newState = Unstaked{operatorIdx = operatorIdx, preimage = revealedPreimage, unstakingTxid = txid tx}
              output = StakeTransitionOutput{signal = Just (PreimageRevealedSignal operatorIdx revealedPreimage), duty = Nothing}
           in (newState, output)
  | otherwise = error "Transaction does not match expected unstaking intent transaction"
processPreimageRevealed state _ = (state, emptyOutput)

processUnstaking PreimageRevealed{..} tx
  | txid tx == expectedUnstakingTxid =
      let newState = Unstaked{unstakingTxid = expectedUnstakingTxid, ..}
          output = StakeTransitionOutput{signal = Just (UnstakedSignal operatorIdx), duty = Nothing}
       in (newState, output)
  | otherwise = error "Unexpected transaction for unstaking"
processUnstaking state _ = (state, emptyOutput)

-- Introspection Functions
isUnstaked :: StakeState -> Bool
isUnstaked Unstaked{} = True
isUnstaked _ = False

getPreimage :: StakeState -> Maybe Preimage
getPreimage PreimageRevealed{..} = Just preimage
getPreimage Unstaked{..} = Just preimage
getPreimage _ = Nothing
