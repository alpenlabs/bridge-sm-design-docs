{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Graph
  ( GraphState
  , GraphDuty
  , GraphSignal
  , GraphTransitionOutput
  , OperatorTable
  , AbortReason
  , processGraphData
  , processAdaptorsVerification
  , processNonces
  , processPartials
  , processAssignment
  , processFulfillment
  , processActivation
  , processClaim
  , processContest
  , processBridgeProof
  , processBridgeProofTimeout
  , processCounterProof
  , processCounterProofAckd
  , processCounterProofNackd
  , processSlash
  , processPayout
  , processPayoutConnectorSpent
  , notifyNewBlock
  , lastProcessedBlock
  ) where

-- Prelude

import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set qualified as Set
import Data.Word (Word32)

type U32 = Word32
type OperatorIdx = U32
type DepositIdx = U32
type BitcoinBlockHeight = U32
type GraphData = String -- placeholder for data required to generate graph
type Transaction = String -- placeholder
type Txid = String -- placeholder
type OutPoint = (Txid, U32)
type Nonce = String -- placeholder
type AggNonce = String -- placeholder
type Sighash = String -- placeholder
type PartialSignature = String -- placeholder
type Signature = String -- placeholder
type BtcDescriptor = String -- placeholder
type Proof = String -- bytestring
type Labels = String -- placeholder for (GC) labels committed in the counterproof
type P2pKey = String -- placeholder for P2P public key
type SchnorrKey = String -- placeholder for Schnorr public key

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation for input outpoints (head :| tail)

-- Parameters
-- NOTE: numbers are arbitrary and expressed in terms of bitcoin blocks
-- payoutTimeout >> all other timeouts
contestTimeout :: U32
contestTimeout = 1008 -- number of blocks within which a contest must be posted after claim (1 week)

proofTimeout :: U32
proofTimeout = 288 -- number of blocks within which a bridge proof must be posted after (2 days)

nackTimeout :: U32
nackTimeout = 1008 -- number of blocks within which all counterproofs must be NACK'd after contest (1 week)

counterProofAckTimeout :: U32
counterProofAckTimeout = 1008 -- number of blocks within which a counterproof must be posted and ACKd (in order to prevent a contested payout)

payoutTimeout :: U32
payoutTimeout = 2016 -- number of blocks within which a contested payout must be posted after a contest (2 weeks)

-- Additional Types
data GraphSummary = GraphSummary
  { claim :: Txid
  , uncontestedPayout :: Txid
  , contest :: Txid
  , contestedPayout :: Txid
  , bridgeProofTimeout :: Txid
  , counterproofs :: Map OperatorIdx Txid
  , counterproofAcks :: Map OperatorIdx Txid
  , counterproofNacks :: Map OperatorIdx Txid
  , slash :: Txid
  }
  deriving (Show, Eq)

summarize :: GraphData -> GraphSummary
summarize _ =
  GraphSummary
    { claim = "claim_txid_placeholder"
    , uncontestedPayout = "uncontested_payout_txid_placeholder"
    , contest = "contest_txid_placeholder"
    , contestedPayout = "contested_payout_txid_placeholder"
    , bridgeProofTimeout = "bridge_proof_timeout_txid_placeholder"
    , counterproofs = mempty
    , counterproofAcks = mempty
    , counterproofNacks = mempty
    , slash = "slash_txid_placeholder"
    } -- Placeholder implementation

data AbortReason
  = UserTakeBack
  | PayoutConnectorSpent
      { spendingTxid :: Txid -- the txid of the transaction that spent the payout connector
      }
  | DepositSpent
  deriving (Show, Eq)

newtype OperatorTable = OperatorTable
  { operators :: Set.Set (OperatorIdx, P2pKey, SchnorrKey)
  }
  deriving (Show, Eq)

opCardinality :: OperatorTable -> Int
opCardinality cfg = Set.size (operators cfg)

povIdx :: OperatorTable -> OperatorIdx
povIdx _cfg = 0 -- placeholder implementation

-- State
-- This represents the state of any pegout graph associated with a particular deposit.
-- Each graph is uniquely identified by the two-tuple (depositIdx, operatorIdx)
data GraphState
  = Created
      { -- Represents a state where a new deposit request has been identified
        depositIdx :: DepositIdx -- the index of the deposit this graph is associated with
      , operatorIdx :: OperatorIdx -- the index of the operator this graph belongs to
      , depositOutPoint :: OutPoint -- the outpoint deposit transaction associated with this contract , outputIndex :: U32 -- the output index within the deposit transaction that is to be used for this pegout (to allow batched deposits)
      , blockHeight :: BitcoinBlockHeight -- the height of the most recent block that this state is aware of
      , drtBlockHeight :: BitcoinBlockHeight -- the block height at which the DRT was confirmed
      }
  | GraphGenerated
      { -- Represents a state where the pegout graph for this deposit and operator has been generated
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , drtBlockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary -- the txids of the generated pegout graph transactions (required for tx filtering)
      }
  | AdaptorsVerified
      { -- Represents a state where all adaptors for this pegout graph have been verified
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , drtBlockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , nonces :: Map OperatorIdx (NonEmpty Nonce) -- nonces from each operator per operator graph (packed/flattened representation)
      }
  | NoncesCollected
      { -- Represents a state where all required nonces for this pegout graph have been collected
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , drtBlockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , aggNonces :: NonEmpty AggNonce -- aggregated nonces (packed/flattened representation)
      , partials :: Map OperatorIdx (NonEmpty PartialSignature) -- partials from each operator
      }
  | GraphSigned
      { -- Represents a state where all required aggregate signatures for this pegout graph have been collected
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , drtBlockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , signatures :: NonEmpty Signature -- final signatures per operator graph (packed/flattened representation)
      }
  | Assigned
      { -- Represents a state where the deposit associated with this pegout graph has been assigned
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , drtBlockHeight :: BitcoinBlockHeight -- needed in case state transitions from `Assigned` back to `GraphSigned` (due to reassignment)
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , signatures :: NonEmpty Signature
      , assignee :: OperatorIdx -- the operator assigned to fulfill the withdrawal
      , deadline :: BitcoinBlockHeight -- the block height by which the withdrawal must be fulfilled
      , recipientDesc :: BtcDescriptor -- the BTC descriptor for the recipient
      }
  | Fulfilled
      { -- Represents a state where the pegout graph has been activated to initiate reimbursement (this is redundant w.r.t. to the DSM's `Fulfilled` state, but is included here in order to preserve relative independence of GSM to recognize faulty claims)
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , fulfillmentTxid :: Txid -- the txid of the fulfillment transaction submitted on chain
      , fulfillmentBlockHeight :: BitcoinBlockHeight -- the block height at which the fulfillment transaction was confirmed
      }
  | Claimed
      { -- Represents a state where the claim transaction has been posted on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , fulfillmentTxid' :: Maybe Txid -- the txid of the fulfillment transaction submitted on chain (if present; could be absent for faulty claims)
      , fulfillmentBlockHeight' :: Maybe BitcoinBlockHeight -- the block height at which the fulfillment transaction was confirmed (if present; could be absent for faulty claims)
      , claimBlockHeight :: BitcoinBlockHeight -- the block height at which the claim transaction was confirmed (required for timeout calculations)
      }
  | Contested
      { -- Represents a state where the contest transaction has been posted on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , fulfillmentTxid' :: Maybe Txid
      , fulfillmentBlockHeight' :: Maybe BitcoinBlockHeight
      , contestBlockHeight :: BitcoinBlockHeight -- the block height at which the contest transaction was confirmed
      }
  | BridgeProofPosted
      { -- Represents a state where the bridge proof transaction has been posted on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , contestBlockHeight :: BitcoinBlockHeight -- needed in case the operator needs to be slashed after contested payout timeout
      , bridgeProofTxid :: Txid -- the txid of the bridge proof transaction submitted on chain
      , bridgeProofBlockHeight :: BitcoinBlockHeight -- the block height at which the bridge proof transaction was confirmed
      , proof :: Proof -- the bridge proof
      }
  | BridgeProofTimedout
      { -- Represents a state where the bridge proof timeout transaction has been posted on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , contestBlockHeight :: BitcoinBlockHeight
      , expectedSlashTxid :: Txid -- the txid of the expected slash transaction (full summary can be discarded)
      , claimTxid :: Txid -- the txid of the claim transaction (required in order to check if the payout connector is spent)
      }
  | CounterProofPosted
      { -- Represents a state where a counterproof transaction has been posted on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , graphData :: GraphData
      , graphSummary :: GraphSummary
      , contestBlockHeight :: BitcoinBlockHeight
      , counterProofsAndConfs :: Map.Map OperatorIdx (Txid, BitcoinBlockHeight) -- the txids of the counterproof transactions submitted on chain along with their confirmation heights
      , counterProofNacks :: Map.Map OperatorIdx Txid -- the txids of the counterproof NACK transactions submitted on chain
      }
  | AllNackd
      { -- Represents a state where all possible counterproof transactions have been NACK'd on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , contestBlockHeight :: BitcoinBlockHeight
      , expectedPayoutTxid :: Txid -- the txid of the expected contested payout transaction (full summary can be discarded)
      , possibleSlashTxid :: Txid -- the txid of the possible slash transaction (this can happen if the operator is not functional/live)
      }
  | Acked
      { -- Represents a state where a counterproof has been ACK'd on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , blockHeight :: BitcoinBlockHeight
      , contestBlockHeight :: BitcoinBlockHeight
      , expectedSlashTxid :: Txid -- the txid of the expected slash transaction (full summary can be discarded)
      , claimTxid :: Txid -- the txid of the claim transaction (required in order to check if the payout connector is spent)
      }
  | Withdrawn
      { -- Represents a state where the deposit output has been spent by either uncontested or contested payout
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , depositOutPoint :: OutPoint
      , payoutTxid :: Txid -- the txid of the transaction (uncontested or contested payout) that spent the deposit output
      }
  | Slashed
      { -- Represents a state where the operator has been slashed on chain
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , slashTxid :: Txid -- the txid of the slash transaction submitted on chain
      }
  | Aborted
      { -- Represents a state where the payout connector has been spent so the graph can be aborted
        -- If the DRT or DT is spent, then the graph is no longer relevant and can simply be deleted
        depositIdx :: DepositIdx
      , operatorIdx :: OperatorIdx
      , reason :: AbortReason -- reason for aborting the graph
      }
  deriving (Show, Eq)

-- Duties
data GraphDuty -- Tasks to be completed post state transition
  = GenerateGraphData -- add fields required to generate a complete graph
  | VerifyAdaptors
      { sighashes :: NonEmpty Sighash -- placeholder for sighashes to verify adaptors against
      }
  | PublishGraphNonces
      { depositIdx :: DepositIdx -- the index of the deposit this graph is associated with (used to seed nonce generation via s2)
      , operatorIdx :: OperatorIdx -- the index of the operator this graph belongs to (used to seed nonce generation via s2)
      }
  | PublishGraphPartials
      { depositIdx :: DepositIdx -- the index of the deposit this graph is associated with (used to retrieve musig2 session via s2)
      , operatorIdx :: OperatorIdx -- the index of the operator this graph belongs to (used to retrieve musig2 session via s2)
      , aggNonces :: NonEmpty AggNonce -- aggregated nonces to be used for partial signature generation
      , sighashes :: NonEmpty Sighash -- sighashes to sign
      , claimTxid :: Txid -- the txid of the claim transaction (since the claim transaction is under complete control of an operator, make sure this transaction does not exist on chain before signing off)
      }
  | PublishClaim
      { claimTx :: Transaction -- the claim transaction to be published
      }
  | PublishUncontestedPayout
      { uncontestedPayoutTx :: Transaction -- the uncontested payout transaction to be published
      }
  | PublishContest
      { contestTx :: Transaction -- the contest transaction to be published
      }
  | PublishBridgeProof
      { depositIdx :: DepositIdx -- the index of the deposit being claimed
      , operatorIdx :: OperatorIdx -- the index of the operator making the claim
      }
  | PublishBridgeProofTimeout
      { timeoutTx :: Transaction -- the bridge proof timeout transaction to be published
      }
  | PublishCounterProof
      { depositIdx :: DepositIdx -- the index of the deposit being claimed
      , operatorIdx :: OperatorIdx -- the index of the operator making the claim
      , contestOutPoint :: OutPoint -- the outpoint of the contest transaction being spent
      , proof :: Proof -- the proof (data) being refuted
      }
  | PublishCounterProofAck
      { counterProofAckTx :: Transaction -- the counterproof ACK transaction to be published
      }
  | PublishCounterProofNack
      { depositIdx :: DepositIdx -- the index of the deposit being claimed
      , counterProverIdx :: OperatorIdx -- the index of the operator making the claim
      , labels :: NonEmpty Labels -- the labels (GC labels) committed in the counterproof
      }
  | PublishSlash
      { slashTx :: Transaction -- the slash transaction to be published
      }
  | PublishContestedPayout
      { contestedPayoutTx :: Transaction -- the contested payout transaction to be published
      }
  deriving (Show, Eq)

-- Signals
-- The messages that need to be propagated across state machines
data GraphSignal
  = GraphAvailable OperatorIdx -- signifies that the graph is fully signed and available for use for unilateral reimbursement
  | OperatorSlashed OperatorIdx -- signifies that the operator has been slashed
  deriving (Show, Eq)

{-# HLINT ignore "Use newtype instead of data" #-}
data DepositSignal
  = CooperativePathFailed DepositIdx -- signifies that the cooperative path for this deposit has failed and unilateral reimbursement must be initiated (triggers the `processActivation` STF)
  deriving (Show, Eq)

-- Output from each state transition
data GraphTransitionOutput = GraphTransitionOutput
  { signal :: Maybe GraphSignal
  , duty :: Maybe GraphDuty
  }
  deriving (Show, Eq)

emptyOutput :: GraphTransitionOutput
emptyOutput =
  GraphTransitionOutput
    { signal = Nothing
    , duty = Nothing
    }

-- State Transition Functions
-- Declarations
processGraphData :: GraphState -> GraphData -> (GraphState, GraphTransitionOutput) -- Created -> GraphGenerated
processAdaptorsVerification :: GraphState -> (GraphState, GraphTransitionOutput) -- GraphGenerated -> AdaptorsVerified
processNonces :: GraphState -> OperatorTable -> (OperatorIdx, NonEmpty Nonce) -> (GraphState, GraphTransitionOutput) -- AdaptorsVerified -> NoncesCollected
processPartials
  :: GraphState -> OperatorTable -> (OperatorIdx, NonEmpty PartialSignature) -> (GraphState, GraphTransitionOutput) -- NoncesCollected -> GraphSigned
processAssignment
  :: GraphState -> OperatorIdx -> BitcoinBlockHeight -> BtcDescriptor -> (GraphState, GraphTransitionOutput) -- GraphSigned/Assigned -> Assigned/GraphSigned
processFulfillment :: GraphState -> Txid -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- Assigned -> Fulfilled
processActivation :: GraphState -> (GraphState, GraphTransitionOutput) -- Fulfilled -> (no state change, triggers unilateral reimbursement process)
processClaim :: GraphState -> Transaction -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- Fulfilled/Assigned/GraphSigned/NoncesCollected -> Claimed
processContest :: GraphState -> Transaction -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- Claimed -> Contested
processBridgeProof
  :: GraphState -> OperatorTable -> Transaction -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- Contested -> BridgeProofPosted
processBridgeProofTimeout :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- Contested -> BridgeProofTimedout
processCounterProof :: GraphState -> Transaction -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- BridgeProofPosted/CounterProofPosted -> CounterProofPosted
processCounterProofAckd :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- CounterProofPosted -> Acked
processCounterProofNackd :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- CounterProofPosted -> AllNackd
processSlash :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- BridgeProofTimedout/Acked -> Slashed
processPayout :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- -> AllNackd/BridgeProofPosted/Claimed -> Withdrawn
processPayoutConnectorSpent :: GraphState -> Transaction -> (GraphState, GraphTransitionOutput) -- [`Claimed`..`CounterProofPosted`] -> Aborted
notifyNewBlock :: GraphState -> OperatorTable -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput) -- \* -> *TimedOut
-- Definitions
processGraphData Created {..} graphData =
  let graphSummary = summarize graphData
  in  ( GraphGenerated
          { graphSummary
          , ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just VerifyAdaptors {sighashes = NonEmpty.fromList ["sighash_placeholder"]} -- Placeholder for sighashes
          }
      )
processGraphData GraphGenerated {} _ = error "Graph data already generated"
processGraphData _ _ = error "Invalid state for graph data"

processAdaptorsVerification GraphGenerated {..} =
  ( AdaptorsVerified
      { nonces = mempty -- start with empty nonces map
      , ..
      }
  , GraphTransitionOutput
      { signal = Nothing
      , duty = Just PublishGraphNonces {depositIdx, operatorIdx}
      }
  )
processAdaptorsVerification AdaptorsVerified {} = error "Adaptors already verified"
processAdaptorsVerification state = error $ "Invalid state for adaptors: " ++ show state

processNonces AdaptorsVerified {..} execConfig (opIdx, receivedNonces) =
  let newNonces =
        if isNothing (Map.lookup opIdx nonces)
          then Map.insert opIdx receivedNonces nonces
          else nonces -- ignore duplicate nonces from same operator
      expectedOperatorCount = opCardinality execConfig
  in  if Map.size newNonces == expectedOperatorCount
        then
          ( NoncesCollected
              { aggNonces = NonEmpty.fromList ["agg_nonce_placeholder"] -- Placeholder for agg nonces
              , partials = mempty -- Placeholder for partial signatures collection
              , ..
              }
          , GraphTransitionOutput
              { signal = Nothing
              , duty =
                  Just
                    PublishGraphPartials
                      { depositIdx
                      , operatorIdx
                      , aggNonces = NonEmpty.fromList ["agg_nonce_placeholder"] -- Placeholder for agg nonces
                      , sighashes = NonEmpty.fromList ["sighash_placeholder"] -- Placeholder for sighashes
                      , claimTxid = claim graphSummary
                      }
              }
          )
        else
          ( AdaptorsVerified
              { nonces = newNonces
              , ..
              }
          , emptyOutput
          )
processNonces NoncesCollected {} _ _ = error "Nonces already collected"
processNonces state _ _ = error $ "Invalid state for nonces: " ++ show state

processPartials NoncesCollected {..} execConfig (opIdx, receivedPartials) =
  let newPartials =
        if isNothing (Map.lookup opIdx partials)
          then Map.insert opIdx receivedPartials partials
          else partials -- ignore duplicate partials from same operator
      expectedOperatorCount = opCardinality execConfig
  in  if Map.size newPartials == expectedOperatorCount
        then
          ( GraphSigned
              { signatures = NonEmpty.fromList ["signature_placeholder"] -- Placeholder for signatures
              , ..
              }
          , GraphTransitionOutput
              { signal = Just (GraphAvailable operatorIdx)
              , duty = Nothing
              }
          )
        else
          ( NoncesCollected
              { partials = newPartials
              , ..
              }
          , emptyOutput
          )
processPartials GraphSigned {} _ _ = error "Graph already signed"
processPartials state _ _ = error $ "Invalid state for partials" ++ show state

processAssignment GraphSigned {..} assignee deadline recipientDesc =
  ( Assigned
      { assignee
      , deadline
      , recipientDesc
      , ..
      }
  , emptyOutput
  )
-- reassignment
processAssignment state@Assigned {..} newAssignee newDeadline newRecipientDesc
  -- same assignee (aka same graph)
  | assignee == newAssignee && (deadline /= newDeadline || recipientDesc /= newRecipientDesc) =
      ( Assigned
          { assignee = newAssignee
          , deadline = newDeadline
          , recipientDesc = newRecipientDesc
          , ..
          }
      , emptyOutput
      )
  -- different assignee (aka different graph, so revert)
  | assignee /= newAssignee =
      ( GraphSigned
          { ..
          }
      , emptyOutput
      )
  | otherwise = (state, emptyOutput)
processAssignment state _ _ _ = error $ "Invalid state for assignment: " ++ show state

processFulfillment Assigned {..} fulfillmentTxid fulfillmentBlockHeight =
  ( Fulfilled
      { ..
      }
  , GraphTransitionOutput
      { signal = Nothing
      , duty = Just PublishClaim {claimTx = "claim_tx_placeholder"} -- Placeholder for claim transaction
      }
  )
processFulfillment Fulfilled {} _ _ = error "Graph already activated"
processFulfillment state _ _ = error $ "Invalid state for activation" ++ show state

processActivation Fulfilled {..} =
  ( Fulfilled
      { ..
      }
  , GraphTransitionOutput
      { signal = Nothing
      , -- Publishing of claim is idempotent so it is fine to create duties multiple times in this state (if neede)
        duty = Just PublishClaim {claimTx = "placeholder_claim_tx"} -- Placeholder for claim transaction
      }
  )
processActivation state = error $ "Invalid state for activation" ++ show state

processClaim Fulfilled {..} tx claimBlockHeight
  | txid tx == claim graphSummary =
      ( Claimed
          { fulfillmentTxid' = Nothing
          , fulfillmentBlockHeight' = Nothing
          , ..
          }
      , emptyOutput
      )
  | otherwise = error "Invalid claim transaction"
processClaim Claimed {} _ _ = error "Graph already claimed"
-- Faulty cases
processClaim Assigned {..} tx claimBlockHeight
  | txid tx == claim graphSummary =
      ( Claimed
          { fulfillmentTxid' = Nothing
          , fulfillmentBlockHeight' = Nothing
          , ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishContest {contestTx = "contest_tx_placeholder"} -- Placeholder for contest transaction
          }
      )
  | otherwise = error "Invalid claim transaction"
processClaim GraphSigned {..} tx claimBlockHeight
  | txid tx == claim graphSummary =
      ( Claimed
          { fulfillmentTxid' = Nothing
          , fulfillmentBlockHeight' = Nothing
          , ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishContest {contestTx = "contest_tx_placeholder"} -- Placeholder for contest transaction
          }
      )
  | otherwise = error "Invalid claim transaction"
-- a malicious operator can withhold their partial signature and then issue a claim
processClaim NoncesCollected {..} tx claimBlockHeight
  | txid tx == claim graphSummary =
      ( Claimed
          { fulfillmentTxid' = Nothing
          , fulfillmentBlockHeight' = Nothing
          , ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishContest {contestTx = "contest_tx_placeholder"} -- Placeholder for contest transaction
          }
      )
  | otherwise = error "Invalid claim transaction"
-- Invalid cases
processClaim state _ _ = error $ "Invalid state for claim: " ++ show state

processContest Claimed {..} tx contestBlockHeight
  | txid tx == contest graphSummary =
      ( Contested
          { ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishBridgeProof {depositIdx, operatorIdx}
          }
      )
  | otherwise = error "Invalid contest transaction"
processContest Contested {} _ _ = error "Graph already contested"
processContest state _ _ = error $ "Invalid state for contest: " ++ show state

processBridgeProof Contested {..} opTable tx bridgeProofBlockHeight
  | (contest graphSummary, 0) `elem` inpoints tx =
      let proof = "proof_placeholder" -- Placeholder for proof (needs to be extracted from tx)
      in  ( BridgeProofPosted
              { bridgeProofTxid = txid tx
              , proof = proof
              , ..
              }
          , GraphTransitionOutput
              { signal = Nothing
              , duty =
                  -- a graph will either produce a bridge proof or a counterproof
                  -- so this duty may be discarded by the executor if the graph belongs to the prover
                  Just
                    PublishCounterProof
                      { depositIdx
                      , operatorIdx
                      , contestOutPoint = (contest graphSummary, 3 + povIdx opTable)
                      , proof = proof
                      }
              }
          )
  | otherwise = error "Invalid bridge proof transaction"
processBridgeProof BridgeProofPosted {} _ _ _ = error "Bridge proof already posted"
processBridgeProof state _ _ _ = error $ "Invalid state for bridge proof: " ++ show state

processBridgeProofTimeout Contested {..} tx
  | txid tx == bridgeProofTimeout graphSummary =
      ( BridgeProofTimedout
          { expectedSlashTxid = slash graphSummary
          , claimTxid = claim graphSummary
          , ..
          }
      , emptyOutput
      )
  | otherwise = error "Invalid bridge proof timeout transaction"
processBridgeProofTimeout BridgeProofTimedout {} _ = error "Bridge proof timeout already processed"
processBridgeProofTimeout state _ = error $ "Invalid state for bridge proof timeout: " ++ show state

processCounterProof BridgeProofPosted {..} tx counterproofBlockHeight =
  case find (\(_opIdx, txid') -> txid' == txid tx) (Map.toList $ counterproofs graphSummary) of
    Just (counterProverIdx, _) ->
      let labels = "labels_placeholder" :| [] -- Placeholder for labels (needs to be extracted from tx)
          duty =
            -- this will be discarded by the executor if counterProverIdx is the same as povIdx
            Just
              PublishCounterProofNack
                { depositIdx = depositIdx
                , counterProverIdx = counterProverIdx
                , labels = labels
                }
          newCounterProofs =
            Map.singleton counterProverIdx (txid tx, counterproofBlockHeight)
      in  ( CounterProofPosted
              { counterProofsAndConfs = newCounterProofs
              , counterProofNacks = mempty
              , ..
              }
          , GraphTransitionOutput
              { signal = Nothing
              , duty = duty
              }
          )
    Nothing -> error "Invalid counterproof transaction"
processCounterProof CounterProofPosted {..} tx counterproofBlockHeight =
  case find (\(_opIdx, txid') -> txid' == txid tx) (Map.toList $ counterproofs graphSummary) of
    Just (counterProverIdx, _) ->
      let isNewCounterProof =
            isNothing (Map.lookup counterProverIdx counterProofsAndConfs)
          newCounterProofs =
            if isNewCounterProof
              then Map.insert counterProverIdx (txid tx, counterproofBlockHeight) counterProofsAndConfs
              else counterProofsAndConfs -- ignore duplicate counterproofs from same operator
          duty =
            if isNewCounterProof
              then
                -- this will be discarded by the executor if counterProverIdx is the same as povIdx
                Just
                  PublishCounterProofNack
                    { depositIdx = depositIdx
                    , counterProverIdx = counterProverIdx
                    , labels = "labels_placeholder" :| [] -- Placeholder for labels (needs to be extracted from tx)
                    }
              else Nothing
      in  ( CounterProofPosted
              { counterProofsAndConfs = newCounterProofs
              , ..
              }
          , GraphTransitionOutput {signal = Nothing, duty = duty}
          )
    Nothing -> error "Invalid counterproof transaction"
processCounterProof state _ _ = error $ "Invalid state for counterproof: " ++ show state

processCounterProofAckd CounterProofPosted {..} tx
  | txid tx `elem` Map.elems (counterproofAcks graphSummary) =
      ( Acked
          { expectedSlashTxid = slash graphSummary
          , claimTxid = claim graphSummary
          , ..
          }
      , emptyOutput
      )
  | otherwise = error "Invalid counterproof ACK transaction"
processCounterProofAckd Acked {} _ = error "Counterproof already ACK'd"
processCounterProofAckd state _ = error $ "Invalid state for counterproof ACK: " ++ show state

processCounterProofNackd CounterProofPosted {..} tx =
  let nackd = find (\(_opIdx, txid') -> txid' == txid tx) (Map.toList $ counterproofNacks graphSummary)
  in  case nackd of
        Just (nackdIdx, _) ->
          let nacks = Map.insert nackdIdx (txid tx) counterProofNacks
              expectedNacks = Map.size (counterproofs graphSummary)
          in  if Map.size nacks == expectedNacks
                then
                  ( AllNackd
                      { expectedPayoutTxid = contestedPayout graphSummary
                      , possibleSlashTxid = slash graphSummary
                      , ..
                      }
                  , emptyOutput
                  )
                else
                  ( CounterProofPosted
                      { counterProofNacks = nacks
                      , ..
                      }
                  , emptyOutput
                  )
        Nothing -> error "Invalid counterproof NACK transaction"
processCounterProofNackd AllNackd {} _ = error "All counterproofs already NACK'd"
processCounterProofNackd state _ = error $ "Invalid state for counterproof NACK: " ++ show state

mkSlashed :: DepositIdx -> OperatorIdx -> Txid -> (GraphState, GraphTransitionOutput)
mkSlashed depositIdx operatorIdx slashTxid =
  ( Slashed
      { slashTxid = slashTxid
      , ..
      }
  , GraphTransitionOutput
      { signal = Just (OperatorSlashed operatorIdx) -- sent to the Operator State Machine
      , duty = Nothing
      }
  )

processSlash BridgeProofTimedout {..} tx
  | txid tx == expectedSlashTxid = mkSlashed depositIdx operatorIdx (txid tx)
  | otherwise = error "Invalid slash transaction"
processSlash Acked {..} tx
  | txid tx == expectedSlashTxid = mkSlashed depositIdx operatorIdx (txid tx)
  | otherwise = error "Invalid slash transaction"
processSlash AllNackd {..} tx
  | txid tx == possibleSlashTxid = mkSlashed depositIdx operatorIdx (txid tx)
  | otherwise = error "Invalid slash transaction"
processSlash Slashed {} _ = error "Operator already slashed"
processSlash state _ = error $ "Invalid state for slash: " ++ show state

mkWithdrawn :: DepositIdx -> OperatorIdx -> Transaction -> (GraphState, GraphTransitionOutput)
mkWithdrawn depositIdx operatorIdx payoutTx =
  ( Withdrawn
      { payoutTxid = txid payoutTx
      , depositOutPoint = NonEmpty.head (inpoints payoutTx) -- first input is the deposit outpoint
      , ..
      }
  , emptyOutput
  )

processPayout Claimed {..} tx
  | txid tx == uncontestedPayout graphSummary = mkWithdrawn depositIdx operatorIdx tx
  | otherwise = error "Invalid uncontested payout transaction"
processPayout BridgeProofPosted {..} tx
  | txid tx == uncontestedPayout graphSummary = mkWithdrawn depositIdx operatorIdx tx
  | otherwise = error "Invalid uncontested payout transaction"
processPayout AllNackd {..} tx
  | txid tx == expectedPayoutTxid = mkWithdrawn depositIdx operatorIdx tx
  | otherwise = error "Invalid contested payout transaction"
processPayout Withdrawn {} _ = error "Deposit already withdrawn"
processPayout state _ = error $ "Invalid state for payout: " ++ show state

-- technically, this can only happen from the `Claimed` state
-- but if the payout connector has been spent, it means we've already reached the `Claimed` state
-- so this allows us to have a simpler definition for this STF
-- also note that this should be checked _after_ the payout checks
-- since payouts also spend this connector
processPayoutConnectorSpent state tx
  | any (`elem` inpoints tx) (getPayoutConnectorOutPoint state) =
      ( Aborted
          { reason = PayoutConnectorSpent {spendingTxid = txid tx}
          , depositIdx = state.depositIdx
          , operatorIdx = state.operatorIdx
          }
      , emptyOutput
      )
  | otherwise = error "Payout connector not spent in the provided transaction"

mkSlashOutput :: GraphState -> GraphTransitionOutput
mkSlashOutput state =
  GraphTransitionOutput
    { signal = Just (OperatorSlashed state.operatorIdx)
    , duty = Just PublishSlash {slashTx = "slash_tx_placeholder"} -- Placeholder for slash transaction
    }

-- check if uncontested payout is possible
notifyNewBlock curState@Claimed {..} _opTable newBlockHeight
  | newBlockHeight > claimBlockHeight + contestTimeout =
      ( curState {blockHeight = newBlockHeight}
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishUncontestedPayout {uncontestedPayoutTx = "uncontested_payout_tx_placeholder"} -- Placeholder for uncontested payout transaction
          }
      )
  | otherwise = (curState {blockHeight = newBlockHeight}, emptyOutput)
-- check if bridge proof timeout (or payout) is possible
notifyNewBlock curState@Contested {..} _opTable newBlockHeight
  | newBlockHeight > contestBlockHeight + payoutTimeout =
      (curState {blockHeight = newBlockHeight}, mkSlashOutput curState) -- Placeholder for slash transaction
  | newBlockHeight > contestBlockHeight + proofTimeout =
      ( curState {blockHeight = newBlockHeight}
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishBridgeProofTimeout {timeoutTx = "bridge_proof_timeout_tx_placeholder"} -- Placeholder for bridge proof timeout transaction
          }
      )
  | otherwise = (curState {blockHeight = newBlockHeight}, emptyOutput)
-- check if ACK is possible
notifyNewBlock curState@CounterProofPosted {..} opTable newBlockHeight
  | newBlockHeight > contestBlockHeight + payoutTimeout =
      (curState {blockHeight = newBlockHeight}, mkSlashOutput curState) -- Placeholder for slash transaction
      -- if no one ACK's their counterproof, the operator could still get a payout (even without NACK-ing)
  | newBlockHeight > contestBlockHeight + counterProofAckTimeout =
      ( curState {blockHeight = newBlockHeight}
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just PublishContestedPayout {contestedPayoutTx = "contested_payout_tx_placeholder"} -- Placeholder for contested payout transaction
          }
      )
  | otherwise =
      let povCounterProof = Map.lookup (povIdx opTable) counterProofsAndConfs
          isAckViable = isJust povCounterProof && snd (fromJust povCounterProof) + nackTimeout > newBlockHeight
      in  if isAckViable
            then
              ( curState {blockHeight = newBlockHeight}
              , GraphTransitionOutput
                  { signal = Nothing
                  , duty = Just PublishCounterProofAck {counterProofAckTx = "counterproof_ack_tx_placeholder"} -- Placeholder for counterproof ACK transaction
                  }
              )
            else (curState {blockHeight = newBlockHeight}, emptyOutput)
-- check if slashing/payout is possible for all other cases
notifyNewBlock curState _opTable newBlockHeight = case curState of
  BridgeProofPosted {..}
    | newBlockHeight > contestBlockHeight + counterProofAckTimeout ->
        ( curState
        , GraphTransitionOutput
            { signal = Nothing
            , duty = Just PublishContestedPayout {contestedPayoutTx = "contested_payout_tx_placeholder"} -- Placeholder for contested payout transaction
            }
        )
    | newBlockHeight > contestBlockHeight + payoutTimeout ->
        (curState {blockHeight = newBlockHeight}, mkSlashOutput curState)
  BridgeProofTimedout {..}
    | newBlockHeight > contestBlockHeight + payoutTimeout ->
        (curState {blockHeight = newBlockHeight}, mkSlashOutput curState)
  Acked {..}
    | newBlockHeight > contestBlockHeight + payoutTimeout ->
        (curState {blockHeight = newBlockHeight}, mkSlashOutput curState)
  AllNackd {..}
    | newBlockHeight > contestBlockHeight + counterProofAckTimeout ->
        ( curState {blockHeight = newBlockHeight}
        , GraphTransitionOutput
            { signal = Nothing
            , duty = Just PublishContestedPayout {contestedPayoutTx = "contested_payout_tx_placeholder"} -- Placeholder for contested payout transaction
            }
        )
    | newBlockHeight > contestBlockHeight + payoutTimeout ->
        (curState {blockHeight = newBlockHeight}, mkSlashOutput curState)
  -- The next three states should not need any further updates
  Slashed {} -> (curState, emptyOutput)
  Withdrawn {} -> (curState, emptyOutput)
  Aborted {} -> (curState, emptyOutput)
  _ -> (curState {blockHeight = newBlockHeight}, emptyOutput)

-- Introspection Functions
payoutConnectorIdx :: U32
payoutConnectorIdx = 1

getPayoutConnectorOutPoint :: GraphState -> Maybe OutPoint
getPayoutConnectorOutPoint state =
  let claimTxid' = case state of
        Created {} -> Nothing
        GraphGenerated {..} -> Just graphSummary.claim
        AdaptorsVerified {..} -> Just graphSummary.claim
        NoncesCollected {..} -> Just graphSummary.claim
        GraphSigned {..} -> Just graphSummary.claim
        Assigned {..} -> Just graphSummary.claim
        Fulfilled {..} -> Just graphSummary.claim
        Claimed {..} -> Just graphSummary.claim
        Contested {..} -> Just graphSummary.claim
        BridgeProofPosted {..} -> Just graphSummary.claim
        BridgeProofTimedout {..} -> Just claimTxid
        Acked {..} -> Just claimTxid
        CounterProofPosted {..} -> Just graphSummary.claim
        AllNackd {} -> Nothing
        Withdrawn {} -> Nothing
        Slashed {} -> Nothing
        Aborted {} -> Nothing
      outpoint =
        if isJust claimTxid'
          then Just (fromJust claimTxid', payoutConnectorIdx)
          else Nothing
  in  outpoint

lastProcessedBlock :: GraphState -> Maybe BitcoinBlockHeight
lastProcessedBlock state = case state of
  Withdrawn {} -> Nothing
  Slashed {} -> Nothing
  Aborted {} -> Nothing
  otherState -> Just otherState.blockHeight
