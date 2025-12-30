{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Graph (GraphState) where

-- Prelude

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Word (Word32)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import qualified Data.List.NonEmpty as NonEmpty

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
type PartialSignature = String -- placeholder
type Signature = String -- placeholder
type BtcDescriptor = String -- placeholder
type Proof = String  -- bytestring
type P2pKey = String -- placeholder for P2P public key
type SchnorrKey = String -- placeholder for Schnorr public key

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation for input outpoints (head :| tail)

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
summarize _ = GraphSummary
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

data AbortReason =
  UserTakeBack
  | PayoutConnectorSpent
  | DepositSpent
  deriving (Show, Eq)

newtype ExecConfig = ExecConfig
    { operators :: Set.Set (OperatorIdx, P2pKey, SchnorrKey)
    }
    deriving (Show, Eq)

-- Helpers
opCardinality :: ExecConfig -> Int
opCardinality cfg = Set.size (operators cfg)

-- State
-- This represents the state of any pegout graph associated with a particular deposit.
-- Each graph is uniquely identified by the two-tuple (depositIdx, operatorIdx)
data GraphState =
  Created { -- Represents a state where a new deposit request has been identified
    depositIdx :: DepositIdx -- the index of the deposit this graph is associated with
    , operatorIdx :: OperatorIdx -- the index of the operator this graph belongs to
    , depositOutPoint :: OutPoint -- the outpoint deposit transaction associated with this contract , outputIndex :: U32 -- the output index within the deposit transaction that is to be used for this pegout (to allow batched deposits)
    , drtBlockHeight :: BitcoinBlockHeight -- the block height at which the DRT was confirmed
  }
  | GraphGenerated  { -- Represents a state where the pegout graph for this deposit and operator has been generated
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary  -- the txids of the generated pegout graph transactions (required for tx filtering)
  }
  | AdaptorsVerified { -- Represents a state where all adaptors for this pegout graph have been verified
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , nonces :: Map OperatorIdx (NonEmpty Nonce) -- nonces from each operator per operator graph (packed/flattened representation)
  }
  | NoncesCollected { -- Represents a state where all required nonces for this pegout graph have been collected
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , aggNonces :: NonEmpty AggNonce -- aggregated nonces (packed/flattened representation)
    , partials :: Map OperatorIdx (NonEmpty PartialSignature) -- partials from each operator
  }
  | GraphSigned { -- Represents a state where all required aggregate signatures for this pegout graph have been collected
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , signatures :: NonEmpty Signature -- final signatures per operator graph (packed/flattened representation)
  }
  | Assigned { -- Represents a state where the deposit associated with this pegout graph has been assigned
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , signatures :: NonEmpty Signature
    , assignee :: OperatorIdx -- the operator assigned to fulfill the withdrawal
    , deadline :: BitcoinBlockHeight -- the block height by which the withdrawal must be fulfilled
    , recipientDesc :: BtcDescriptor -- the BTC descriptor for the recipient
  }
  | Activated { -- Represents a state where the pegout graph has been activated to initiate reimbursement
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , fulfillmentTxid :: Txid -- the txid of the fulfillment transaction submitted on chain
    , fulfillmentBlockHeight :: BitcoinBlockHeight -- the block height at which the fulfillment transaction was confirmed
  }
  | Claimed { -- Represents a state where the claim transaction has been posted on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , fulfillmentTxid' :: Maybe Txid -- the txid of the fulfillment transaction submitted on chain (if present; could be absent for faulty claims)
    , fulfillmentBlockHeight' :: Maybe BitcoinBlockHeight -- the block height at which the fulfillment transaction was confirmed (if present; could be absent for faulty claims)
    , claimBlockHeight :: BitcoinBlockHeight -- the block height at which the claim transaction was confirmed (required for timeout calculations)
  }
  | Contested { -- Represents a state where the contest transaction has been posted on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , fulfillmentTxid :: Txid
    , fulfillmentBlockHeight' :: Maybe BitcoinBlockHeight
    , claimBlockHeight' :: Maybe BitcoinBlockHeight
    , contestBlockHeight :: BitcoinBlockHeight -- the block height at which the contest transaction was confirmed
  }
  | BridgeProofPosted { -- Represents a state where the bridge proof transaction has been posted on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , bridgeProofTxid :: Txid -- the txid of the bridge proof transaction submitted on chain
    , bridgeProofBlockHeight :: BitcoinBlockHeight -- the block height at which the bridge proof transaction was confirmed
    , proof :: Proof -- the bridge proof
  }
  | BridgeProofTimedout { -- Represents a state where the bridge proof timeout transaction has been posted on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , depositOutPoint :: OutPoint
    , drtBlockHeight :: BitcoinBlockHeight
    , expectedSlashTxid :: Txid -- the txid of the expected slash transaction (full summary can be discarded)
  }
  | CounterProofPosted {  -- Represents a state where a counterproof transaction has been posted on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , graphData :: GraphData
    , graphSummary :: GraphSummary
    , counterproofTxids :: NonEmpty (Txid, BitcoinBlockHeight) -- the txids of the counterproof transactions submitted on chain along with their confirmation heights
  }
  | AllNacked { -- Represents a state where all possible counterproof transactions have been NACK'd on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , outputIndex :: U32
    , expectedPayoutTxid :: Txid -- the txid of the expected contested payout transaction (full summary can be discarded)
  }
  | Acked { -- Represents a state where a counterproof has been ACK'd on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , outputIndex :: U32
    , expectedSlashTxid :: Txid -- the txid of the expected slash transaction (full summary can be discarded)
  }
  | Withdrawn { -- Represents a state where the deposit output has been spent by either uncontested or contested payout
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , outputIndex :: U32
    , graphData :: GraphData
    , payoutTxid :: Txid -- the txid of the transaction (uncontested or contested payout) that spent the deposit output
  }
  | Slashed { -- Represents a state where the operator has been slashed on chain
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , slashTxid :: Txid -- the txid of the slash transaction submitted on chain 
  }
  | Aborted {
    depositIdx :: DepositIdx
    , operatorIdx :: OperatorIdx
    , reason :: AbortReason -- reason for aborting the graph
  }
  deriving (Show, Eq)

-- Duties
data GraphDuty -- Tasks to be completed post state transition
  = GenerateGraphData
  | VerifyAdaptors
  | PublishGraphNonces
  | PublishGraphPartials
  | PublishClaim
  | PublishUncontestedPayout
  | PublishContest
  | PublishBridgeProof
  | PublishBridgeProofTimeout
  | PublishCounterProof
  | PublishCounterProofAck
  | PublishCounterProofNack
  | PublishSlash
  | PublishContestedPayout
  deriving (Show, Eq)

-- Signals
-- The messages that need to be propagated across state machines
{-# HLINT ignore "Use newtype instead of data" #-}
data GraphSignal
  = GraphAvailable OperatorIdx
  deriving (Show, Eq)

-- Output from each state transition
data GraphTransitionOutput = GraphTransitionOutput
  { signal :: Maybe GraphSignal
  , duty :: Maybe GraphDuty
  }
  deriving (Show, Eq)

emptyOutput :: GraphTransitionOutput
emptyOutput = GraphTransitionOutput
  { signal = Nothing
  , duty = Nothing
  }

-- State Transition Functions
processGraphData :: GraphState -> GraphData -> (GraphState, GraphTransitionOutput)
processAdaptorsVerification :: GraphState -> (GraphState, GraphTransitionOutput)
processNonces :: GraphState -> ExecConfig -> (OperatorIdx, NonEmpty Nonce) -> (GraphState, GraphTransitionOutput)
processPartials :: GraphState -> ExecConfig -> (OperatorIdx, NonEmpty PartialSignature) -> (GraphState, GraphTransitionOutput)
processAssignment :: GraphState -> OperatorIdx -> BitcoinBlockHeight -> BtcDescriptor -> (GraphState, GraphTransitionOutput)
processActivation :: GraphState -> Txid -> BitcoinBlockHeight -> (GraphState, GraphTransitionOutput)

processGraphData Created {..} graphData
  = let graphSummary = summarize graphData
    in ( GraphGenerated {
          graphSummary
          , ..
          }
      , GraphTransitionOutput
          { signal = Nothing
          , duty = Just VerifyAdaptors
          }
      )
processGraphData GraphGenerated {} _ = error "Graph data already generated"
processGraphData _ _ = error "Invalid state for graph data"

processAdaptorsVerification  GraphGenerated { .. }
  = ( AdaptorsVerified {
          nonces = mempty -- start with empty nonces map
          , ..
      }
    , GraphTransitionOutput
        { signal = Nothing
        , duty = Just PublishGraphNonces
        }
    )
processAdaptorsVerification AdaptorsVerified {} = error "Adaptors already verified"
processAdaptorsVerification state = error $ "Invalid state for adaptors: " ++ show state

processNonces  AdaptorsVerified { .. } execConfig (opIdx, receivedNonces)
  = let newNonces = if isNothing (Map.lookup opIdx nonces)
                   then Map.insert opIdx receivedNonces nonces
                   else nonces -- ignore duplicate nonces from same operator
        expectedOperatorCount = opCardinality execConfig
    in if Map.size newNonces == expectedOperatorCount
         then (NoncesCollected {
                  aggNonces = NonEmpty.fromList ["agg_nonce_placeholder"] -- Placeholder for agg nonces
                  , partials = mempty -- Placeholder for partial signatures collection
                  , ..
                }
              , GraphTransitionOutput
                  { signal = Nothing
                  , duty = Just PublishGraphPartials
                  }
              )
         else
              (AdaptorsVerified {
                    nonces = newNonces
                    , ..
                  }
                , emptyOutput
              )
processNonces NoncesCollected {} _ _ = error "Nonces already collected"
processNonces state _ _ = error $ "Invalid state for nonces: " ++ show state

processPartials NoncesCollected { .. } execConfig (opIdx, receivedPartials)
  = let newPartials = if isNothing (Map.lookup opIdx partials)
                      then Map.insert opIdx receivedPartials partials
                      else partials -- ignore duplicate partials from same operator
        expectedOperatorCount = opCardinality execConfig
    in if Map.size newPartials == expectedOperatorCount
         then (GraphSigned {
                  signatures = NonEmpty.fromList ["signature_placeholder"] -- Placeholder for signatures
                  , ..
                }
              , GraphTransitionOutput
                  { signal = Just (GraphAvailable operatorIdx)
                  , duty = Nothing
                  }
              )
         else
              (NoncesCollected {
                    partials = newPartials
                    , ..
                  }
                , emptyOutput
              )
processPartials GraphSigned {} _ _ = error "Graph already signed"
processPartials state _ _ = error $ "Invalid state for partials" ++ show state

processAssignment GraphSigned { .. } assignee deadline recipientDesc
  = (Assigned {
        assignee
        , deadline
        , recipientDesc
        , ..
      }
    , GraphTransitionOutput
        { signal = Nothing
        , duty = Nothing
        }
    )
-- reassignment
processAssignment state@Assigned { .. } newAssignee newDeadline newRecipientDesc
  -- same assignee (aka same graph)
  | assignee == newAssignee && (deadline /= newDeadline || recipientDesc /= newRecipientDesc)
            = (Assigned {
                assignee = newAssignee
                , deadline = newDeadline
                , recipientDesc = newRecipientDesc
                , ..
              }
            , emptyOutput
            )
  -- different assignee (aka different graph, so revert)
  | assignee /= newAssignee
            = (GraphSigned {
                ..
              }
            , emptyOutput
            )
  | otherwise = (state, emptyOutput)
processAssignment state _ _ _ = error $ "Invalid state for assignment: " ++ show state

processActivation Assigned { .. } fulfillmentTxid fulfillmentBlockHeight
  = (Activated {
        ..
      }
    , GraphTransitionOutput
        { signal = Nothing
        , duty = Just PublishClaim
        }
    )
processActivation Activated {} _ _ = error "Graph already activated"
processActivation state _ _ = error $ "Invalid state for activation" ++ show state


-- Introspection Functions
-- TODO: Implement introspection functions for GraphState
