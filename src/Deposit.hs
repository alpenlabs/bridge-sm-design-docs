{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Deposit (
    DepositState,
    DepositDuty,
    processPartial,
    processDepositConfirmation,
    processAssignment,
    processFulfillment,
    processPayoutNonce,
    processPayoutPartial,
    notifyNewBlock,
    processDepositSpend,
    hasCooperativePayoutFailed,
) where

-- Prelude

import Data.List.NonEmpty (NonEmpty ((:|)), head)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)

type U32 = Word32
type OperatorIdx = U32
type DepositIdx = U32
type BitcoinBlockHeight = U32
type Transaction = String -- placeholder
type Txid = String -- placeholder
type OutPoint = (Txid, U32)
type PubNonce = String -- placeholder
type AggNonce = String -- placeholder
type PartialSignature = String -- placeholder
type Signature = String -- placeholder
type BtcDescriptor = String -- placeholder
type ExecEnvDescriptor = String -- placeholder
type Sighash = String -- placeholder
type P2pKey = String -- placeholder
type SchnorrKey = String -- placeholder
type AdaptorKey = String -- placeholder

newtype DepositTx = DepositTx
    { tx :: Transaction -- this is derived from a DRT
    }
    deriving (Show, Eq)

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

sighashes :: DepositTx -> NonEmpty Sighash
sighashes _ = "sighash_placeholder" :| [] -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation for input outpoints (head :| tail)

-- State
-- This state tracks the lifecycle of a deposit UTXO.
data DepositState
    = Created -- State machine initialized, also tracks graph generations
        { depositIdx :: U32
        , depositTransaction :: DepositTx
        , drtBlockHeight :: BitcoinBlockHeight -- block height where the DRT was confirmed
        , depositRequestOutPoint :: OutPoint -- used to track the deposit request UTXO in case of aborts
        , outputIndex :: U32 -- index of the deposit output corresponding to the `depositIdx` (in case we aggregate multiple deposits into one transaction)
        , blockHeight :: U32 -- the last block height observed by this state machine
        , linkedGraphs :: Set.Set OperatorIdx -- operators whose graphs have been generated to spend this deposit
        }
    | GraphGenerated -- All operators' graphs have been generated and linked to this deposit, also tracks DT pubnonces
        { depositIdx :: U32
        , depositTransaction :: DepositTx
        , drtBlockHeight :: BitcoinBlockHeight
        , depositRequestOutPoint :: OutPoint
        , outputIndex :: U32
        , blockHeight :: U32
        , pubnonces :: Map.Map OperatorIdx PubNonce -- pubnonces required to sign the deposit transaction (per operator)
        }
    | DepositNoncesCollected -- All deposit pubnonces have been collected
        { depositIdx :: U32
        , depositTransaction :: DepositTx
        , drtBlockHeight :: BitcoinBlockHeight
        , depositRequestOutPoint :: OutPoint
        , outputIndex :: U32
        , blockHeight :: U32
        , aggNonce :: AggNonce -- aggregated nonce for signing the deposit transaction
        , partialSignatures :: Map.Map OperatorIdx PartialSignature -- partial signatures per operator for signing the deposit transaction
        }
    | DepositPartialsCollected -- All deposit partial signatures have been collected
        { depositIdx :: U32
        , outputIndex :: U32
        , blockHeight :: U32
        , drtBlockHeight :: BitcoinBlockHeight
        , depositRequestOutPoint :: OutPoint
        , depositTransaction :: DepositTx
        , aggSignature :: Signature -- aggregated signature for the deposit transaction
        }
    | Deposited -- Deposit transaction confirmed on-chain
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint -- outpoint of the confirmed deposit UTXO that will be used for reimbursing operators
        }
    | Assigned -- Withdrawal assigned to an operator for this deposit
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , assignee :: OperatorIdx -- operator assigned to front the user
        , deadline :: BitcoinBlockHeight -- block height by which the operator must fulfill the withdrawal
        , recipientDesc :: BtcDescriptor -- the user's descriptor where funds are to be sent by the operator
        }
    | Fulfilled -- Operator has fronted the user
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , assignee :: OperatorIdx
        , fulfillmentTxid :: Txid -- txid of the fulfillment transaction (fronting the user)
        , fulfillmentBlockHeight :: BitcoinBlockHeight -- block height where the fulfillment transaction was confirmed
        , cooperativePayoutDeadline :: BitcoinBlockHeight -- block height by which the cooperative payout must be completed
        , operatorDesc :: Maybe BtcDescriptor -- the output descriptor of the operator for the cooperative payout (needs to be provided by the recipient, only set once)
        , payoutNonces :: Map.Map OperatorIdx PubNonce -- pubnonces required to sign the cooperative payout transaction (per operator)
        }
    | PayoutNoncesCollected -- All pubnonces have been collected for cooperative payout
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , assignee :: OperatorIdx -- required to pass onto the `CooperativeFailed` state from where a graph can be assigned
        , fulfillmentTxid :: Txid -- do --
        , fulfillmentBlockHeight :: BitcoinBlockHeight -- do --
        , payoutOutputDesc :: BtcDescriptor
        , cooperativePayoutDeadline :: BitcoinBlockHeight
        , payoutAggNonce :: AggNonce -- aggregated nonce for signing the cooperative payout transaction
        , payoutPartialSignatures :: Map.Map OperatorIdx PartialSignature -- partial signatures per operator for signing the cooperative payout transaction
        }
    | PayoutPartialsCollected -- All partial signatures have been collected for cooperative payout
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , payoutTxid :: Txid
        , payoutAggSignature :: Signature -- aggregated signature for the cooperative payout transaction
        }
    | CooperativePathFailed -- Cooperative payout path could not succeed in time
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , assignee :: OperatorIdx
        , fulfillmentTxid :: Txid
        , fulfillmentBlockHeight :: BitcoinBlockHeight
        }
    | Spent -- Deposit has been spent on-chain
        { depositIdx :: U32
        , blockHeight :: U32
        , depositOutPoint :: OutPoint
        , payoutTxid :: Txid -- txid of the transaction that spent the deposit, might not be the same as the Cooperative payout txid
        }
    | Aborted -- Deposit Request UTXO was taken by the depositor
        { depositIdx :: U32 -- the index of the deposit that was aborted
        }
    deriving (Show, Eq)

-- Duties
-- Tasks that any operator (signer) has to perform for this deposit (from creation to spend)
data DepositDuty
    = PublishDepositNonces -- publish this operator's nonce for spending the drt
        { depositOutPoint :: OutPoint -- DRT outpoint to ID the signing session
        }
    | PublishDepositPartials -- publish this operator's partial signature for spending the drt
        { depositOutPoint :: OutPoint -- DRT outpoint to resume the earlier signing session
        , depositSighash :: Sighash -- sighash to be signed for the deposit transaction
        , depositAggNonce :: AggNonce
        }
    | PublishDeposit -- publish the deposit transaction to the Bitcoin network
        { depositTx :: DepositTx
        , aggSignature :: Signature
        }
    | FulfillWithdrawal -- front the user by sending funds to the provided descriptor within the given deadline
        { depositIdx :: DepositIdx
        , deadline :: BitcoinBlockHeight
        , recipientDesc :: BtcDescriptor -- the user's descriptor where funds are to be sent by the operator
        }
    | -- request pubnonces from *all* operators for cooperative payout (this duty execution will generate the operator's descriptor which will then be stored in state)
      -- only the assignee creates this duty
      -- the assignee will also request *themselves* since getting a new descriptor from a wallet is a side-effect and has to be done inside a duty context
      RequestPayoutNonce
        { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
        , depositIdx :: DepositIdx
        }
    | PublishPayoutNonce -- publish the nonce for spending the deposit utxo cooperatively
        { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
        , operatorIdx :: OperatorIdx -- the index of the operator requesting cooperation for payout (could be the same as this operator)
        , operatorDesc :: BtcDescriptor -- descriptor of the operator to receive payout
        }
    | -- request partial signatures from *all* operators for cooperative payout
      -- this technically does not require a request since the operators can just publish their partials when they aggregate pubnonces
      -- however, it is cleaner to have the assignee drive all actions in the cooperative payout path
      -- and have each operator respond via a dedicated 1:1 channel
      RequestPayoutPartial
        { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
        , depositIdx :: DepositIdx
        }
    | PublishPayoutPartial -- publish the partial signature for spending the deposit utxo cooperatively
        { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
        , depositIdx :: DepositIdx
        , aggNonce :: AggNonce
        }
    | PublishPayout -- publish the cooperative payout transaction to the Bitcoin network
        { payoutTx :: Transaction
        }
    deriving (Show, Eq)

-- Additional Types
newtype ExecConfig = ExecConfig
    { operators :: Set.Set (OperatorIdx, P2pKey, SchnorrKey, AdaptorKey)
    }
    deriving (Show, Eq)

-- Helpers
opCardinality :: ExecConfig -> Int
opCardinality cfg = Set.size (operators cfg)

povIdx :: ExecConfig -> OperatorIdx
povIdx _cfg = 0 -- placeholder implementation

getFulfillWithdrawalDuty :: DepositState -> ExecConfig -> Maybe DepositDuty
getFulfillWithdrawalDuty Assigned{..} cfg =
    if povIdx cfg == assignee
        then
            Just
                FulfillWithdrawal
                    { depositIdx = depositIdx
                    , deadline = deadline
                    , recipientDesc = recipientDesc
                    }
        else Nothing
getFulfillWithdrawalDuty _ _ = Nothing

-- Numeric constants (params)
-- abort window
abortWindow :: BitcoinBlockHeight
abortWindow = 1008 -- e.g., 1 week assuming 10 min blocks

-- cooperative payout window
cooperativePayoutWindow :: BitcoinBlockHeight
cooperativePayoutWindow = 2016 -- e.g., ~2 weeks assuming 10 min blocks

-- STFs
-- Definitions
processGraphGenerated :: DepositState -> ExecConfig -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processNonce :: DepositState -> ExecConfig -> PubNonce -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processPartial :: DepositState -> ExecConfig -> PartialSignature -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processDepositConfirmation :: DepositState -> Transaction -> DepositState
processAssignment :: DepositState -> ExecConfig -> OperatorIdx -> BitcoinBlockHeight -> BtcDescriptor -> (DepositState, Maybe DepositDuty)
processFulfillment :: DepositState -> ExecConfig -> Transaction -> BitcoinBlockHeight -> (DepositState, Maybe DepositDuty)
processPayoutNonce :: DepositState -> ExecConfig -> PubNonce -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processPayoutPartial :: DepositState -> ExecConfig -> PartialSignature -> OperatorIdx -> (DepositState, Maybe DepositDuty)
notifyNewBlock :: BitcoinBlockHeight -> DepositState -> DepositState
processDepositSpend :: DepositState -> Transaction -> DepositState
-- Implementations
processGraphGenerated deposit@Created{..} _cfg operatorIdx =
    let linkedGraphs' = linkedGraphs `Set.union` Set.singleton operatorIdx
        newState =
            if Set.size linkedGraphs' == opCardinality _cfg
                then
                    GraphGenerated
                        { pubnonces = mempty
                        , ..
                        }
                else
                    deposit{linkedGraphs = linkedGraphs'}
     in (newState, Nothing)
processGraphGenerated state _ _ = (state, Nothing) -- do nothing if the state has already progressed

processNonce deposit@GraphGenerated{..} cfg nonce operatorIdx =
    let newNonces = Map.insert operatorIdx nonce pubnonces
        (newState, duty) =
            if Map.size newNonces == opCardinality cfg
                then
                    let aggNonce = "agg_nonce_placeholder" -- Placeholder for actual aggregation logic
                        newState' =
                            DepositNoncesCollected
                                { aggNonce = aggNonce
                                , partialSignatures = mempty
                                , ..
                                }
                        duty' =
                            Just
                                PublishDepositPartials
                                    { depositOutPoint = Data.List.NonEmpty.head $ inpoints $ tx depositTransaction
                                    , depositSighash = Data.List.NonEmpty.head $ sighashes depositTransaction
                                    , depositAggNonce = aggNonce
                                    }
                     in (newState', duty')
                else
                    (deposit{pubnonces = newNonces}, Nothing)
     in (newState, duty)
processNonce _ _ _ _ = error "Invalid state transition"

processPartial deposit@DepositNoncesCollected{..} cfg partialSig operatorIdx =
    let newPartials = Map.insert operatorIdx partialSig partialSignatures
        (newState, duty) =
            if Map.size newPartials == opCardinality cfg
                then
                    let aggSignature = "agg_signature_placeholder" -- Placeholder for actual aggregation logic
                        newState' =
                            DepositPartialsCollected
                                { aggSignature = aggSignature
                                , ..
                                }
                        duty' = Just PublishDeposit{depositTx = depositTransaction, aggSignature = aggSignature}
                     in (newState', duty')
                else
                    (deposit{partialSignatures = newPartials}, Nothing)
     in (newState, duty)
processPartial _ _ _ _ = error "Invalid state transition"

processDepositConfirmation DepositPartialsCollected{..} confirmedTx =
    let newState =
            if txid confirmedTx == txid (tx depositTransaction)
                then
                    Deposited
                        { depositOutPoint = (txid confirmedTx, outputIndex)
                        , ..
                        }
                else error "Transaction confirmation does not match expected deposit transaction"
     in newState
processDepositConfirmation _ _ = error "Invalid state transition"

processAssignment Deposited{..} cfg assignee deadline recipientDesc =
    let newState =
            Assigned
                { assignee = assignee
                , deadline = deadline
                , recipientDesc = recipientDesc
                , ..
                }
        duty = getFulfillWithdrawalDuty newState cfg
     in (newState, duty)
processAssignment deposit@Assigned{} cfg assignee' deadline' recipientDesc' =
    let newState =
            deposit
                { assignee = assignee'
                , deadline = deadline'
                , recipientDesc = recipientDesc'
                }
        duty = getFulfillWithdrawalDuty newState cfg
     in (newState, duty)
processAssignment _ _ _ _ _ = error "Invalid state transition"

processFulfillment Assigned{..} cfg fulfillmentTx fulfillmentBlockHeight =
    let newState =
            Fulfilled
                { fulfillmentTxid = txid fulfillmentTx
                , fulfillmentBlockHeight = fulfillmentBlockHeight
                , cooperativePayoutDeadline = fulfillmentBlockHeight + cooperativePayoutWindow
                , operatorDesc = Nothing -- to be set when payout pubnonces are requested and a descriptor is provided in the request
                , payoutNonces = mempty
                , ..
                }
        duty =
            if assignee == povIdx cfg
                then Just RequestPayoutNonce{depositOutPoint = depositOutPoint, depositIdx = depositIdx}
                else Nothing
     in (newState, duty)
processFulfillment _ _ _ _ = error "Invalid state transition"

processPayoutNonce deposit@Fulfilled{..} cfg nonce operatorIdx =
    let newPayoutNonces = Map.insert operatorIdx nonce payoutNonces
        (newState, duty) =
            if Map.size newPayoutNonces == opCardinality cfg
                then
                    let aggNonce = "payout_agg_nonce_placeholder" -- Placeholder for actual aggregation logic
                        newState' =
                            PayoutNoncesCollected
                                { payoutAggNonce = aggNonce
                                , payoutPartialSignatures = mempty
                                , payoutOutputDesc = case operatorDesc of
                                    Just desc -> desc
                                    Nothing -> error "Operator descriptor must be set before collecting payout pubnonces"
                                , ..
                                }
                        duty' = Just PublishPayoutPartial{depositOutPoint = depositOutPoint, depositIdx = depositIdx, aggNonce = aggNonce}
                     in (newState', duty')
                else
                    (deposit{payoutNonces = newPayoutNonces}, Nothing)
     in (newState, duty)
processPayoutNonce _ _ _ _ = error "Invalid state transition"

processPayoutPartial deposit@PayoutNoncesCollected{..} cfg partialSig operatorIdx =
    let newPartials = Map.insert operatorIdx partialSig payoutPartialSignatures
        (newState, duty) =
            if Map.size newPartials == opCardinality cfg
                then
                    let aggSignature = "payout_agg_signature_placeholder" -- Placeholder for actual aggregation logic;
                    -- Placeholder for actual payout transaction id
                    -- this is needed for book-keeping purposes so that any spend of the deposit
                    -- can be associated with the expected payout in the cooperative path.
                        payoutTxid = "payout_txid_placeholder"
                        newState' =
                            PayoutPartialsCollected
                                { payoutAggSignature = aggSignature
                                , payoutTxid = payoutTxid
                                , ..
                                }
                        duty' =
                            Just
                                PublishPayout
                                    { payoutTx = "payout_transaction_placeholder" -- Placeholder for actual payout transaction
                                    , ..
                                    }
                     in (newState', duty')
                else
                    (deposit{payoutPartialSignatures = newPartials}, Nothing)
     in (newState, duty)
processPayoutPartial _ _ _ _ = error "Invalid state transition"

-- Processes information about new blocks and applies any updates related to block height timeouts
notifyNewBlock height Created{..}
    | height > drtBlockHeight + abortWindow =
        Aborted{..}
notifyNewBlock height GraphGenerated{..}
    | height > drtBlockHeight + abortWindow =
        Aborted{..}
notifyNewBlock height DepositNoncesCollected{..}
    | height > drtBlockHeight + abortWindow =
        Aborted{..}
notifyNewBlock height DepositPartialsCollected{..}
    | height > drtBlockHeight + abortWindow =
        Aborted{..}
notifyNewBlock height Fulfilled{..}
    | height > fulfillmentBlockHeight + cooperativePayoutWindow =
        CooperativePathFailed{..}
notifyNewBlock height PayoutNoncesCollected{..}
    | height > fulfillmentBlockHeight + cooperativePayoutWindow =
        CooperativePathFailed{..}
notifyNewBlock _ state@Aborted{} = state
notifyNewBlock height state = state{blockHeight = height}

-- Handles both PayoutPartialsCollected and PayoutNoncesCollected states
-- It is technically possible to see a spend before all the payout partials have been collected
-- this can happen if the assignee withholds their partial signature
-- and just broadcasts the cooperative payout tx directly.
processDepositSpend state confirmedTx = case state of
    PayoutPartialsCollected{..}
        | depositOutPoint `elem` inpoints confirmedTx ->
            Spent{payoutTxid = txid confirmedTx, ..}
    PayoutNoncesCollected{..}
        | depositOutPoint `elem` inpoints confirmedTx ->
            Spent{payoutTxid = txid confirmedTx, ..}
    _ -> error "Invalid state transition"

-- Introspection functions
hasCooperativePayoutFailed :: DepositState -> Bool
hasCooperativePayoutFailed CooperativePathFailed{} = True
hasCooperativePayoutFailed _ = False
