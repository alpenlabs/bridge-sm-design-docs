{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Deposit
  ( DepositState
  , DepositDuty
  , processPartial
  , processDepositConfirmation
  , processAssignment
  , processFulfillment
  , processPayoutNonce
  , processPayoutPartial
  , notifyNewBlock
  , processDepositSpend
  , hasCooperativePayoutFailed
  , processNagTick
  , processRetryTick
  ) where

-- Prelude

import Data.List.NonEmpty (NonEmpty ((:|)), head)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
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
  deriving (Show, Eq, Ord)

txid :: Transaction -> Txid
txid _ = "txid_placeholder" -- Placeholder implementation

sighashes :: DepositTx -> NonEmpty Sighash
sighashes _ = "sighash_placeholder" :| [] -- Placeholder implementation

inpoints :: Transaction -> NonEmpty OutPoint
inpoints _ = ("txid_placeholder", 0) :| [] -- Placeholder implementation for input outpoints (head :| tail)

witnessLength :: Transaction -> U32
witnessLength _ = 0 -- Placeholder implementation

-- State
-- This state tracks the lifecycle of a deposit UTXO.
data DepositState
  = Created -- State machine initialized, also tracks graph generations
      { depositIdx :: U32
      , depositTransaction :: DepositTx
      , depositRequestOutPoint :: OutPoint -- used to track the deposit request UTXO in case of aborts
      , outputIndex :: U32 -- index of the deposit output corresponding to the `depositIdx` (in case we aggregate multiple deposits into one transaction)
      , blockHeight :: U32 -- the last block height observed by this state machine
      , linkedGraphs :: Set.Set OperatorIdx -- operators whose graphs have been generated to spend this deposit
      }
  | GraphGenerated -- All operators' graphs have been generated and linked to this deposit, also tracks DT pubnonces
      { depositIdx :: U32
      , depositTransaction :: DepositTx
      , depositRequestOutPoint :: OutPoint
      , outputIndex :: U32
      , blockHeight :: U32
      , claimTxids :: Map.Map OperatorIdx Txid -- the txid of the claim transactions per operator (used to make sure that a claim is not already on chain in case of a malicious operator trying to start an early claim that may go unnoticed by GSM)
      , pubnonces :: Map.Map OperatorIdx PubNonce -- pubnonces required to sign the deposit transaction (per operator)
      }
  | DepositNoncesCollected -- All deposit pubnonces have been collected
      { depositIdx :: U32
      , depositTransaction :: DepositTx
      , depositRequestOutPoint :: OutPoint
      , outputIndex :: U32
      , blockHeight :: U32
      , claimTxids :: Map.Map OperatorIdx Txid
      , pubnonces :: Map.Map OperatorIdx PubNonce
      , aggNonce :: AggNonce -- aggregated nonce for signing the deposit transaction
      , partialSignatures :: Map.Map OperatorIdx PartialSignature -- partial signatures per operator for signing the deposit transaction
      }
  | DepositPartialsCollected -- All deposit partial signatures have been collected
      { depositIdx :: U32
      , outputIndex :: U32
      , blockHeight :: U32
      , depositRequestOutPoint :: OutPoint
      , signedDepositTx :: Transaction -- the fully signed deposit transaction ready to be broadcast
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
      }
  | PayoutDescriptorReceived -- The descriptor of the operator for the cooperative payout has been received
      { depositIdx :: U32
      , blockHeight :: U32
      , depositOutPoint :: OutPoint
      , assignee :: OperatorIdx
      , fulfillmentTxid :: Txid -- txid of the fulfillment transaction (fronting the user)
      , fulfillmentBlockHeight :: BitcoinBlockHeight -- block height where the fulfillment transaction was confirmed
      , cooperativePayoutDeadline :: BitcoinBlockHeight
      , payoutOutputDesc :: BtcDescriptor -- the output descriptor of the operator for the cooperative payout (needs to be provided by the recipient)
      , payoutNonces :: Map.Map OperatorIdx PubNonce -- pubnonces required to sign the cooperative payout transaction (per operator)
      }
  | PayoutNoncesCollected -- All pubnonces have been collected for cooperative payout
      { depositIdx :: U32
      , blockHeight :: U32
      , depositOutPoint :: OutPoint
      , assignee :: OperatorIdx -- required to pass onto the `CooperativeFailed` state from where a graph can be assigned
      , fulfillmentTxid :: Txid -- txid of the fulfillment transaction (fronting the user)
      , fulfillmentBlockHeight :: BitcoinBlockHeight -- block height where the fulfillment transaction was confirmed
      , payoutOutputDesc :: BtcDescriptor
      , cooperativePayoutDeadline :: BitcoinBlockHeight
      , payoutNonces :: Map.Map OperatorIdx PubNonce
      , payoutAggNonce :: AggNonce -- aggregated nonce for signing the cooperative payout transaction
      , payoutPartialSignatures :: Map.Map OperatorIdx PartialSignature -- partial signatures per operator for signing the cooperative payout transaction
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
  = PublishDepositNonce -- publish this operator's nonce for spending the drt
      { depositIdx :: U32
      , depositRequestOutPoint :: OutPoint -- DRT outpoint to ID the signing session
      , claimTxids :: Map.Map OperatorIdx Txid -- the txid of the claim transaction per operator (used to make sure that a claim is not already on chain in case of a malicious operator trying to start an early claim that may go unnoticed by GSM)
      , orderedPubkeys :: [SchnorrKey] -- ordered public keys of all operators for MuSig2 signing
      }
  | PublishDepositPartial -- publish this operator's partial signature for spending the drt
      { depositRequestOutPoint :: OutPoint -- DRT outpoint to resume the earlier signing session
      , depositSighash :: Sighash -- sighash to be signed for the deposit transaction
      , claimTxids :: Map.Map OperatorIdx Txid -- the txid of the claim transaction per operator (used to make sure that a claim is not already on chain in case of a malicious operator trying to start an early claim that may go unnoticed by GSM)
      , depositAggNonce :: AggNonce -- the aggregate nonce for signing the deposit transaction (used to generate the partial signature)
      }
  | PublishDeposit -- publish the deposit transaction to the Bitcoin network
      { signedDepositTx :: Transaction -- the fully signed deposit transaction ready to be broadcast
      }
  | FulfillWithdrawal -- front the user by sending funds to the provided descriptor within the given deadline
      { depositIdx :: DepositIdx
      , deadline :: BitcoinBlockHeight
      , recipientDesc :: BtcDescriptor -- the user's descriptor where funds are to be sent by the operator
      }
  | -- request pubnonces from *all* operators for cooperative payout
    -- this duty execution will generate the operator's descriptor (which will then be stored in state)
    -- the assignee will also request *themselves* since getting a new descriptor from a wallet is a side-effect and has to be done inside a duty context
    -- only the assignee creates this duty
    RequestPayoutNonce
      { depositIdx :: DepositIdx
      }
  | PublishPayoutNonces -- publish the nonce for spending the deposit utxo cooperatively
      { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
      , operatorIdx :: OperatorIdx -- the index of the operator requesting cooperation for payout (could be the same as this operator)
      , payoutOutputDesc :: BtcDescriptor -- descriptor of the operator to receive payout
      }
  | PublishPayoutPartial -- publish the partial signature for spending the deposit utxo cooperatively
      { depositOutPoint :: OutPoint -- outpoint referencing the deposit utxo
      , depositIdx :: DepositIdx
      , aggNonce :: AggNonce
      }
  | -- The assignee generates their own partial signature as well as signing and broadcasting the payout tx.
    -- The extra fields (aggNonce, collectedPartials) are needed for generating the assignee's partial.
    PublishPayout
      { depositOutPoint :: OutPoint
      , depositIdx :: DepositIdx
      , aggNonce :: AggNonce
      , payoutTx :: Transaction
      , collectedPartials :: Map.Map OperatorIdx PartialSignature -- partial signatures per operator for signing the cooperative payout transaction
      }
  | -- nag other operators for missing information
    Nag
      { duty :: NagDuty
      }
  deriving (Show, Eq, Ord)

-- Duties to nag for missing information from other operators
data NagDuty
  = -- Nag other operators to broadcast their nonce for a deposit
    NagDepositNonce
      { depositIdx :: DepositIdx -- the index of the deposit for which the nonce is required
      , operatorIdx :: OperatorIdx -- the index of the operator to nag
      }
  | -- Nag other operators to broadcast their partial signature for a deposit
    NagDepositPartial
      { depositIdx :: DepositIdx -- the index of the deposit for which the partial signature is required
      , operatorIdx :: OperatorIdx -- the index of the operator to nag
      }
  | -- Nag other operators to broadcast their nonce for a payout
    NagPayoutNonce
      { depositIdx :: DepositIdx -- the index of the deposit for which the payout nonce is required
      , operatorIdx :: OperatorIdx -- the index of the operator to nag
      }
  | -- Nag other operators to broadcast their partial signature for a payout
    NagPayoutPartial
      { depositIdx :: DepositIdx -- the index of the deposit for which the payout partial signature
      , operatorIdx :: OperatorIdx -- the index of the operator to nag
      }
  deriving (Show, Eq, Ord)

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

getOrderedPubkeys :: ExecConfig -> [SchnorrKey]
getOrderedPubkeys cfg = map (\(_, _, schnorrKey, _) -> schnorrKey) $ Set.toAscList (operators cfg)

getFulfillWithdrawalDuty :: DepositState -> ExecConfig -> Maybe DepositDuty
getFulfillWithdrawalDuty Assigned {..} cfg =
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

-- Placeholder verification for partial signatures
verifyPartialSig :: ExecConfig -> OperatorIdx -> PubNonce -> AggNonce -> Sighash -> PartialSignature -> Bool
verifyPartialSig _cfg _operatorIdx _pubNonce _aggNonce _sighash _partialSig =
  True -- Placeholder: accept all signatures for now

-- Numeric constants (params)
-- abort window
abortWindow :: BitcoinBlockHeight
abortWindow = 1008 -- e.g., 1 week assuming 10 min blocks

-- cooperative payout window
cooperativePayoutWindow :: BitcoinBlockHeight
cooperativePayoutWindow = 2016 -- e.g., ~2 weeks assuming 10 min blocks

-- STFs
-- Definitions
processGraphGenerated :: DepositState -> ExecConfig -> Txid -> OperatorIdx -> (DepositState, DepositDuty)
processNonce :: DepositState -> ExecConfig -> PubNonce -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processPartial :: DepositState -> ExecConfig -> PartialSignature -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processDepositConfirmation :: DepositState -> Transaction -> DepositState
processAssignment
  :: DepositState -> ExecConfig -> OperatorIdx -> BitcoinBlockHeight -> BtcDescriptor -> (DepositState, Maybe DepositDuty)
processFulfillment
  :: DepositState -> ExecConfig -> Transaction -> BitcoinBlockHeight -> (DepositState, Maybe DepositDuty)
processPayoutDescriptor :: DepositState -> BtcDescriptor -> (DepositState, DepositDuty)
processPayoutNonce :: DepositState -> ExecConfig -> PubNonce -> OperatorIdx -> (DepositState, Maybe DepositDuty)
processPayoutPartial
  :: DepositState -> ExecConfig -> PartialSignature -> OperatorIdx -> (DepositState, Maybe DepositDuty)
notifyNewBlock :: BitcoinBlockHeight -> DepositState -> DepositState
processDepositSpend :: DepositState -> Transaction -> DepositState
processDepositRequestSpend :: DepositState -> Transaction -> DepositState
-- Implementations
processGraphGenerated deposit@Created {..} cfg claimTxid operatorIdx =
  let linkedGraphs' = linkedGraphs `Set.union` Set.singleton operatorIdx
      claimTxids = Map.insert operatorIdx claimTxid claimTxids -- since this comes from an internal signal, no need to error on duplicates
      orderedPubkeys = getOrderedPubkeys cfg
      newState =
        if Set.size linkedGraphs' == opCardinality cfg
          then
            GraphGenerated
              { pubnonces = mempty
              , claimTxids = claimTxids
              , ..
              }
          else
            deposit {linkedGraphs = linkedGraphs'}
      duty = PublishDepositNonce {..}
  in  (newState, duty)
processGraphGenerated _ _ _ _ = error "Invalid state transition"

processNonce deposit@GraphGenerated {..} cfg nonce operatorIdx =
  case Map.lookup operatorIdx pubnonces of
    Just _ ->
      error $ "Duplicate nonce received from operator: " ++ show operatorIdx
    Nothing ->
      let newNonces = Map.insert operatorIdx nonce pubnonces
          (newState, duty) =
            if Map.size newNonces == opCardinality cfg
              then
                let aggNonce = "agg_nonce_placeholder" -- Placeholder for actual aggregation logic
                    newState' =
                      DepositNoncesCollected
                        { pubnonces = newNonces
                        , aggNonce = aggNonce
                        , partialSignatures = mempty
                        , ..
                        }
                    duty' =
                      Just
                        PublishDepositPartial
                          { depositRequestOutPoint = Data.List.NonEmpty.head $ inpoints $ tx depositTransaction
                          , depositSighash = Data.List.NonEmpty.head $ sighashes depositTransaction
                          , depositAggNonce = aggNonce
                          , ..
                          }
                in  (newState', duty')
              else
                (deposit {pubnonces = newNonces}, Nothing)
      in  (newState, duty)
processNonce _ _ _ _ = error "Invalid state transition"

processPartial deposit@DepositNoncesCollected {..} cfg partialSig operatorIdx =
  case Map.lookup operatorIdx partialSignatures of
    Just _ ->
      error $ "Duplicate partial signature received from operator: " ++ show operatorIdx
    Nothing ->
      let operatorNonce = case Map.lookup operatorIdx pubnonces of
            Just nonce -> nonce
            Nothing -> error "Operator nonce not found"
          depositSighash = Data.List.NonEmpty.head $ sighashes depositTransaction
      in  if verifyPartialSig cfg operatorIdx operatorNonce aggNonce depositSighash partialSig
            then
              let newPartials = Map.insert operatorIdx partialSig partialSignatures
                  (newState, duty) =
                    if Map.size newPartials == opCardinality cfg
                      then
                        let signedDepositTx = "signed_deposit_tx_placeholder" -- Placeholder for actual aggregation and tx finalization logic
                            newState' =
                              DepositPartialsCollected
                                { signedDepositTx = signedDepositTx
                                , ..
                                }
                            duty' = Just PublishDeposit {signedDepositTx = signedDepositTx}
                        in  (newState', duty')
                      else
                        (deposit {partialSignatures = newPartials}, Nothing)
              in  (newState, duty)
            else
              error "Partial Signature Verification Failed"
processPartial _ _ _ _ = error "Invalid state transition"

processDepositConfirmation DepositPartialsCollected {..} confirmedTx =
  let newState =
        if txid confirmedTx == txid signedDepositTx
          then
            Deposited
              { depositOutPoint = (txid confirmedTx, outputIndex)
              , ..
              }
          else error "Transaction confirmation does not match expected deposit transaction"
  in  newState
-- This can happen if one of the operators withholds their own partial signature
-- while aggregating it with the rest of the collected partials and broadcasts it unilaterally
processDepositConfirmation DepositNoncesCollected {..} confirmedTx =
  let newState =
        if txid confirmedTx == txid (tx depositTransaction)
          then
            Deposited
              { depositOutPoint = (txid confirmedTx, outputIndex)
              , ..
              }
          else error "Transaction confirmation does not match expected deposit transaction"
  in  newState
processDepositConfirmation _ _ = error "Invalid state transition"

processAssignment Deposited {..} cfg assignee deadline recipientDesc =
  let newState =
        Assigned
          { assignee = assignee
          , deadline = deadline
          , recipientDesc = recipientDesc
          , ..
          }
      duty = getFulfillWithdrawalDuty newState cfg
  in  (newState, duty)
processAssignment deposit@Assigned {} cfg assignee' deadline' recipientDesc' =
  let newState =
        deposit
          { assignee = assignee'
          , deadline = deadline'
          , recipientDesc = recipientDesc'
          }
      duty = getFulfillWithdrawalDuty newState cfg
  in  (newState, duty)
processAssignment _ _ _ _ _ = error "Invalid state transition"

processFulfillment Assigned {..} cfg fulfillmentTx fulfillmentBlockHeight =
  let newState =
        Fulfilled
          { fulfillmentTxid = txid fulfillmentTx
          , fulfillmentBlockHeight = fulfillmentBlockHeight
          , cooperativePayoutDeadline = fulfillmentBlockHeight + cooperativePayoutWindow
          , ..
          }
      duty =
        if assignee == povIdx cfg
          then Just RequestPayoutNonce {..}
          else Nothing
  in  (newState, duty)
processFulfillment _ _ _ _ = error "Invalid state transition"

processPayoutDescriptor Fulfilled {..} payoutOutputDesc =
  let newState =
        PayoutDescriptorReceived
          { payoutOutputDesc = payoutOutputDesc
          , payoutNonces = mempty
          , ..
          }
      duty = PublishPayoutNonces {operatorIdx = assignee, ..}
  in  (newState, duty)
processPayoutDescriptor _ _ = error "Invalid state transition"

processPayoutNonce deposit@PayoutDescriptorReceived {..} cfg nonce operatorIdx =
  case Map.lookup operatorIdx payoutNonces of
    Just _ ->
      error $ "Duplicate payout nonce received from operator: " ++ show operatorIdx
    Nothing ->
      let newPayoutNonces = Map.insert operatorIdx nonce payoutNonces
          (newState, duty) =
            if Map.size newPayoutNonces == opCardinality cfg
              then
                let aggNonce = "payout_agg_nonce_placeholder" -- Placeholder for actual aggregation logic
                    newState' =
                      PayoutNoncesCollected
                        { payoutNonces = newPayoutNonces
                        , payoutAggNonce = aggNonce
                        , payoutPartialSignatures = mempty
                        , ..
                        }
                    duty' =
                      -- Everyone except the assignee publishes their cooperative-payout partial signature.
                      -- Rationale: prevent payout-tx hostage attacks.
                      -- If the assignee also published their partial, a malicious coordinator/operator could
                      -- withhold their own partial and force the assignee to fall back to posting a claim.
                      -- If a cooperative payout is later broadcast, the assignee is unable to spend the
                      -- contested or uncontested path, and can be slashed after the timelock expires.
                      -- By withholding the assignee's partial, only the assignee can finalize and broadcast
                      -- the cooperative payout transaction.
                      if assignee /= povIdx cfg
                        then Just PublishPayoutPartial {..}
                        else Nothing
                in  (newState', duty')
              else
                (deposit {payoutNonces = newPayoutNonces}, Nothing)
      in  (newState, duty)
processPayoutNonce _ _ _ _ = error "Invalid state transition"

processPayoutPartial deposit@PayoutNoncesCollected {..} cfg partialSig operatorIdx =
  case Map.lookup operatorIdx payoutPartialSignatures of
    Just _ ->
      error $ "Duplicate payout partial signature received from operator: " ++ show operatorIdx
    Nothing ->
      let operatorNonce = case Map.lookup operatorIdx payoutNonces of
            Just nonce -> nonce
            Nothing -> error "Operator nonce not found - this should never happen"
          payoutSighash = "payout_sighash_placeholder" -- Placeholder for actual payout sighash
      in  if verifyPartialSig cfg operatorIdx operatorNonce payoutAggNonce payoutSighash partialSig
            then
              let newPartials = Map.insert operatorIdx partialSig payoutPartialSignatures
                  (newState, duty) =
                    if Map.size newPartials == opCardinality cfg - 1
                      then
                        let newState' = deposit {payoutPartialSignatures = newPartials}
                            duty' =
                              if assignee == povIdx cfg
                                then
                                  Just
                                    PublishPayout
                                      { payoutTx = "payout_transaction_placeholder"
                                      , collectedPartials = newPartials
                                      , aggNonce = payoutAggNonce
                                      , ..
                                      }
                                else Nothing
                        in  (newState', duty')
                      else
                        (deposit {payoutPartialSignatures = newPartials}, Nothing)
              in  (newState, duty)
            else
              error "Partial Signature Verification Failed"
processPayoutPartial _ _ _ _ = error "Invalid state transition"

-- Processes information about new blocks and applies any updates related to block height timeouts
notifyNewBlock height Fulfilled {..}
  | height > fulfillmentBlockHeight + cooperativePayoutWindow =
      CooperativePathFailed {..}
  | height <= blockHeight =
      error "Rejecting already processed block"
notifyNewBlock height PayoutDescriptorReceived {..}
  | height > fulfillmentBlockHeight + cooperativePayoutWindow =
      CooperativePathFailed {..}
  | height <= blockHeight =
      error "Rejecting already processed block"
notifyNewBlock height PayoutNoncesCollected {..}
  | height > fulfillmentBlockHeight + cooperativePayoutWindow =
      CooperativePathFailed {..}
  | height <= blockHeight =
      error "Rejecting already processed block"
notifyNewBlock _ Aborted {} = error "No more updates required" -- does not need any more updates
notifyNewBlock _ Spent {} = error "No more updates required" -- does not need any more updates
notifyNewBlock newHeight state = case lastProcessedBlock state of
  Just h | newHeight > h -> state {blockHeight = newHeight}
  _ -> error "Rejecting already processed block"

-- The assignee never shares their partial signature to prevent payout tx hostage attacks.
-- The assignee generates their partial only when broadcasting the cooperative payout.
-- A spend can be seen while in PayoutNoncesCollected (after assignee broadcasts).
-- It is also possible that a spend is seen after we progress to a `CooperativePathFailed` state
-- since each operator may observe the spend at different times and because the timeout
-- for cooperative payout is different for different operators.
processDepositSpend state confirmedTx = case state of
  PayoutNoncesCollected {..}
    | depositOutPoint `elem` inpoints confirmedTx ->
        Spent {payoutTxid = txid confirmedTx, ..}
  CooperativePathFailed {..}
    | depositOutPoint `elem` inpoints confirmedTx ->
        Spent {payoutTxid = txid confirmedTx, ..}
  _ -> error "Invalid state transition"

chkUserSpend :: OutPoint -> Transaction -> Bool
chkUserSpend outPoint confirmedTx = outPoint `elem` inpoints confirmedTx && witnessLength confirmedTx /= 1 -- script spend has more than 1 witness item (covenant spend has exactly 1 witness item)

-- While it _should_ be enough to just check if the DRT is old enough,
-- this stricter check for the non-covenant spend of the DRT makes sure
-- that a malicious operator cannot mount a tx hostage attack where
-- they hold all the partial signatures but do not broadcast the deposit tx
-- until after the abort window has passed and then races with the user to spend the DRT.
-- If this attack succeeds, the deposit may be minted but the graph data would be lost.
processDepositRequestSpend state confirmedTx = case state of
  Created {..}
    | chkUserSpend depositRequestOutPoint confirmedTx ->
        Aborted {..}
    | otherwise -> error "not user spend"
  GraphGenerated {..}
    | chkUserSpend depositRequestOutPoint confirmedTx ->
        Aborted {..}
    | otherwise -> error "not user spend"
  DepositNoncesCollected {..}
    | chkUserSpend depositRequestOutPoint confirmedTx ->
        Aborted {..}
    | otherwise -> error "not user spend"
  DepositPartialsCollected {..}
    | chkUserSpend depositRequestOutPoint confirmedTx ->
        Aborted {..}
    | otherwise -> error "not user spend"
  _ -> error $ "Invalid event for state: " ++ show state

-- Introspection functions
hasCooperativePayoutFailed :: DepositState -> Bool
hasCooperativePayoutFailed CooperativePathFailed {} = True
hasCooperativePayoutFailed _ = False

lastProcessedBlock :: DepositState -> Maybe BitcoinBlockHeight
lastProcessedBlock state = case state of
  Aborted {} -> Nothing
  Spent {} -> Nothing
  otherState -> Just (blockHeight otherState)

-- Retry Handlers
-- Declarations
processNagTick :: DepositState -> ExecConfig -> Set.Set DepositDuty
processRetryTick :: DepositState -> Set.Set DepositDuty
-- Definitions
processNagTick state cfg =
  let expectedIds = Set.map (\(idx, _, _, _) -> idx) $ operators cfg
      presentIds = Map.keysSet $ case state of
        GraphGenerated {pubnonces} -> pubnonces
        DepositNoncesCollected {partialSignatures} -> partialSignatures
        PayoutDescriptorReceived {payoutNonces} -> payoutNonces
        PayoutNoncesCollected {payoutPartialSignatures} -> payoutPartialSignatures
        _ -> Map.empty

      missingIds = Set.difference expectedIds presentIds
  in  Set.fromList
        $ mapMaybe
          ( \opIdx -> case state of
              GraphGenerated {..} ->
                Just Nag {duty = NagDepositNonce {depositIdx = depositIdx, operatorIdx = opIdx}}
              DepositNoncesCollected {..} ->
                Just Nag {duty = NagDepositPartial {depositIdx = depositIdx, operatorIdx = opIdx}}
              PayoutDescriptorReceived {..} ->
                Just Nag {duty = NagPayoutNonce {depositIdx = depositIdx, operatorIdx = opIdx}}
              PayoutNoncesCollected {..} ->
                Just Nag {duty = NagPayoutPartial {depositIdx = depositIdx, operatorIdx = opIdx}}
              _ -> Nothing
          )
        $ Set.toList missingIds

processRetryTick state = case state of
  DepositPartialsCollected {..} ->
    Set.singleton PublishDeposit {signedDepositTx = signedDepositTx}
  Assigned {..} ->
    Set.singleton
      FulfillWithdrawal
        { depositIdx = depositIdx
        , deadline = deadline
        , recipientDesc = recipientDesc
        }
  Fulfilled {..} ->
    Set.singleton RequestPayoutNonce {depositIdx = depositIdx}
  PayoutNoncesCollected {..} ->
    Set.singleton
      PublishPayoutPartial
        { depositOutPoint = depositOutPoint
        , depositIdx = depositIdx
        , aggNonce = payoutAggNonce
        }
  -- the rest of the duties need not be retried
  _ -> Set.empty
