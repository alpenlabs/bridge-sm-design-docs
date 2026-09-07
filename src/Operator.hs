module Operator
  ( OperatorRecord (..)
  , OperatorTable
  , CovenantId
  , PendingUpdate (..)
  , OperatorEvent (..)
  , OperatorSignal (..)
  , OperatorTransitionOutput (..)
  , OperatorState
  , initializeOperatorState
  , updateOperatorTable
  , notifyNewBlock
  , prepareCovenant
  , getCurrentOperatorTable
  , getMasterOperatorTable
  , getPendingUpdates
  , getCurrentCovenantId
  , getMembershipHistory
  , canObserveDeposits
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)

-- Prelude
type Height = Word32
type ActivationHeight = Height
type OperatorIdx = Word32
type P2PKey = String -- placeholder
type SchnorrKey = String -- placeholder

-- One immutable registration per index; re-entry requires a fresh index and signing key.
data OperatorRecord = OperatorRecord
  { p2pKey :: P2PKey
  , schnorrKey :: SchnorrKey
  , activationHeight :: ActivationHeight
  , deactivationHeight :: Maybe Height
  }
  deriving (Show, Eq, Ord)

-- Map keys are the actual, possibly sparse indices, not positions in an array.
type OperatorTable = Map.Map OperatorIdx OperatorRecord
type MasterTable = OperatorTable
type CurTable = OperatorTable

-- The aggregate identifies signing authority; the height is the last effective
-- admin-change boundary and is retained on automatic exits, permitting pre-staking.
type CovenantId = (SchnorrKey, ActivationHeight)

-- Activation height, additions, removals. Updates are ordered by activation height,
-- preserving the supplied order at equal heights. Additions precede removals within an update.
data PendingUpdate = PendingUpdate ActivationHeight (Set.Set OperatorIdx) (Set.Set OperatorIdx)
  deriving (Show, Eq)

type PendingUpdates = [PendingUpdate]

-- Confirmed exits, supplied in transaction order. An intent for any stake removes
-- the registration from active membership, regardless of the stake's covenant.
data OperatorEvent
  = UnstakingIntentConfirmed OperatorIdx
  | SlashConfirmed OperatorIdx
  deriving (Show, Eq)

-- Request a StakeSM identified by (CovenantId, OperatorIdx), retaining any matching
-- instance. Every member, including survivors, needs a stake for the exact covenant.
data OperatorSignal = InitializeStake CovenantId OperatorIdx OperatorTable
  deriving (Show, Eq)

data OperatorTransitionOutput = OperatorTransitionOutput
  { signals :: [OperatorSignal]
  }
  deriving (Show, Eq)

-- State
data OperatorState = OperatorState
  { masterTable :: MasterTable -- all known registrations, including past and future members
  , curTable :: CurTable -- current active membership
  , pendingUpdates :: PendingUpdates
  , exitedOperators :: Set.Set OperatorIdx
  , currentCovenantId :: CovenantId
  , membershipHistory :: [(Height, OperatorTable)] -- newest first; includes intermediate configurations
  , lastBlockHeight :: Height
  , lastTransitionHeight :: Maybe Height
  }
  deriving (Show, Eq)

-- Initial state from registration intervals at the supplied height.
-- Registration tables and schedules are assumed to be well-formed inputs.
initializeOperatorState :: Height -> MasterTable -> PendingUpdates -> (OperatorState, OperatorTransitionOutput)
initializeOperatorState height table updates
  | Map.null active = error "Rejected: empty operator set"
  | otherwise = (state, initializeStakes covenant active)
  where
    -- Keep entries with activation <= height and no deactivation yet.
    -- `maybe True (height <)` is Rust's Option::map_or(true, |end| height < end).
    active = Map.filter (\op -> activationHeight op <= height && maybe True (height <) (deactivationHeight op)) table
    -- Extract each entry's deactivation, keep Some(end) where end <= height,
    -- then collect its index into a set. Nothing does not count as an exit.
    exited = Map.keysSet $ Map.filter (maybe False (<= height) . deactivationHeight) table
    -- Flat-map each entry to its activation/deactivation heights, discard future
    -- heights, and take the maximum. Prepending 0 supplies a fallback for an empty list.
    boundary = maximum (0 : concatMap (filter (<= height) . boundaries) (Map.elems table))
    -- `:` prepends activation; `maybe [] pure` turns Nothing into [] and Just h into [h].
    boundaries op = activationHeight op : maybe [] pure (deactivationHeight op)
    covenant = covenantId boundary active
    state = OperatorState table active updates exited covenant [(height, active)] height (Just boundary)

-- STF: extend known registrations and supply the remaining scheduled updates.
-- Existing registrations, exits and current membership are unchanged.
updateOperatorTable :: OperatorState -> MasterTable -> PendingUpdates -> OperatorState
updateOperatorTable state table updates =
  -- Map.union is left-biased: existing records win when an index appears in both maps.
  state
    { masterTable = masterTable state `Map.union` table
    , pendingUpdates = updates
    }

-- Block-level STF: apply exits in transaction order, then due scheduled updates.
-- Finalize at most one successor and emit only its staking signals after the entire
-- block, even if several operators exit or the final signing set is unchanged.
-- The subsequent transaction pass starts with this finalized membership, including
-- all exits and scheduled changes. It updates stake lifecycles, not membership again;
-- it reuses membership-pass exit validation without recomputing it against the final set.
-- DRTs use this final covenant; admission separately checks stakes at their transaction
-- position. Historical SMs retain their own covenants.
notifyNewBlock :: OperatorState -> Height -> [OperatorEvent] -> (OperatorState, OperatorTransitionOutput)
notifyNewBlock state height events
  | toInteger height /= toInteger (lastBlockHeight state) + 1 = error "Rejected: nonconsecutive block"
  | otherwise =
      -- foldl' passes the resulting state to the next event, like Rust's Iterator::fold.
      let afterExits = foldl' exit state events
          -- span splits at the first future update, retaining both halves in order.
          -- This relies on height-sorted updates; it is not an arbitrary partition.
          (due, future) = span (\(PendingUpdate h _ _) -> h <= height) (pendingUpdates state)
          final = foldl' applyUpdate afterExits due
          changed = lastTransitionHeight final == Just height
          covenant = if changed then covenantId (snd $ currentCovenantId final) (curTable final) else currentCovenantId state
          output = if changed then initializeStakes covenant (curTable final) else OperatorTransitionOutput []
      in  (final {pendingUpdates = future, currentCovenantId = covenant, lastBlockHeight = height}, output)
  where
    exit current (UnstakingIntentConfirmed idx) = removeOperator height current idx
    exit current (SlashConfirmed idx) = removeOperator height current idx

-- Request stakes for a projected covenant without changing membership or block height.
-- Stakes for a different CovenantId cannot be reused for this projection.
prepareCovenant :: OperatorState -> ActivationHeight -> OperatorTransitionOutput
prepareCovenant state height
  | height <= lastBlockHeight state = error "Rejected: preparation requires a future activation"
  | otherwise =
      -- Take only the ordered prefix through the target height, then fold it over state.
      let due = takeWhile (\(PendingUpdate h _ _) -> h <= height) (pendingUpdates state)
          projected = foldl' applyUpdate state due
      in  if lastTransitionHeight projected == Just height
            then initializeStakes (covenantId height $ curTable projected) (curTable projected)
            else OperatorTransitionOutput []

-- Helpers (not independent events): record intermediate tables, but no signals.
-- Membership remains nonempty at each transition.
recordMembership :: Height -> OperatorState -> OperatorTable -> OperatorState
recordMembership height state table
  | Map.null table = error "Rejected: empty operator set"
  | table == curTable state = state
  | otherwise =
      state
        { curTable = table
        , membershipHistory = (height, table) : membershipHistory state
        , lastTransitionHeight = Just height
        }

removeOperator :: Height -> OperatorState -> OperatorIdx -> OperatorState
removeOperator height state idx
  | Map.notMember idx (masterTable state) = error "Rejected: unknown operator index"
  | otherwise =
      (recordMembership height state $ Map.delete idx $ curTable state)
        { exitedOperators = Set.insert idx (exitedOperators state)
        }

applyUpdate :: OperatorState -> PendingUpdate -> OperatorState
applyUpdate state (PendingUpdate height additions removals) =
  -- Record one resolved configuration per scheduled update, not per list element.
  (recordMembership height state resolved)
    { exitedOperators = exitedOperators state `Set.union` removals
    , currentCovenantId =
        if resolved /= curTable state
          then covenantId height resolved
          else currentCovenantId state
    }
  where
    -- Exclude previously exited indices from the requested additions.
    eligible = additions `Set.difference` exitedOperators state
    -- Select those records from the master map, then merge them with current membership.
    added = Map.restrictKeys (masterTable state) eligible `Map.union` curTable state
    -- Remove every requested index from the merged map; removal wins if present in both sets.
    resolved = Map.withoutKeys added removals

-- Read the $ chain right-to-left: map values in index order, extract signing keys,
-- then aggregate. Each $ simply applies the function on its left to the expression on its right.
covenantId :: ActivationHeight -> OperatorTable -> CovenantId
covenantId height table = (aggregateKeys $ map schnorrKey $ Map.elems table, height)

aggregateKeys :: [SchnorrKey] -> SchnorrKey
aggregateKeys = show -- placeholder for the existing aggregation in canonical OperatorIdx order

-- The list comprehension maps each index (in ascending order) to one initialization signal.
initializeStakes :: CovenantId -> OperatorTable -> OperatorTransitionOutput
initializeStakes covenant table = OperatorTransitionOutput [InitializeStake covenant i table | i <- Map.keys table]

-- Introspection functions (not part of the STF)
getCurrentOperatorTable :: OperatorState -> OperatorTable
getCurrentOperatorTable = curTable

getMasterOperatorTable :: OperatorState -> OperatorTable
getMasterOperatorTable = masterTable

getPendingUpdates :: OperatorState -> PendingUpdates
getPendingUpdates = pendingUpdates

getCurrentCovenantId :: OperatorState -> CovenantId
getCurrentCovenantId = currentCovenantId

getMembershipHistory :: OperatorState -> [(Height, OperatorTable)]
getMembershipHistory = membershipHistory

-- Whether membership permits deposit observation at the processed height.
-- Includes transition blocks. Admission separately requires usable stakes for the
-- final covenant before the DRT's transaction position.
canObserveDeposits :: OperatorState -> Height -> Bool
canObserveDeposits state height =
  height == lastBlockHeight state
