module Operator (
  OperatorState,
  updateOperatorTable,
  notifyNewBlock,
  getCurrentOperatorTable,
  getMasterOperatorTable,
  getPendingUpdates,
) where

-- Prelude
import Data.Set qualified as Set
import Data.Word

type U32 = Data.Word.Word32
type Height = U32
type ActivationHeight = Height
type OperatorIdx = U32
type P2PKey = String -- placeholder
type SchnorrKey = String -- placeholder
type AdaptorKey = String -- placeholder

type OperatorRecord = (OperatorIdx, P2PKey, SchnorrKey, AdaptorKey) -- ordered by `OperatorIdx`
-- a `Data.Set` in Haskell orders tuples by default based on the first member
-- in Rust, use `BTreeSet` to get the same property

type OperatorTable = Set.Set OperatorRecord
type MasterTable = OperatorTable
type CurTable = OperatorTable
type NewTable = OperatorTable

type PendingUpdate = (ActivationHeight, OperatorTable) -- ordered by `ActivationHeight`
type PendingUpdates = Set.Set PendingUpdate

-- State
data OperatorState
  = Ready MasterTable CurTable
  | UpdateReceived MasterTable CurTable PendingUpdates

-- STF
updateOperatorTable :: OperatorState -> ActivationHeight -> NewTable -> OperatorState
updateOperatorTable curState activationHeight newTable =
  let newPendingUpdate = Set.singleton (activationHeight, newTable)
      (masterTable, curTable, pendingUpdates) = case curState of
        Ready m c -> (m, c, Set.empty)
        UpdateReceived m c p -> (m, c, p)
   in UpdateReceived
        (newTable `Set.union` masterTable)
        curTable
        (pendingUpdates `Set.union` newPendingUpdate)

-- STF
notifyNewBlock :: OperatorState -> Height -> OperatorState
-- not affected
notifyNewBlock (Ready masterTable curTable) _height =
  Ready masterTable curTable
notifyNewBlock (UpdateReceived masterTable curTable pendingUpdates) height =
  -- find the first instance where `height` >= `activationHeight`
  case Set.lookupGE (height, Set.empty) pendingUpdates of
    Just (activationHeight, newTable) ->
      -- if this was the only pending update, then transition to ready
      if length pendingUpdates == 1
        then Ready masterTable newTable
        -- if there are other pending updates,
        -- then just update the `curTable` and `pendingUpdates`
        else
          UpdateReceived
            masterTable
            newTable
            (Set.delete (activationHeight, newTable) pendingUpdates)
    Nothing ->
      UpdateReceived masterTable curTable pendingUpdates -- no viable update found

-- Introspection functions (not part of the STF)
getCurrentOperatorTable :: OperatorState -> OperatorTable
getCurrentOperatorTable (Ready _ curTable) = curTable
getCurrentOperatorTable (UpdateReceived _ curTable _) = curTable

getMasterOperatorTable :: OperatorState -> OperatorTable
getMasterOperatorTable (Ready masterTable _) = masterTable
getMasterOperatorTable (UpdateReceived masterTable _ _) = masterTable

getPendingUpdates :: OperatorState -> PendingUpdates
getPendingUpdates (Ready _ _) = Set.empty
getPendingUpdates (UpdateReceived _ _ pendingUpdates) = pendingUpdates
