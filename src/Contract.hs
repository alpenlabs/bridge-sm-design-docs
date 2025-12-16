{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Contract (ContractState) where


-- State
-- This represents the state of any pegout graph associated with a particular deposit
-- TODO: Expand with associated fields/substates as needed
data ContractState = 
  Created
  | GraphsGenerated
  | AdaptorsVerified
  | NoncesCollected
  | PartialsCollected
  | Deposited
  | Aborted
  | Assigned
  | Fulfilled
  | CooperativePayoutNoncesCollected
  | CooperativePayoutPartialsCollected
  | CooperativePathFailed
  | Claimed
  | Contested
  | BridgeProofPosted
  | BridgeProofTimeout
  | CounterProofPosted
  | AllNacked
  | Acked
  | Spent
  | Slashed
  deriving (Show, Eq)

-- State Transition Functions
-- TODO: Implement state transition functions for ContractState 
 
-- Introspection Functions
-- TODO: Implement introspection functions for ContractState
