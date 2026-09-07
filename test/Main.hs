module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Operator

main :: IO ()
main = do
  let a = OperatorRecord "peer-a" "a" 0 Nothing
      b = OperatorRecord "peer-b" "b" 0 Nothing
      c = OperatorRecord "peer-c" "c" 0 Nothing
      d = OperatorRecord "peer-d" "d" 2 Nothing
      abc = Map.fromList [(8, c), (1, a), (3, b)]
      (initial, initialOutput) = initializeOperatorState 0 abc []
      (ordinary, noChange) = notifyNewBlock initial 1 []
      (removed, removedOutput) = notifyNewBlock ordinary 2 [UnstakingIntentConfirmed 3, SlashConfirmed 8]
      onlyA = Map.singleton 1 a
  assert "initial stakes use sparse indices in canonical order" $
    signals initialOutput == [InitializeStake (show ["a", "b", "c"], 0) i abc | i <- [1, 3, 8]]
  assert "ordinary blocks preserve covenant identity and do not restake" $
    getCurrentCovenantId ordinary == getCurrentCovenantId initial && null (signals noChange)
  assert "only the final successor produces staking signals" $
    signals removedOutput == [InitializeStake (show ["a"], 0) 1 onlyA]
  assert "intermediate indexed membership is retained without extra covenants" $
    getMembershipHistory removed == [(2, onlyA), (2, Map.delete 3 abc), (0, abc)]
  assert "automatic exits retain historical registrations" $ getMasterOperatorTable removed == abc
  assert "automatic-exit blocks permit deposit indexing" $ canObserveDeposits removed 2
  assert "observation does not depend on stakes or participation" $ canObserveDeposits ordinary 1
  assert "observation requires completion of the matching block" $ not (canObserveDeposits ordinary 2)
  let (afterDuplicates, duplicateOutput) = notifyNewBlock removed 3 [SlashConfirmed 3, UnstakingIntentConfirmed 8]
  assert "repeated historical exits do not create another covenant" $
    getCurrentCovenantId afterDuplicates == getCurrentCovenantId removed
      && getMembershipHistory afterDuplicates == getMembershipHistory removed
      && null (signals duplicateOutput)
      && canObserveDeposits afterDuplicates 3
  let (sameBlockDuplicates, _) = notifyNewBlock ordinary 2 [SlashConfirmed 3, UnstakingIntentConfirmed 3]
  assert "duplicate exits within one block have one effect" $ length (getMembershipHistory sameBlockDuplicates) == 2

  let ab = Map.fromList [(1, a), (3, b)]
      abd = Map.insert 10 d ab
      schedule = [PendingUpdate 2 (Set.singleton 10) Set.empty]
      (beforeSchedule, _) = initializeOperatorState 0 abd schedule
      (beforeActivation, _) = notifyNewBlock beforeSchedule 1 []
      preStakes = prepareCovenant beforeActivation 2
      (mixed, mixedOutput) = notifyNewBlock beforeActivation 2 [UnstakingIntentConfirmed 3]
      ad = Map.delete 3 abd
  assert "a future update is not activated early" $
    getCurrentOperatorTable beforeActivation == ab && getPendingUpdates beforeActivation == schedule
  assert "preparation uses the future activation height" $
    signals preStakes == [InitializeStake (show ["a", "b", "d"], 2) i abd | i <- [1, 3, 10]]
  assert "exits precede admin additions and invalidate a different prepared covenant" $
    getCurrentOperatorTable mixed == ad
      && signals mixedOutput == [InitializeStake (show ["a", "d"], 2) i ad | i <- [1, 10]]
      && null (getPendingUpdates mixed)
  assert "admin-transition blocks permit indexing independently of stake readiness" $ canObserveDeposits mixed 2
  let (activated, activatedOutput) = notifyNewBlock beforeActivation 2 []
  assert "an unchanged projection requests the same stake identities at activation" $
    activatedOutput == preStakes && snd (getCurrentCovenantId activated) == 2 && canObserveDeposits activated 2

  let returningB = OperatorRecord "peer-b-returning" "b-returning" 2 Nothing
      reentryTable = Map.insert 10 returningB ab
      (beforeReentry, _) = initializeOperatorState 1 reentryTable [PendingUpdate 2 (Set.singleton 10) Set.empty]
      (reentered, reentryOutput) = notifyNewBlock beforeReentry 2 [UnstakingIntentConfirmed 3]
      (afterReentryIntent, reentryIntentOutput) = notifyNewBlock reentered 3 [UnstakingIntentConfirmed 3]
  assert "re-entry uses a fresh signing key and index" $
    getCurrentCovenantId reentered == (show ["a", "b-returning"], 2)
      && signals reentryOutput
        == [InitializeStake (show ["a", "b-returning"], 2) i (Map.fromList [(1, a), (10, returningB)]) | i <- [1, 10]]
      && canObserveDeposits reentered 2
  assert "an old-index intent cannot remove a fresh-key registration" $
    getCurrentOperatorTable afterReentryIntent == getCurrentOperatorTable reentered
      && null (signals reentryIntentOutput)

  let (afterAdminExit, adminExitOutput) = notifyNewBlock activated 3 [SlashConfirmed 3]
  assert "automatic exits retain the last effective admin height" $
    getCurrentCovenantId afterAdminExit == (show ["a", "d"], 2)
      && signals adminExitOutput == [InitializeStake (show ["a", "d"], 2) i ad | i <- [1, 10]]
  let roundTrip = [PendingUpdate 2 (Set.singleton 10) Set.empty, PendingUpdate 2 Set.empty (Set.singleton 10)]
      (beforeRoundTrip, _) = initializeOperatorState 1 abd roundTrip
      (afterRoundTrip, roundTripOutput) = notifyNewBlock beforeRoundTrip 2 []
  assert "effective admin changes returning to the original signing set advance its height" $
    getCurrentCovenantId afterRoundTrip == (show ["a", "b"], 2)
      && signals roundTripOutput == [InitializeStake (show ["a", "b"], 2) i ab | i <- [1, 3]]
  let (beforeNoop, _) = initializeOperatorState 1 ab [PendingUpdate 2 (Set.singleton 1) Set.empty]
      (afterNoop, noopOutput) = notifyNewBlock beforeNoop 2 []
  assert "ineffective admin updates preserve the covenant and emit no stakes" $
    getCurrentCovenantId afterNoop == getCurrentCovenantId beforeNoop && null (signals noopOutput)

  let unchanged = updateOperatorTable removed abc []
      e = OperatorRecord "peer-e" "e" 4 Nothing
      extended = Map.insert 20 e abc
      extra = [PendingUpdate 4 (Set.singleton 20) Set.empty]
      registered = updateOperatorTable unchanged extended extra
      (waiting, _) = notifyNewBlock registered 3 []
      (joined, joinedOutput) = notifyNewBlock waiting 4 []
      (afterOldIntent, oldIntentOutput) = notifyNewBlock joined 5 [UnstakingIntentConfirmed 3]
  assert "a table update does not resurrect exited registrations" $ getCurrentOperatorTable unchanged == onlyA
  assert "additional registrations stay pending until their activation" $
    getMasterOperatorTable registered == extended && getCurrentOperatorTable waiting == onlyA
  assert "scheduled additions do not restore older exited members" $
    getCurrentOperatorTable joined == Map.insert 20 e onlyA && length (signals joinedOutput) == 2
  assert "an old-index intent cannot evict a later registration" $
    getCurrentOperatorTable afterOldIntent == getCurrentOperatorTable joined && null (signals oldIntentOutput)

  let retiringA = a {deactivationHeight = Just 2}
      replacement = Map.fromList [(1, retiringA), (10, d)]
      ordered = [PendingUpdate 2 (Set.singleton 10) Set.empty, PendingUpdate 2 Set.empty (Set.singleton 1)]
      (beforeReplacement, _) = initializeOperatorState 1 replacement ordered
      (replaced, replacementOutput) = notifyNewBlock beforeReplacement 2 []
  assert "all same-height admin updates apply, in authorized order" $
    getCurrentOperatorTable replaced == Map.singleton 10 d
      && signals replacementOutput == [InitializeStake (show ["d"], 2) 10 (Map.singleton 10 d)]
  let (batched, _) = initializeOperatorState 1 replacement [PendingUpdate 2 (Set.singleton 10) (Set.singleton 1)]
      (afterBatch, _) = notifyNewBlock batched 2 []
  assert "one scheduled update records only its resolved membership" $
    getMembershipHistory afterBatch == [(2, Map.singleton 10 d), (1, Map.singleton 1 retiringA)]
  assertRejected "an earlier empty set is not rescued by a later addition" "empty operator set" $
    notifyNewBlock beforeReplacement 2 [UnstakingIntentConfirmed 1]
  let (reverseOrder, _) = initializeOperatorState 1 replacement (reverse ordered)
  assertRejected "same-height admin updates must not be reordered" "empty operator set" $
    notifyNewBlock reverseOrder 2 []
  let (fresh, _) = initializeOperatorState 2 replacement []
  assert "initial membership follows registration intervals" $
    getCurrentOperatorTable fresh == Map.singleton 10 d && snd (getCurrentCovenantId fresh) == 2

  assertRejected "unknown index" "unknown operator index" $ notifyNewBlock ordinary 2 [SlashConfirmed 999]
  assertRejected "duplicate block notification" "nonconsecutive block" $ notifyNewBlock removed 2 []
  assertRejected "skipped block notification" "nonconsecutive block" $ notifyNewBlock initial 2 []
  putStrLn "OperatorSetSM specification checks passed."

assert :: String -> Bool -> IO ()
assert label condition = unless condition $ error ("Failed: " ++ label)

assertRejected :: (Show a) => String -> String -> a -> IO ()
assertRejected label expected value = do
  -- Force the whole result: a lazy record/tuple alone may hide a failed transition.
  result <- try (evaluate $ length $ show value) :: IO (Either ErrorCall Int)
  case result of
    Left err -> assert label (expected `isInfixOf` show err)
    Right _ -> error ("Expected rejection: " ++ label)
