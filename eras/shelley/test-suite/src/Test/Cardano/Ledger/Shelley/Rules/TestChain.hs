{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.TestChain
  ( -- TestPoolReap
    removedAfterPoolreap,
    -- TestNewEpoch
    adaPreservationChain,
    collisionFreeComplete,
    -- Test Pool
    poolProperties,
    -- Test Delegation
    delegProperties,
    forAllChainTrace,
    -- Helper Functions
    ledgerTraceFromBlock,
    -- Helper Constraints
    TestingLedger,
    -- Stake Comp
    stakeDistr,
    stakeIncrTest,
    incrementalStakeProp,
    aggregateUtxoCoinByCredential,
    splitTrace,
    forEachEpochTrace,
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..))
import Cardano.Ledger.Block
  ( Block (..),
    bbody,
    bheader,
    neededTxInsForBlock,
  )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (StakeRefBase, StakeRefPtr))
import Cardano.Ledger.EpochBoundary (SnapShot (..), Stake (..), obligation)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking, Witness))
import Cardano.Ledger.Shelley.API (ApplyBlock, ShelleyDELEG)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    IncrementalStake (..),
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PState (..),
    UTxOState (..),
    completeRupd,
    credMap,
    delegations,
    deltaF,
    deltaR,
    deltaT,
    iRReserves,
    iRTreasury,
    incrementalStakeDistr,
    keyRefunds,
    ptrsMap,
    rewards,
    rs,
  )
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules
  ( DelegEnv (..),
    LedgerEnv (..),
    PoolEnv (..),
    ShelleyPOOL,
    votedValue,
  )
import Cardano.Ledger.Shelley.TxBody hiding (TxBody, TxOut)
import Cardano.Ledger.Shelley.UTxO (totalDeposits)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..), coinBalance, txins, txouts)
import Cardano.Ledger.UnifiedMap (ViewMap)
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader
  ( BHeader (..),
    bhbody,
    bheaderSlotNo,
  )
import Cardano.Slotting.Slot (EpochNo)
import Control.Monad.Trans.Reader (ReaderT)
import Control.SetAlgebra (dom, eval, (∩), (▷), (◁))
import Control.State.Transition
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    TraceOrder (OldestFirst),
    sourceSignalTargets,
    splitTrace,
    traceStates,
  )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Default.Class (Default)
import Data.Foldable (fold, foldl', toList)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff.QuickCheck (ediffEq)
import qualified Data.UMap as UM
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Records (HasField (..))
import Lens.Micro hiding (ix)
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.Generator.Block (tickChainState)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Preset (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombinations)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..), totalAda, totalAdaPots)
import qualified Test.Cardano.Ledger.Shelley.Rules.TestDeleg as TestDeleg
  ( checkInstantaneousRewards,
    keyDeRegistration,
    keyDelegation,
    keyRegistration,
    rewardsSumInvariant,
  )
import qualified Test.Cardano.Ledger.Shelley.Rules.TestPool as TestPool
  ( poolRegistration,
    poolRetirement,
    poolStateIsInternallyConsistent,
  )
import qualified Test.Cardano.Ledger.Shelley.Rules.TestPoolreap as TestPoolreap
import Test.Cardano.Ledger.Shelley.Utils
  ( ChainProperty,
    epochFromSlotNo,
    runShelleyBase,
    testGlobals,
  )
import Test.Cardano.Ledger.TerseTools (tersemapdiffs)
import Test.QuickCheck
  ( Property,
    Testable (..),
    conjoin,
    counterexample,
    withMaxSuccess,
    (.&&.),
    (.||.),
    (===),
  )

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

longTraceLen :: Word64
longTraceLen = 150

type TestingLedger era ledger =
  ( BaseM ledger ~ ReaderT Globals Identity,
    Environment ledger ~ LedgerEnv era,
    State ledger ~ LedgerState era,
    Signal ledger ~ Tx era,
    Embed (EraRule "DELEGS" era) ledger,
    Embed (EraRule "UTXOW" era) ledger,
    STS ledger
  )

----------------------------------------------------------------------
-- Properties for Chain
---------------------------------------------------------------------

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
collisionFreeComplete ::
  forall era ledger.
  ( EraGen era,
    ChainProperty era,
    TestingLedger era ledger,
    Default (State (EraRule "PPUP" era)),
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
collisionFreeComplete =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ -- collision freeness
        map (eliminateTxInputs @era @ledger) ssts,
        map (newEntriesAndUniqueTxIns @era @ledger) ssts,
        -- no double spend
        map noDoubleSpend ssts,
        -- tx signatures
        map (requiredMSigSignaturesSubset @era @ledger) ssts
      ]

-- | STAKE INCR
stakeIncrTest ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger,
    State (EraRule "PPUP" era) ~ PPUPState era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
stakeIncrTest =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts = sourceSignalTargets tr

    conjoin . concat $
      [ -- preservation properties
        map (incrStakeComp @era @ledger) ssts
      ]

incrStakeComp ::
  forall era ledger.
  (EraSegWits era, ChainProperty era, TestingLedger era ledger) =>
  SourceSignalTarget (CHAIN era) ->
  Property
incrStakeComp SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map checkIncrStakeComp $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    checkIncrStakeComp :: SourceSignalTarget ledger -> Property
    checkIncrStakeComp
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosStakeDistr = sd} dp,
          signal = tx,
          target = LedgerState UTxOState {utxosUtxo = u', utxosStakeDistr = sd'} dp'
        } =
        counterexample
          ( mconcat
              ( [ "\nDetails:\n",
                  "\ntx\n",
                  show tx,
                  "\nsize original utxo\n",
                  show (Map.size $ unUTxO u),
                  "\noriginal utxo\n",
                  show u,
                  "\noriginal sd\n",
                  show sd,
                  "\nfinal utxo\n",
                  show u',
                  "\nfinal sd\n",
                  show sd',
                  "\noriginal ptrs\n",
                  show ptrs,
                  "\nfinal ptrs\n",
                  show ptrs'
                ]
              )
          )
          $ utxoBal === incrStakeBal
        where
          utxoBal = coinBalance u'
          incrStakeBal = fold (credMap sd') <> fold (ptrMap sd')
          ptrs = ptrsMap . dpsDState $ dp
          ptrs' = ptrsMap . dpsDState $ dp'

-- | Various preservation properties
adaPreservationChain ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger,
    State (EraRule "PPUP" era) ~ PPUPState era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
adaPreservationChain =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts

    conjoin . concat $
      [ -- preservation properties
        map (checkPreservation @era) ssts,
        map (potsSumIncreaseWdrlsPerTx @era @ledger) ssts,
        map (potsSumIncreaseByRewardsPerTx @era @ledger) ssts,
        map (preserveBalance @era @ledger) ssts,
        map (preserveBalanceRestricted @era @ledger) ssts,
        map (preserveOutputsTx @era @ledger) ssts,
        map (potsRewardsDecreaseByWdrlsPerTx @era @ledger) ssts,
        map (canRestrictUTxO @era @ledger) ssts,
        -- well formed deposits
        map nonNegativeDeposits ssts,
        -- non-epoch-boundary preservation properties
        map checkWithdrawlBound noEpochBoundarySsts,
        map (utxoDepositsIncreaseByFeesWithdrawals @era @ledger) noEpochBoundarySsts,
        map potsSumIncreaseWdrlsPerBlock noEpochBoundarySsts,
        map feesNonDecreasing noEpochBoundarySsts
      ]

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation ::
  forall era.
  ( EraSegWits era,
    ShelleyEraTxBody era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    State (EraRule "PPUP" era) ~ PPUPState era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
checkPreservation SourceSignalTarget {source, target, signal} =
  counterexample
    ( mconcat
        ( [ "\nPots before block\n",
            show (totalAdaPots source),
            "\n\nPots after block\n",
            show (totalAdaPots target),
            "\n\nTotal lovelace before block\n",
            show sourceTotal,
            "\n\nTotal lovelace after block\n",
            show targetTotal,
            "\n\nEpoch before block\n",
            show (nesEL . chainNes $ source),
            "\n\nEpoch after block\n",
            show (nesEL . chainNes $ target),
            "\n\nCurrent protocol parameters\n",
            show currPP,
            "\n\nReward Accounts before update\n",
            show (UM.unUnify oldRAs),
            "\n\nReward Accounts after update\n",
            show (UM.unUnify newRAs),
            "\n\nMIR\n",
            show mir,
            "\n\nRegistered Reserves MIR total\n",
            show (fold regMirRes),
            "\n\nUnregistered Reserves MIR total\n",
            show (fold unRegMirRes),
            "\n\nRegistered Treasury MIR total\n",
            show (fold regMirTre),
            "\n\nUnregistered Treasury MIR total\n",
            show (fold unRegMirTre),
            "\n\nPools Retiring This epoch\n",
            show (Map.filter (\e -> e == (nesEL . chainNes $ source)) (psRetiring . dpsPState . lsDPState $ lsOld)),
            "\n\ntxs\n"
          ]
            ++ obligationMsgs
            ++ rewardUpdateMsgs
            ++ txs
        )
    )
    $ sourceTotal === targetTotal
  where
    sourceTotal = totalAda source
    targetTotal = totalAda target

    currPP = esPp . nesEs . chainNes $ source
    prevPP = esPrevPp . nesEs . chainNes $ source

    ru' = nesRu . chainNes $ source
    lsOld = esLState . nesEs . chainNes $ source
    lsNew = esLState . nesEs . chainNes $ target
    pools = psStakePoolParams . dpsPState . lsDPState $ lsOld
    oldRAs = rewards . dpsDState . lsDPState $ lsOld
    newRAs = rewards . dpsDState . lsDPState $ lsNew

    proposal = votedValue (proposals . utxosPpups . lsUTxOState $ lsOld) currPP 5
    obligationMsgs = case proposal of
      Nothing -> []
      Just proposal' ->
        let Coin oblgCurr = obligation currPP oldRAs pools
            Coin oblgNew = obligation proposal' oldRAs pools
            obligationDiff = oblgCurr - oblgNew
         in [ "\n\nProposed protocol parameter update\n",
              show proposal',
              "\n\nObligation Diff\n",
              show obligationDiff
            ]

    mir = dsIRewards . dpsDState . lsDPState $ lsOld
    isRegistered kh _ = UM.member kh oldRAs
    (regMirRes, unRegMirRes) = Map.partitionWithKey isRegistered (iRReserves mir)
    (regMirTre, unRegMirTre) = Map.partitionWithKey isRegistered (iRTreasury mir)

    rewardUpdateMsgs = case ru' of
      SNothing -> []
      SJust ru'' ->
        let (ru, _rewevent) = runShelleyBase (completeRupd ru'')
            regRewards = Map.filterWithKey (\kh _ -> UM.member kh oldRAs) (rs ru)
         in [ "\n\nSum of new rewards\n",
              show (sumRewards prevPP (rs ru)),
              "\n\nNew rewards\n",
              show (rs ru),
              "\n\nSum of new registered rewards\n",
              show (sumRewards prevPP regRewards),
              "\n\nChange in Fees\n",
              show (deltaF ru),
              "\n\nChange in Treasury\n",
              show (deltaT ru),
              "\n\nChange in Reserves\n",
              show (deltaR ru),
              "\n\nNet effect of reward update\n",
              show $
                deltaT ru
                  <> deltaF ru
                  <> deltaR ru
                  <> toDeltaCoin (sumRewards prevPP (rs ru))
            ]

    txs' = toList $ (fromTxSeq @era . bbody) signal
    txs = map dispTx (zip txs' [0 :: Int ..])

    dispTx (tx, ix) =
      "\nTransaction "
        ++ show ix
        ++ "\nfee :"
        ++ show (tx ^. bodyTxL . feeTxBodyL)
        ++ "\nwithdrawals: "
        ++ show (tx ^. bodyTxL . wdrlsTxBodyL)
        ++ "\ncerts: "
        ++ show (map dispCert . toList $ tx ^. bodyTxL . certsTxBodyL)
        ++ "\n"

    dispCert (DCertDeleg (RegKey kh)) = "regkey " <> show kh
    dispCert (DCertDeleg (DeRegKey kh)) = "deregkey " <> show kh
    dispCert (DCertDeleg (Delegate (Delegation _ _))) = "deleg"
    dispCert (DCertPool (RegPool p)) =
      if ppId p `Map.member` pools
        then "PoolReg" <> show (ppId p)
        else "Pool Re-Reg"
    dispCert (DCertPool (RetirePool _ _)) = "retire"
    dispCert (DCertGenesis (GenesisDelegCert _ _ _)) = "gen"
    dispCert (DCertMir _) = "mir"

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawlBound ::
  EraGen era => SourceSignalTarget (CHAIN era) -> Property
checkWithdrawlBound SourceSignalTarget {source, signal, target} =
  counterexample "checkWithdrawlBound" $
    rewardDelta === withdrawals signal
  where
    rewardDelta :: Coin
    rewardDelta =
      fold
        ( rewards
            . dpsDState
            . lsDPState
            . esLState
            . nesEs
            . chainNes
            $ source
        )
        <-> fold
          ( rewards
              . dpsDState
              . lsDPState
              . esLState
              . nesEs
              . chainNes
              $ target
          )

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits)
-- increases by Withdrawals minus Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  counterexample "utxoDepositsIncreaseByFeesWithdrawals" $
    circulation target
      <-> circulation source
      === withdrawals signal
      <-> txFees ledgerTr
  where
    us = lsUTxOState . esLState . nesEs . chainNes
    circulation chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d} = us chainSt
       in coinBalance u <+> d
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger source signal

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWdrlsPerBlock ::
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWdrlsPerBlock SourceSignalTarget {source, signal, target} =
  counterexample
    "potsSumIncreaseWdrlsPerBlock"
    $ potsSum target <-> potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in coinBalance u <+> d <+> f

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals in a transaction
potsSumIncreaseWdrlsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseWdrlsPerTx" $
    conjoin $
      map sumIncreaseWdrls $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseWdrls :: SourceSignalTarget ledger -> Property
    sumIncreaseWdrls
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} _,
          signal = tx,
          target = LedgerState UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'} _
        } =
        property (hasFailedScripts tx)
          .||. (coinBalance u' <+> d' <+> f')
            <-> (coinBalance u <+> d <+> f)
            === fold (unWdrl (tx ^. bodyTxL . wdrlsTxBodyL))

-- | (Utxo + Deposits + Fees) increases by the reward delta
potsSumIncreaseByRewardsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    EraSegWits era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseByRewardsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseByRewardsPerTx" $
    conjoin $
      map sumIncreaseRewards $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseRewards
      SourceSignalTarget
        { source =
            LedgerState
              UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f}
              DPState {dpsDState = DState {dsUnified = umap1}},
          target =
            LedgerState
              UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'}
              DPState {dpsDState = DState {dsUnified = umap2}}
        } =
        (coinBalance u' <+> d' <+> f')
          <-> (coinBalance u <+> d <+> f)
          === fold (UM.unUnify (UM.Rewards umap1))
          <-> fold (UM.unUnify (UM.Rewards umap2))

-- | The Rewards pot decreases by the sum of withdrawals in a transaction
potsRewardsDecreaseByWdrlsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsRewardsDecreaseByWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsRewardsDecreaseByWdrlsPerTx" $
    conjoin $
      map rewardsDecreaseByWdrls $
        sourceSignalTargets ledgerTr
  where
    rewardsSum = fold . rewards . dpsDState
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    rewardsDecreaseByWdrls
      SourceSignalTarget
        { source = LedgerState _ dpstate,
          signal = tx,
          target = LedgerState _ dpstate'
        } =
        let totalRewards = rewardsSum dpstate
            totalRewards' = rewardsSum dpstate'
            txWithdrawals = fold (unWdrl (tx ^. bodyTxL . wdrlsTxBodyL))
         in conjoin
              [ counterexample
                  "A transaction should not increase the Rewards pot"
                  (totalRewards >= totalRewards'),
                counterexample
                  "Withdrawals should be non-negative"
                  (txWithdrawals >= Coin 0),
                counterexample
                  "Rewards should increase by withdrawals"
                  (hasFailedScripts tx || totalRewards <-> totalRewards' == txWithdrawals)
              ]

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalance SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalance" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = ledgerSt, signal = tx, target = ledgerSt'} =
      counterexample
        "preserveBalance created /= consumed ... "
        (failedScripts .||. ediffEq created consumed_)
      where
        failedScripts = property $ hasFailedScripts tx
        LedgerState (UTxOState {utxosUtxo = u}) dstate = ledgerSt
        LedgerState (UTxOState {utxosUtxo = u'}) _ = ledgerSt'
        txb = tx ^. bodyTxL
        certs = toList (txb ^. certsTxBodyL)
        pools = psStakePoolParams . dpsPState $ dstate
        created =
          coinBalance u'
            <+> txb ^. feeTxBodyL
            <+> totalDeposits pp_ (`Map.notMember` pools) certs
        consumed_ =
          coinBalance u
            <+> keyRefunds pp_ txb
            <+> fold (unWdrl (txb ^. wdrlsTxBodyL))

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger,
    ShelleyEraTxBody era,
    EraSegWits era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalanceRestricted SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalanceRestricted" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = UTxO u}) dstate,
          signal = tx
        } =
        inps === outs
        where
          txb = tx ^. bodyTxL
          pools = psStakePoolParams . dpsPState $ dstate
          inps =
            coinBalance @era (UTxO (Map.restrictKeys u (txb ^. inputsTxBodyL)))
              <> keyRefunds pp_ txb
              <> fold (unWdrl (txb ^. wdrlsTxBodyL))
          outs =
            let certs = toList (txb ^. certsTxBodyL)
             in coinBalance (txouts @era txb)
                  <> txb ^. feeTxBodyL
                  <> totalDeposits pp_ (`Map.notMember` pools) certs

preserveOutputsTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveOutputsTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveOutputsTx" $
    conjoin $
      map outputPreserved $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    outputPreserved
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = UTxO utxo}) _,
          signal = tx
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
         in property $
              hasFailedScripts tx
                .||. counterexample "TxOuts are not a subset of UTxO" (outs `Map.isSubmapOf` utxo)

canRestrictUTxO ::
  forall era ledger.
  ( ChainProperty era,
    EraSegWits era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
canRestrictUTxO SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "canRestrictUTxO" $
    conjoin $
      zipWith
        outputPreserved
        (sourceSignalTargets ledgerTrFull)
        (sourceSignalTargets ledgerTrRestr)
  where
    (_, ledgerTrFull) = ledgerTraceFromBlock @era @ledger chainSt block
    (UTxO irrelevantUTxO, ledgerTrRestr) =
      ledgerTraceFromBlockWithRestrictedUTxO @era @ledger chainSt block
    outputPreserved
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uFull}) _}
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uRestr}) _} =
        counterexample
          (unlines ["non-disjoint:", show uRestr, show irrelevantUTxO])
          (uRestr `Map.disjoint` irrelevantUTxO)
          .&&. uFull === (uRestr `Map.union` irrelevantUTxO)

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "eliminateTxInputs" $
    conjoin $
      map inputsEliminated $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    inputsEliminated
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = (UTxO u')}) _,
          signal = tx
        } =
        property $
          hasFailedScripts tx
            || Set.null (eval (txins @era (tx ^. bodyTxL) ∩ Map.keysSet u'))

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
newEntriesAndUniqueTxIns SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "newEntriesAndUniqueTxIns" $
    conjoin $
      map newEntryPresent $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    newEntryPresent
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = UTxO u}) _,
          signal = tx,
          target = LedgerState (UTxOState {utxosUtxo = UTxO u'}) _
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
            outIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet u)
         in property $
              hasFailedScripts tx
                || ((outIds `Set.disjoint` oldIds) && (outs `Map.isSubmapOf` u'))

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
requiredMSigSignaturesSubset ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
requiredMSigSignaturesSubset SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "requiredMSigSignaturesSubset" $
    conjoin $
      map signaturesSubset $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    signaturesSubset :: SourceSignalTarget ledger -> Property
    signaturesSubset SourceSignalTarget {signal = tx} =
      let khs = keyHashSet tx
       in property $
            all (existsReqKeyComb khs) (tx ^. witsTxL . scriptTxWitsL)

    existsReqKeyComb keyHashes msig =
      any (\kl -> Set.fromList kl `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
    keyHashSet :: Tx era -> Set (KeyHash 'Witness (EraCrypto era))
    keyHashSet tx_ =
      Set.map witVKeyHash (tx_ ^. witsTxL . addrTxWitsL)

--- | Check for absence of double spend in a block
noDoubleSpend ::
  forall era.
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
noDoubleSpend SourceSignalTarget {signal} =
  counterexample "noDoubleSpend" $
    [] === getDoubleInputs txs
  where
    txs = toList $ (fromTxSeq @era . bbody) signal

    getDoubleInputs :: [Tx era] -> [(Tx era, [Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx era -> [Tx era] -> [(Tx era, [Tx era])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      [(tx_j, doubles) | not (null doubles)]
      where
        doubles =
          if hasFailedScripts tx_j
            then []
            else
              filter
                ( \tx_i ->
                    not
                      ( hasFailedScripts tx_i
                          || Set.disjoint inps_j (tx_i ^. bodyTxL . inputsTxBodyL)
                      )
                )
                ts
        inps_j = tx_j ^. bodyTxL . inputsTxBodyL

withdrawals ::
  forall era.
  EraGen era =>
  Block (BHeader (EraCrypto era)) era ->
  Coin
withdrawals (UnserialisedBlock _ txseq) =
  foldl'
    ( \c tx ->
        let wdrls = unWdrl $ tx ^. bodyTxL . wdrlsTxBodyL
         in if hasFailedScripts tx then c else c <> fold wdrls
    )
    (Coin 0)
    $ fromTxSeq @era txseq

txFees ::
  forall era ledger.
  (EraGen era, TestingLedger era ledger) =>
  Trace ledger ->
  Coin
txFees ledgerTr =
  foldl' f (Coin 0) (sourceSignalTargets ledgerTr)
  where
    f
      c
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = utxo} _,
          signal = tx
        } = c <> feeOrCollateral tx utxo

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {utxosDeposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Checks that the fees are non-decreasing when not at an epoch boundary
feesNonDecreasing ::
  SourceSignalTarget (CHAIN era) ->
  Property
feesNonDecreasing SourceSignalTarget {source, target} =
  counterexample ("feesNonDecreasing: " <> show (fees_ source) <> " <= " <> show (fees_ target)) $
    fees_ source <= fees_ target
  where
    fees_ chainSt =
      let UTxOState {utxosFees = fees} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in fees

----------------------------------------------------------------------
-- POOL Properties
----------------------------------------------------------------------

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
poolProperties ::
  forall era.
  ( EraGen era,
    Default (State (EraRule "PPUP" era)),
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
poolProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ map poolRetirement ssts,
        map poolRegistration ssts,
        map poolStateIsInternallyConsistent ssts
      ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolRetirement ::
  ( ChainProperty era,
    EraSegWits era,
    ShelleyEraTxBody era,
    HasField "_eMax" (PParams era) EpochNo,
    HasField "_minPoolCost" (PParams era) Coin
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRetirement SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map (TestPool.poolRetirement currentEpoch maxEpoch) (sourceSignalTargets poolTr)
  where
    (chainSt', poolTr) = poolTraceFromBlock chainSt block
    bhb = bhbody $ bheader block
    currentEpoch = (epochFromSlotNo . bheaderSlotNo) bhb
    maxEpoch = (getField @"_eMax" . esPp . nesEs . chainNes) chainSt'

-- | Check that a newly registered pool key is registered and not
-- in the retiring map.
poolRegistration ::
  ( ChainProperty era,
    EraSegWits era,
    ShelleyEraTxBody era,
    HasField "_eMax" (PParams era) EpochNo,
    HasField "_minPoolCost" (PParams era) Coin
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRegistration (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map TestPool.poolRegistration (sourceSignalTargets poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
poolStateIsInternallyConsistent ::
  ( ChainProperty era,
    EraSegWits era,
    ShelleyEraTxBody era,
    HasField "_eMax" (PParams era) EpochNo,
    HasField "_minPoolCost" (PParams era) Coin
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolStateIsInternallyConsistent (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map TestPool.poolStateIsInternallyConsistent (traceStates OldestFirst poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

----------------------------------------------------------------------
-- DELEG Properties
----------------------------------------------------------------------

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
delegProperties ::
  forall era.
  ( EraGen era,
    Default (State (EraRule "PPUP" era)),
    QC.HasTrace (CHAIN era) (GenEnv era),
    ChainProperty era
  ) =>
  Property
delegProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    conjoin $
      map chainProp (sourceSignalTargets tr)
  where
    delegProp :: DelegEnv era -> SourceSignalTarget (ShelleyDELEG era) -> Property
    delegProp denv delegSst =
      conjoin $
        [ TestDeleg.keyRegistration delegSst,
          TestDeleg.keyDeRegistration delegSst,
          TestDeleg.keyDelegation delegSst,
          TestDeleg.rewardsSumInvariant delegSst,
          TestDeleg.checkInstantaneousRewards denv delegSst
        ]
    chainProp :: SourceSignalTarget (CHAIN era) -> Property
    chainProp (SourceSignalTarget {source = chainSt, signal = block}) =
      let delegInfo = delegTraceFromBlock chainSt block
          delegEnv = fst delegInfo
          delegTr = snd delegInfo
          delegSsts = sourceSignalTargets delegTr
       in conjoin (map (delegProp delegEnv) delegSsts)

----------------------------------------------------------------------
-- Projections of CHAIN Trace
----------------------------------------------------------------------

-- | Reconstruct a LEDGER trace from the transactions in a Block and ChainState
ledgerTraceFromBlock ::
  forall era ledger.
  ( ChainProperty era,
    EraSegWits era,
    TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace ledger)
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0 txs
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block

-- | This function is nearly the same as ledgerTraceFromBlock, but
-- it restricts the UTxO state to only those needed by the block.
-- It also returns the unused UTxO for comparison later.
ledgerTraceFromBlockWithRestrictedUTxO ::
  forall era ledger.
  ( ChainProperty era,
    EraSegWits era,
    TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (UTxO era, Trace ledger)
ledgerTraceFromBlockWithRestrictedUTxO chainSt block =
  ( UTxO irrelevantUTxO,
    runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0' txs
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    txIns = neededTxInsForBlock block
    LedgerState utxoSt delegationSt = ledgerSt0
    utxo = unUTxO . utxosUtxo $ utxoSt
    (relevantUTxO, irrelevantUTxO) = Map.partitionWithKey (const . (`Set.member` txIns)) utxo
    ledgerSt0' = LedgerState (utxoSt {utxosUtxo = UTxO relevantUTxO}) delegationSt

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
poolTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    ShelleyEraTxBody era,
    EraSegWits era,
    HasField "_eMax" (PParams era) EpochNo,
    HasField "_minPoolCost" (PParams era) Coin
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace (ShelleyPOOL era))
poolTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(ShelleyPOOL era) poolEnv poolSt0 poolCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (toList . view certsTxBodyL . view bodyTxL)
    poolCerts = filter poolCert (certs txs)
    poolEnv =
      let (LedgerEnv s _ pp _) = ledgerEnv
       in PoolEnv s pp
    poolSt0 =
      let LedgerState _ (DPState _ poolSt0_) = ledgerSt0
       in poolSt0_
    poolCert (DCertPool _) = True
    poolCert _ = False

-- | Reconstruct a DELEG trace from all the transaction certificates in a Block
delegTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    ShelleyEraTxBody era,
    EraSegWits era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (DelegEnv era, Trace (ShelleyDELEG era))
delegTraceFromBlock chainSt block =
  ( delegEnv,
    runShelleyBase $
      Trace.closure @(ShelleyDELEG era) delegEnv delegSt0 blockCerts
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (reverse . toList . view certsTxBodyL . view bodyTxL)
    blockCerts = filter delegCert (certs txs)
    delegEnv =
      let (LedgerEnv s txIx pp reserves) = ledgerEnv
          dummyCertIx = minBound
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves pp
    delegSt0 =
      let LedgerState _ (DPState delegSt0_ _) = ledgerSt0
       in delegSt0_
    delegCert (DCertDeleg _) = True
    delegCert (DCertMir _) = True
    delegCert _ = False

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
--
-- NOTE: we need to tick the slot before processing transactions
-- (in the same way that the CHAIN rule TICKs the slot before processing
-- transactions with the LEDGERS rule)
ledgerTraceBase ::
  forall era.
  ( EraSegWits era,
    GetLedgerView era,
    ApplyBlock era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, LedgerEnv era, LedgerState era, [Tx era])
ledgerTraceBase chainSt block =
  ( tickedChainSt,
    LedgerEnv slot minBound pp_ (esAccountState nes),
    esLState nes,
    txs
  )
  where
    (UnserialisedBlock (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = esPp nes
    -- Oldest to Newest first
    txs = (reverse . toList . fromTxSeq) txSeq -- HERE WE USE SOME SegWit function

-- | Transform the [(source, signal, target)] of a CHAIN Trace
-- by manually applying the Chain TICK Rule to each source and producing
-- [(source, signal, target')].
--
-- This allows for testing properties on Chain traces while excluding the effects
-- of Transactions and Certificates. For example we can check that pools that are
-- due for retirement at an epoch transition, are indeed retired.
--
-- Had we not excluded the effects of Transactions/Certificates, we might have
-- a pool that was correctly retired, but is again registered by a certificate
-- in the block following the transition.
chainSstWithTick ::
  forall era.
  ChainProperty era =>
  Trace (CHAIN era) ->
  [SourceSignalTarget (CHAIN era)]
chainSstWithTick ledgerTr =
  map applyTick (sourceSignalTargets ledgerTr)
  where
    applyTick sst@SourceSignalTarget {source = chainSt, signal = block} =
      let bh = bheader block
          slot = (bheaderSlotNo . bhbody) bh
       in sst {target = tickChainState @era slot chainSt}

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

removedAfterPoolreap ::
  forall era.
  ( ChainProperty era,
    Default (State (EraRule "PPUP" era)),
    EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
removedAfterPoolreap =
  forAllChainTrace traceLen $ \tr ->
    conjoin $
      map removedAfterPoolreap_ $
        filter (not . sameEpoch) (chainSstWithTick tr)
  where
    poolState = dpsPState . lsDPState . esLState . nesEs . chainNes

    removedAfterPoolreap_ :: SourceSignalTarget (CHAIN era) -> Property
    removedAfterPoolreap_ (SourceSignalTarget {source, target, signal = (UnserialisedBlock bh _)}) =
      let e = (epochFromSlotNo . bheaderSlotNo . bhbody) bh
       in TestPoolreap.removedAfterPoolreap (poolState source) (poolState target) e

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  forall era prop.
  ( Testable prop,
    Default (State (EraRule "PPUP" era)),
    EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Word64 -> -- trace length
  (Trace (CHAIN era) -> prop) ->
  Property
forAllChainTrace n prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState
      testGlobals
      n
      (Preset.genEnv p)
      (Just $ mkGenesisChainState (Preset.genEnv p))
      prop
  where
    p :: Proxy era
    p = Proxy

sameEpoch ::
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes

-- | Test a property on the first 'subtracecount' sub-Traces that end on an EpochBoundary
forEachEpochTrace ::
  forall era prop.
  ( EraGen era,
    Testable prop,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (EraRule "PPUP" era))
  ) =>
  Int ->
  Word64 ->
  (Trace (CHAIN era) -> prop) ->
  Property
forEachEpochTrace subtracecount tracelen f = forAllChainTrace tracelen action
  where
    -- split at contiguous elements with different Epoch numbers
    p new old = (nesEL . chainNes) new /= (nesEL . chainNes) old
    -- At a minimum throw away the last trace which is probably an incomplete epoch
    action tr = conjoin $ map f (take (min subtracecount (m - 1)) (reverse traces))
      where
        traces = splitTrace p tr
        m = length traces

-- ============================================================
-- Properties for Incremental Stake Distribution  Calculation

atEpoch ::
  forall era prop.
  ( EraGen era,
    Testable prop,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (EraRule "PPUP" era))
  ) =>
  (LedgerState era -> LedgerState era -> prop) ->
  Property
atEpoch f =
  forAllChainTrace traceLen $ \tr ->
    conjoin $
      map g $
        filter (not . sameEpoch) (sourceSignalTargets tr)
  where
    g (SourceSignalTarget s1 s2 _) = f (ledgerStateFromChainState s1) (ledgerStateFromChainState s2)

ledgerStateFromChainState :: ChainState era -> LedgerState era
ledgerStateFromChainState = esLState . nesEs . chainNes

testIncrementalStake ::
  forall era.
  EraTxOut era =>
  LedgerState era ->
  LedgerState era ->
  Property
testIncrementalStake _ (LedgerState (UTxOState utxo _ _ _ incStake) (DPState dstate pstate)) =
  let stake = stakeDistr @era utxo dstate pstate

      istake = incrementalStakeDistr @(EraCrypto era) incStake dstate pstate
   in counterexample
        ( "\nIncremental stake distribution does not match old style stake distribution"
            ++ tersediffincremental "differences: Old vs Incremental" (ssStake stake) (ssStake istake)
        )
        (stake === istake)

incrementalStakeProp ::
  forall era.
  ( EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (EraRule "PPUP" era))
  ) =>
  Proxy era ->
  Property
incrementalStakeProp Proxy = atEpoch @era (testIncrementalStake @era)

tersediffincremental :: String -> Stake c -> Stake c -> String
tersediffincremental message (Stake a) (Stake c) =
  tersemapdiffs (message ++ " " ++ "hashes") (mp a) (mp c)
  where
    mp = Map.map fromCompact . VMap.toMap

-- | Compute the current Stake Distribution. This was called at the Epoch boundary in the Snap Rule.
--   Now it is called in the tests to see that its incremental analog 'incrementalStakeDistr' agrees.
stakeDistr ::
  forall era.
  EraTxOut era =>
  UTxO era ->
  DState (EraCrypto era) ->
  PState (EraCrypto era) ->
  SnapShot (EraCrypto era)
stakeDistr u ds ps =
  SnapShot
    (Stake $ VMap.fromMap (compactCoinOrError <$> eval (dom activeDelegs ◁ stakeRelation)))
    (VMap.fromMap (UM.unUnify delegs))
    (VMap.fromMap poolParams)
  where
    rewards' = rewards ds
    delegs = delegations ds
    ptrs' = ptrsMap ds
    PState poolParams _ _ = ps
    stakeRelation :: Map (Credential 'Staking (EraCrypto era)) Coin
    stakeRelation = aggregateUtxoCoinByCredential ptrs' u (UM.unUnify rewards')
    activeDelegs :: ViewMap (EraCrypto era) (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    activeDelegs = eval ((dom rewards' ◁ delegs) ▷ dom poolParams)
    compactCoinOrError c =
      case toCompact c of
        Nothing -> error $ "Invalid ADA value in staking: " <> show c
        Just compactCoin -> compactCoin

-- | Sum up all the Coin for each staking Credential. This function has an
--   incremental analog. See 'incrementalAggregateUtxoCoinByCredential'
aggregateUtxoCoinByCredential ::
  forall era.
  EraTxOut era =>
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  UTxO era ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) Coin
aggregateUtxoCoinByCredential ptrs (UTxO u) initial =
  Map.foldl' accum initial u
  where
    accum ans out =
      let c = out ^. coinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p)
              | Just cred <- Map.lookup p ptrs -> Map.insertWith (<>) cred c ans
            Addr _ _ (StakeRefBase hk) -> Map.insertWith (<>) hk c ans
            _other -> ans
