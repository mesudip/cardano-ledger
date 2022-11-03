{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    DState (..),
    PState (..),
    InstantaneousRewards (..),
    FutureGenDeleg (..),
    rewards,
    delegations,
    ptrsMap,
    payKeyDeposit,
    payPoolDeposit,
    refundKeyDeposit,
    refundPoolDeposit,
    obligationDPState,
    deltaTxDeposit,
    deltaCertsDeposit,
    totalTxDeposits,
    totalCertsDeposits,
    keyTxRefunds,
    keyCertsRefunds,
    depositTest,
  )
where

import Cardano.Ledger.Binary
  ( FromCBOR (..),
    FromSharedCBOR (..),
    Interns,
    ToCBOR (..),
    decodeRecordNamed,
    decodeRecordNamedT,
    encodeListLen,
    fromNotSharedCBOR,
    fromSharedPlusCBOR,
    fromSharedPlusLensCBOR,
    toMemptyLens,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
  )
import Cardano.Ledger.Core (EraCrypto, PParams, TxBody)
import Cardano.Ledger.Credential (Credential (..), Ptr)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), isRegKey)
import Cardano.Ledger.Shelley.TxBody
  ( PoolParams (..),
    ShelleyEraTxBody,
    certsTxBodyL,
    pattern DeRegKey,
    pattern RegKey,
    pattern RegPool,
  )
import Cardano.Ledger.Slot
  ( EpochNo (..),
    SlotNo (..),
  )
import Cardano.Ledger.UnifiedMap (UMap (UnifiedMap), UnifiedMap, View (Delegations, Rewards), ViewMap)
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Default.Class (Default (def))
import Data.Foldable (foldl', toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.UMap as UM
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Lens.Micro ((^.), _1, _2)
import NoThunks.Class (NoThunks (..))

-- ======================================

data FutureGenDeleg c = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo,
    fGenDelegGenKeyHash :: !(KeyHash 'Genesis c)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureGenDeleg c)

instance NFData (FutureGenDeleg c)

instance CC.Crypto c => ToCBOR (FutureGenDeleg c) where
  toCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance CC.Crypto c => FromCBOR (FutureGenDeleg c) where
  fromCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> fromCBOR <*> fromCBOR

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards c = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking c) Coin),
    iRTreasury :: !(Map (Credential 'Staking c) Coin),
    deltaReserves :: !DeltaCoin,
    deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c)

instance NFData (InstantaneousRewards c)

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState c = DState
  { -- | Unified Reward Maps. This contains the reward map (which is the source
    -- of truth regarding the registered stake credentials, the delegation map,
    -- and the stake credential pointer map.
    dsUnified :: !(UnifiedMap c),
    -- | Future genesis key delegations
    dsFutureGenDelegs :: !(Map (FutureGenDeleg c) (GenDelegPair c)),
    -- | Genesis key delegations
    dsGenDelegs :: !(GenDelegs c),
    -- | Instantaneous Rewards
    dsIRewards :: !(InstantaneousRewards c),
    -- | The Deposit map for staking credentials
    dsDeposits :: !(Map (Credential 'Staking c) Coin)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DState c)

instance NFData (InstantaneousRewards c) => NFData (DState c)

instance (CC.Crypto c, ToCBOR (InstantaneousRewards c)) => ToCBOR (DState c) where
  toCBOR (DState unified fgs gs ir ds) =
    encodeListLen 5
      <> toCBOR unified
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir
      <> toCBOR ds

instance (CC.Crypto c, FromSharedCBOR (InstantaneousRewards c)) => FromSharedCBOR (DState c) where
  type
    Share (DState c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  fromSharedPlusCBOR =
    decodeRecordNamedT "DState" (const 5) $ do
      unified <- fromSharedPlusCBOR
      fgs <- lift fromCBOR
      gs <- lift fromCBOR
      ir <- fromSharedPlusLensCBOR _1
      ds <- fromSharedPlusLensCBOR (_1 . toMemptyLens _1 id)
      pure $ DState unified fgs gs ir ds

-- | The state used by the POOL rule, which tracks stake pool information.
data PState c = PState
  { -- | The stake pool parameters.
    psStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | The future stake pool parameters.
    -- Changes to existing stake pool parameters are staged in order
    -- to give delegators time to react to changes.
    -- See section 11.2, "Example Illustration of the Reward Cycle",
    -- of the Shelley Ledger Specification for a sequence diagram.
    psFutureStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | A map of retiring stake pools to the epoch when they retire.
    psRetiring :: !(Map (KeyHash 'StakePool c) EpochNo),
    -- | A map of the deposits for each pool
    psDeposits :: !(Map (KeyHash 'StakePool c) Coin)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState c)

instance NFData (PState c)

instance CC.Crypto c => ToCBOR (PState c) where
  toCBOR (PState a b c d) =
    encodeListLen 4 <> toCBOR a <> toCBOR b <> toCBOR c <> toCBOR d

instance CC.Crypto c => FromSharedCBOR (PState c) where
  type
    Share (PState c) =
      Interns (KeyHash 'StakePool c)
  fromSharedPlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits}

instance (CC.Crypto c, FromSharedCBOR (PState c)) => FromCBOR (PState c) where
  fromCBOR = fromNotSharedCBOR

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data DPState c = DPState
  { dpsDState :: !(DState c),
    dpsPState :: !(PState c)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DPState c)

instance NFData (InstantaneousRewards c) => NFData (DPState c)

instance CC.Crypto c => ToCBOR (InstantaneousRewards c) where
  toCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> toCBOR irR <> toCBOR irT <> toCBOR dR <> toCBOR dT

instance CC.Crypto c => FromSharedCBOR (InstantaneousRewards c) where
  type Share (InstantaneousRewards c) = Interns (Credential 'Staking c)
  fromSharedPlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      irT <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      dR <- lift fromCBOR
      dT <- lift fromCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance
  CC.Crypto c =>
  ToCBOR (DPState c)
  where
  toCBOR DPState {dpsPState, dpsDState} =
    encodeListLen 2
      <> toCBOR dpsPState -- We get better sharing when encoding pstate before dstate
      <> toCBOR dpsDState

instance CC.Crypto c => FromSharedCBOR (DPState c) where
  type
    Share (DPState c) =
      ( Interns (Credential 'Staking c),
        Interns (KeyHash 'StakePool c)
      )
  fromSharedPlusCBOR = decodeRecordNamedT "DPState" (const 2) $ do
    dpsPState <- fromSharedPlusLensCBOR _2
    dpsDState <- fromSharedPlusCBOR
    pure DPState {dpsPState, dpsDState}

instance Default (DPState c) where
  def = DPState def def

instance Default (InstantaneousRewards c) where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState c) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def
      Map.empty

instance Default (PState c) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState c -> ViewMap c (Credential 'Staking c) Coin
rewards (DState unified _ _ _ _) = Rewards unified

delegations ::
  DState c ->
  ViewMap c (Credential 'Staking c) (KeyHash 'StakePool c)
delegations (DState unified _ _ _ _) = Delegations unified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState c -> Map Ptr (Credential 'Staking c)
ptrsMap (DState (UnifiedMap _ ptrmap) _ _ _ _) = ptrmap

-- ==========================================================
-- Functions that handle Deposits for stake credetials and key hashes.

-- | One only pays a deposit on the initial key registration. If the key has been
--   de-registered it should have been removed from the map. If it hasn't been
--   de-registered, then it has no effect on the Deposits. In places where this function
--   is called, there should be an explicit check that the credential is not in the map.
payKeyDeposit ::
  HasField "_keyDeposit" (PParams era) Coin =>
  Credential 'Staking (EraCrypto era) ->
  PParams era ->
  DState (EraCrypto era) ->
  DState (EraCrypto era)
payKeyDeposit cred pp dstate = dstate {dsDeposits = newStake}
  where
    stake = dsDeposits dstate
    newStake = case Map.lookup cred stake of
      Nothing -> Map.insert cred (getField @"_keyDeposit" pp) stake
      Just _ -> stake

refundKeyDeposit :: Credential 'Staking c -> DState c -> (Coin, DState c)
refundKeyDeposit cred dstate =
  (coin, dstate {dsDeposits = newStake})
  where
    stake = dsDeposits dstate
    (coin, newStake) = case Map.lookup cred stake of
      Just c -> (c, Map.delete cred stake)
      Nothing -> (mempty, stake)

-- | One only pays a deposit on the initial pool registration. So return the
--   the Deposits unchanged if the keyhash already exists. There are legal
--   situations where a pool may be registered multiple times.
payPoolDeposit ::
  HasField "_poolDeposit" (PParams era) Coin =>
  KeyHash 'StakePool (EraCrypto era) ->
  PParams era ->
  PState (EraCrypto era) ->
  PState (EraCrypto era)
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    newpool = case Map.lookup keyhash pool of
      Nothing -> Map.insert keyhash (getField @"_poolDeposit" pp) pool
      Just _ -> pool -- Should it be overwritten with the current  (getField @"_poolDeposit" pp) ?
      -- things are simpler if it is not.

refundPoolDeposit :: KeyHash 'StakePool c -> PState c -> (Coin, PState c)
refundPoolDeposit keyhash pstate = (coin, pstate {psDeposits = newpool})
  where
    pool = psDeposits pstate
    (coin, newpool) = case Map.lookup keyhash pool of
      Just c -> (c, Map.delete keyhash pool)
      Nothing -> (mempty, pool)

-- | Calculate total possible refunds in the system
obligationDPState :: DPState era -> Coin
obligationDPState (DPState DState {dsDeposits = keys} PState {psDeposits = stakePools}) =
  foldl' (<>) (Coin 0) keys <> foldl' (<>) (Coin 0) stakePools

{-
obligation ::
  forall c pp t.
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    Foldable (t (Credential 'Staking c))
  ) =>
  pp ->
  t (Credential 'Staking c) Coin ->
  Map (KeyHash 'StakePool c) (PoolParams c) ->
  Coin
obligation pp rewards stakePools =
  (length rewards <×> getField @"_keyDeposit" pp)
    <+> (length stakePools <×> getField @"_poolDeposit" pp)
-}

-- | Determine the total deposit amount needed from a TxBody.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration. It is even possible for a single
-- transaction to have multiple pool registration for the same pool, so as
-- we process pool registrations, we must keep track of those that are already
-- registered, so we do not add a Deposit for the same pool twice.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
totalCertsDeposits ::
  forall c pp.
  ( HasField "_poolDeposit" pp Coin,
    HasField "_keyDeposit" pp Coin
  ) =>
  pp ->
  DPState c ->
  [DCert c] ->
  Coin
totalCertsDeposits pp dpstate certs =
  numKeys <×> getField @"_keyDeposit" pp
    <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    numKeys = length $ filter isRegKey certs
    regpools = psStakePoolParams (dpsPState dpstate)
    accum (pools, ans) (DCertPool (RegPool poolparam)) =
      if Map.member (ppId poolparam) pools -- We don't pay a deposit on a pool that is already registered
        then (pools, ans)
        else (Map.insert (ppId poolparam) poolparam pools, ans <+> getField @"_poolDeposit" pp)
    accum ans _ = ans

totalTxDeposits ::
  forall era pp.
  ( HasField "_poolDeposit" pp Coin,
    HasField "_keyDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  pp ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
totalTxDeposits pp dpstate txb = totalCertsDeposits pp dpstate (toList $ txb ^. certsTxBodyL)

-- | Compute the key deregistration refunds in a transaction
keyCertsRefunds ::
  HasField "_keyDeposit" pp Coin =>
  pp ->
  DPState c ->
  [DCert c] ->
  Coin
keyCertsRefunds pp dpstate certs = snd (foldl' accum (regkeys, Coin 0) certs)
  where
    regkeys = dsDeposits (dpsDState dpstate)
    accum (keys, ans) (DCertDeleg (RegKey k)) =
      -- Deposit is added locally to the growing 'keys'
      (Map.insert k (getField @"_keyDeposit" pp) keys, ans)
    accum (keys, ans) (DCertDeleg (DeRegKey k)) =
      -- If the key is registered, lookup the deposit in the locally growing 'keys'
      -- if it is not registered, then just return ans
      case Map.lookup k keys of
        Just deposit -> (keys, ans <+> deposit)
        Nothing -> (keys, ans)
    accum ans _ = ans

keyTxRefunds ::
  (ShelleyEraTxBody era, HasField "_keyDeposit" pp Coin) =>
  pp ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
keyTxRefunds pp dpstate tx = keyCertsRefunds pp dpstate (toList $ tx ^. certsTxBodyL)

-- | Compute the change in the deposit caused by 'running' the certificates of one Tx
--   This is a replacement function for 'totalDeposits', and 'keyRefunds' which did not take
--   the DPState as input, and thus could not accurately compute the answer.
--   Note that the set of registered keys and registered pools can grow when we see RegKey and RegPool
--   We have to track this properly because a Tx may have both a RegKey and a DeRegKey
deltaCertsDeposit ::
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin
  ) =>
  pp ->
  DPState c ->
  StrictSeq (DCert c) ->
  Coin
deltaCertsDeposit pp dpstate certs = (\(_, _, c) -> c) $ foldl' accum (regpools, regkeys, Coin 0) certs
  where
    regpools = psStakePoolParams (dpsPState dpstate)
    regkeys = dsDeposits (dpsDState dpstate)
    accum (pools, keys, ans) (DCertDeleg (DeRegKey k)) =
      -- If the key is registered subtract whatever deposit that was initially made from ans
      -- if it is not registered, then just return ans
      case Map.lookup k keys of
        Just deposit -> (pools, keys, ans <-> deposit)
        Nothing -> (pools, keys, ans)
    accum (pools, keys, ans) (DCertDeleg (RegKey k)) =
      (pools, Map.insert k (getField @"_keyDeposit" pp) keys, ans <+> getField @"_keyDeposit" pp)
    accum (pools, keys, ans) (DCertPool (RegPool poolparam)) =
      if Map.member (ppId poolparam) pools -- We don't pay a deposit on a pool that is already registered
        then (pools, keys, ans)
        else (Map.insert (ppId poolparam) poolparam pools, keys, ans <+> getField @"_poolDeposit" pp)
    accum ans _ = ans

deltaTxDeposit ::
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  pp ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
deltaTxDeposit pp dpstate txb = deltaCertsDeposit pp dpstate (txb ^. certsTxBodyL)

depositTest ::
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  pp ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Bool
depositTest pp dpstate txb =
  deltaTxDeposit pp dpstate txb == totalTxDeposits pp dpstate txb <-> keyTxRefunds pp dpstate txb
