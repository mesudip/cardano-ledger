{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.LedgerState.ToExprOrphans where

-- ===========================================
-- ToExpr instances
-- ===========================================

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Class ()
-- Show instance of Hash

import Cardano.Ledger.Address (Addr, BootstrapAddress, RewardAcnt)
import Cardano.Ledger.BaseTypes (BlocksMade, ProtVer)
-- import GHC.Num.Natural(Natural)
import Cardano.Ledger.Binary.Version (Version, getVersion64)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Compactible (CompactForm (fromCompact))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference)
import Cardano.Ledger.EpochBoundary (SnapShot, SnapShots, Stake)
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs, KeyHash (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake, PoolDistr)
import Cardano.Ledger.PoolParams (PoolMetadata, PoolParams, StakePoolRelay)
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState.DPState (DPState, DState, FutureGenDeleg, InstantaneousRewards, PState)
import Cardano.Ledger.Shelley.LedgerState.Types
  ( AccountState,
    EpochState,
    IncrementalStake,
    LedgerState,
    NewEpochState,
    StashedAVVMAddresses,
    UTxOState,
  )
import Cardano.Ledger.Shelley.PParams (PPUPState, ProposedPPUpdates, ShelleyPParamsHKD)
import Cardano.Ledger.Shelley.PoolRank (Likelihood, LogWeight, NonMyopic)
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..), WithOrigin (..))
import Control.Monad.Identity (Identity)
import Control.State.Transition (STS (State))
import Data.Foldable (toList)
import Data.IP (IPv4, IPv6)
import Data.Maybe.Strict (StrictMaybe)
import Data.Primitive.Types (Prim)
import Data.Sequence.Strict (StrictSeq)
import Data.TreeDiff.Class (ToExpr (listToExpr, toExpr), defaultExprViaShow)
import Data.TreeDiff.Expr
import Data.UMap (Trip, UMap)
import Data.VMap (VB, VMap, VP, toMap)

instance
  ( ToExpr (TxOut era),
    ToExpr (State (EraRule "PPUP" era)),
    ToExpr (PParams era),
    ToExpr (StashedAVVMAddresses era)
  ) =>
  ToExpr (NewEpochState era)

instance
  ( ToExpr (TxOut era),
    ToExpr (State (EraRule "PPUP" era)),
    ToExpr (PParams era)
  ) =>
  ToExpr (EpochState era)

instance
  ( ToExpr (TxOut era),
    ToExpr (State (EraRule "PPUP" era))
  ) =>
  ToExpr (LedgerState era)

instance
  ( ToExpr (TxOut era),
    ToExpr (State (EraRule "PPUP" era))
  ) =>
  ToExpr (UTxOState era)

instance
  ( ToExpr (TxOut era)
  ) =>
  ToExpr (UTxO era)

instance ToExpr (DPState c)

instance ToExpr (PState c)

instance ToExpr (DState c)

instance ToExpr (RewardAcnt era)

instance ToExpr AccountState

instance ToExpr (PoolDistr c)

instance ToExpr (IndividualPoolStake c)

instance ToExpr (Addr c)

instance ToExpr (StakeReference c)

instance ToExpr (BootstrapAddress c) where
  toExpr = defaultExprViaShow

instance ToExpr (PulsingRewUpdate c) where
  toExpr _ = App "PulsingRewUpdate..." []

instance ToExpr a => ToExpr (StrictSeq a) where
  toExpr x = App "StrictSeqFromList" [listToExpr (toList x)]

instance ToExpr a => ToExpr (StrictMaybe a)

instance (ToExpr a, ToExpr b) => ToExpr (VMap VB VB a b) where
  toExpr x = toExpr (toMap x)

instance (ToExpr a, ToExpr b, Prim b) => ToExpr (VMap VB VP a b) where
  toExpr x = toExpr (toMap x)

instance (EraTxOut era, ToExpr (Value era)) => ToExpr (ShelleyTxOut era) where
  toExpr (ShelleyTxOut x y) = App "ShelleyTxOut" [toExpr x, toExpr y]

instance ToExpr (SnapShots c)

instance ToExpr (SnapShot c)

instance ToExpr (Stake c)

instance ToExpr (FutureGenDeleg c)

instance ToExpr (GenDelegPair c)

instance ToExpr Likelihood

instance ToExpr LogWeight

instance ToExpr (GenDelegs c)

instance ToExpr (InstantaneousRewards c)

instance ToExpr (ScriptHash c)

instance ToExpr (TxIn c)

instance ToExpr (TxId c)

instance ToExpr (NonMyopic c)

instance ToExpr (BlocksMade c)

instance ToExpr (SafeHash c index) where
  toExpr x = App "SafeHash" [toExpr (extractHash x)]

instance ToExpr (Hash.Hash c index) where
  toExpr x = defaultExprViaShow x

instance ToExpr (IncrementalStake c)

instance ToExpr (Credential keyrole c)

instance ToExpr Coin

instance ToExpr DeltaCoin

instance ToExpr (CompactForm Coin) where
  toExpr x = App "Coin" [toExpr (fromCompact x)]

instance ToExpr Ptr

instance ToExpr (KeyHash keyrole c) where
  toExpr (KeyHash x) = App "KeyHash" [toExpr x]

instance ToExpr (PoolParams era)

instance ToExpr StakePoolRelay

instance ToExpr PoolMetadata

instance ToExpr IPv4

instance ToExpr IPv6

instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d) => ToExpr (UMap a b c d)

instance (ToExpr a, ToExpr b, ToExpr c) => ToExpr (Trip a b c)

instance ToExpr (PParamsUpdate era) => ToExpr (PPUPState era)

instance ToExpr (PParamsUpdate era) => ToExpr (ProposedPPUpdates era)

instance ToExpr (ShelleyPParamsHKD StrictMaybe era)

instance ToExpr (ShelleyPParamsHKD Identity era)

instance ToExpr ProtVer

instance ToExpr Version where
  toExpr x = App "Version" [toExpr (getVersion64 x)]

instance ToExpr SlotNo

instance ToExpr BlockNo

instance ToExpr EpochNo

instance ToExpr EpochSize

instance ToExpr x => ToExpr (WithOrigin x)
