{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Delegs
  ( DELEGS,
    DelegsEnv (..),
    DelegsPredicateFailure (..),
    PredicateFailure,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Control.Iterate.SetAlgebra (dom, eval, (∈), (⨃))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    (?!),
    (?!:),
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Address (mkRwdAcnt)
import Shelley.Spec.Ledger.BaseTypes
  ( ShelleyBase,
    invalidKey,
    networkId,
  )
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    DPState (..),
    RewardAccounts,
    emptyDelegation,
    _dstate,
    _pParams,
    _rewards,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Delpl (DELPL, DelplEnv (..))
import Shelley.Spec.Ledger.Serialization
  ( decodeRecordSum,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    Ix,
    Ptr (..),
    RewardAcnt (..),
    Wdrl (..),
  )

data DELEGS era

data DelegsEnv era = DelegsEnv
  { delegsSlotNo :: SlotNo,
    delegsIx :: Ix,
    delegspp :: PParams era,
    delegsTx :: Tx era,
    delegsAccount :: AccountState
  }

deriving stock instance
  ShelleyBased era =>
  Show (DelegsEnv era)

data DelegsPredicateFailure era
  = DelegateeNotRegisteredDELEG
      !(KeyHash 'StakePool (Crypto era)) -- target pool which is not registered
  | WithdrawalsNotInRewardsDELEGS
      !(Map (RewardAcnt era) Coin) -- withdrawals that are missing or do not withdrawl the entire amount
  | DelplFailure (PredicateFailure (DELPL era)) -- Subtransition Failures
  deriving (Show, Eq, Generic)

instance CryptoClass.Crypto c => STS (DELEGS (ShelleyEra c)) where
  type State (DELEGS (ShelleyEra c)) = DPState (ShelleyEra c)
  type Signal (DELEGS (ShelleyEra c)) = Seq (DCert (ShelleyEra c))
  type Environment (DELEGS (ShelleyEra c)) = DelegsEnv (ShelleyEra c)
  type BaseM (DELEGS (ShelleyEra c)) = ShelleyBase
  type
    PredicateFailure (DELEGS (ShelleyEra c)) =
      DelegsPredicateFailure (ShelleyEra c)

  initialRules = [pure emptyDelegation]
  transitionRules = [delegsTransition]

instance NoThunks (DelegsPredicateFailure (ShelleyEra c))

instance
  (Typeable era, Era era) =>
  ToCBOR (DelegsPredicateFailure era)
  where
  toCBOR = \case
    DelegateeNotRegisteredDELEG kh ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR kh
    WithdrawalsNotInRewardsDELEGS ws ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> mapToCBOR ws
    (DelplFailure a) ->
      encodeListLen 2 <> toCBOR (2 :: Word8)
        <> toCBOR a

instance
  (Era era) =>
  FromCBOR (DelegsPredicateFailure era)
  where
  fromCBOR =
    decodeRecordSum "PredicateFailure" $
      ( \case
          0 -> do
            kh <- fromCBOR
            pure (2, DelegateeNotRegisteredDELEG kh)
          1 -> do
            ws <- mapFromCBOR
            pure (2, WithdrawalsNotInRewardsDELEGS ws)
          2 -> do
            a <- fromCBOR
            pure (2, DelplFailure a)
          k -> invalidKey k
      )

delegsTransition ::
  forall era.
  ( ShelleyBased era,
    HasField "wdrls" (Core.TxBody era) (Wdrl era),
    State (DELEGS era) ~ DPState era,
    Environment (DELEGS era) ~ DelegsEnv era,
    PredicateFailure (DELEGS era)
      ~ DelegsPredicateFailure era,
    Signal (DELEGS era) ~ Seq (DCert era),
    Embed (DELPL era) (DELEGS era)
  ) =>
  TransitionRule (DELEGS era)
delegsTransition = do
  TRC (env@(DelegsEnv slot txIx pp tx acnt), dpstate, certificates) <- judgmentContext
  network <- liftSTS $ asks networkId

  case certificates of
    Empty -> do
      let ds = _dstate dpstate
          wdrls_ = unWdrl $ getField @"wdrls" (_body tx)
          rewards = _rewards ds

      isSubmapOf wdrls_ rewards -- wdrls_ ⊆ rewards
        ?! WithdrawalsNotInRewardsDELEGS
          ( Map.differenceWith
              (\x y -> if x /= y then Just x else Nothing)
              wdrls_
              (Map.mapKeys (mkRwdAcnt network) rewards)
          )

      let wdrls_' :: RewardAccounts era
          wdrls_' =
            Map.foldrWithKey
              ( \(RewardAcnt _ cred) _coin ->
                  Map.insert cred mempty
              )
              Map.empty
              wdrls_
          rewards' = eval (rewards ⨃ wdrls_')
      pure $ dpstate {_dstate = ds {_rewards = rewards'}}
    gamma :|> c -> do
      dpstate' <-
        trans @(DELEGS era) $ TRC (env, dpstate, gamma)

      let isDelegationRegistered = case c of
            DCertDeleg (Delegate deleg) ->
              let stPools_ = _pParams $ _pstate dpstate'
                  targetPool = _delegatee deleg
               in case eval (targetPool ∈ dom stPools_) of
                    True -> Right ()
                    False -> Left $ DelegateeNotRegisteredDELEG targetPool
            _ -> Right ()
      isDelegationRegistered ?!: id

      let ptr = Ptr slot txIx (fromIntegral $ length gamma)
      trans @(DELPL era) $
        TRC (DelplEnv slot ptr pp acnt, dpstate', c)
  where
    -- @wdrls_@ is small and @rewards@ big, better to transform the former
    -- than the latter into the right shape so we can call 'Map.isSubmapOf'.
    isSubmapOf :: Map (RewardAcnt era) Coin -> RewardAccounts era -> Bool
    isSubmapOf wdrls_ rewards = wdrls_' `Map.isSubmapOf` rewards
      where
        wdrls_' =
          Map.fromList
            [ (cred, coin)
              | (RewardAcnt _ cred, coin) <- Map.toList wdrls_
            ]

instance
  CryptoClass.Crypto c =>
  Embed (DELPL (ShelleyEra c)) (DELEGS (ShelleyEra c))
  where
  wrapFailed = DelplFailure
