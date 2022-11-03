{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.AdaPots
  ( AdaPots (..),
    totalAdaES,
    totalAdaPotsES,
    compareAdaPots,
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    LedgerState (..),
    PState (..),
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.UTxO (coinBalance)
import Cardano.Ledger.Val ((<->))
import Data.Foldable (fold)

data AdaPots = AdaPots
  { treasuryAdaPot :: Coin,
    reservesAdaPot :: Coin,
    rewardsAdaPot :: Coin,
    utxoAdaPot :: Coin,
    keyDepositAdaPot :: Coin,
    poolDepositAdaPot :: Coin,
    depositsAdaPot :: Coin,
    feesAdaPot :: Coin
  }
  deriving (Show, Eq)

-- | Calculate the total ada pots in the epoch state
totalAdaPotsES ::
  EraTxOut era =>
  EpochState era ->
  AdaPots
totalAdaPotsES (EpochState (AccountState treasury_ reserves_) _ ls _ _ _) =
  AdaPots
    { treasuryAdaPot = treasury_,
      reservesAdaPot = reserves_,
      rewardsAdaPot = rewards_,
      utxoAdaPot = coins,
      keyDepositAdaPot = keyDeposits_,
      poolDepositAdaPot = poolDeposits_,
      depositsAdaPot = deposits,
      feesAdaPot = fees_
    }
  where
    UTxOState u deposits fees_ _ _ = lsUTxOState ls
    DPState dstate _ = lsDPState ls
    rewards_ = fold (rewards dstate)
    coins = coinBalance u
    keyDeposits_ = fold ((dsDeposits . dpsDState . lsDPState) ls)
    poolDeposits_ = fold ((psDeposits . dpsPState . lsDPState) ls)

-- | Calculate the total ada in the epoch state
totalAdaES :: EraTxOut era => EpochState era -> Coin
totalAdaES cs =
  treasuryAdaPot
    <> reservesAdaPot
    <> rewardsAdaPot
    <> utxoAdaPot
    <> depositsAdaPot
    <> feesAdaPot
  where
    AdaPots
      { treasuryAdaPot,
        reservesAdaPot,
        rewardsAdaPot,
        utxoAdaPot,
        -- keyDepositAdaPot,
        -- poolDepositAdaPot,
        depositsAdaPot,
        feesAdaPot
      } = totalAdaPotsES cs

pad :: Int -> String -> String
pad n x = if m < n then x ++ [const ' ' i | i <- [1 .. n - m]] else x
  where
    m = length x

compareAdaPots :: String -> AdaPots -> String -> AdaPots -> String
compareAdaPots xlabel x ylabel y =
  unlines
    [ pad n "field" ++ pad n xlabel ++ pad n ylabel ++ pad n "difference",
      oneline "treasuryAdaPot" treasuryAdaPot,
      oneline "reservesAdaPot" reservesAdaPot,
      oneline "rewardsAdaPot" rewardsAdaPot,
      oneline "utxoAdaPot" utxoAdaPot,
      oneline "keyDepositAdaPot" keyDepositAdaPot,
      oneline "poolDepositAdaPot" poolDepositAdaPot,
      oneline "depositsAdaPot" depositsAdaPot,
      oneline "feesAdaPot" feesAdaPot
    ]
  where
    n = 25
    oneline name f = pad n name ++ pad n (show (f x)) ++ pad n (show (f y)) ++ pad n (show (f y <-> f x))
