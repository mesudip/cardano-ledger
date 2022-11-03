{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tools for debugging code.
module Cardano.Ledger.Shelley.Debug where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), EraCrypto, bodyTxL, feeTxBodyL, inputsTxBodyL)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    InstantaneousRewards (..),
    PState (..),
    UTxOState (..),
    deltaTxDeposit,
    keyTxRefunds,
    obligationDPState,
    produced,
    totalTxDeposits,
  )
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Slot (EpochNo, SlotNo)
import Cardano.Ledger.UTxO (DepositInfo, EraUTxO, UTxO (..), balance, coinBalance, getConsumedValue, txouts)
import Cardano.Ledger.Val ((<+>))
import Data.Foldable (fold, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Lens.Micro

-- ===============================================
-- Printing Certificates

trim :: Int -> String -> String
trim n s = take n s

showCred :: Credential x c -> String
showCred (ScriptHashObj (ScriptHash x)) = show x
showCred (KeyHashObj (KeyHash x)) = show x

synopsisCert :: DCert c -> String
synopsisCert x = case x of
  DCertDeleg (RegKey cred) -> "RegKey " ++ trim 10 (showCred cred)
  DCertDeleg (DeRegKey cred) -> "DeRegKey " ++ trim 10 (showCred cred)
  DCertDeleg (Delegate _) -> "Delegation"
  DCertPool (RegPool pool) -> let KeyHash hash = ppId pool in "RegPool " ++ trim 10 (show hash)
  DCertPool (RetirePool khash e) -> "RetirePool " ++ showKeyHash khash ++ " " ++ show e
  DCertGenesis _ -> "GenesisCert"
  DCertMir _ -> "MirCert"

showKeyHash :: KeyHash c x -> String
showKeyHash (KeyHash hash) = trim 10 (show hash)

showCerts :: [DCert c] -> String
showCerts certs = unlines (map (("  " ++) . synopsisCert) certs)

showTxCerts :: ShelleyEraTxBody era => Core.TxBody era -> String
showTxCerts txb = showCerts (toList (txb ^. certsTxBodyL))

-- ===============================================
-- Printing Produced == Consumed

produceEqualsConsumed ::
  ( ShelleyEraTxBody era,
    HasField "_poolDeposit" pp Coin,
    HasField "_keyDeposit" pp Coin,
    DepositInfo era ~ DPState (EraCrypto era),
    EraUTxO era
  ) =>
  pp ->
  DPState (EraCrypto era) ->
  UTxO era ->
  Core.TxBody era ->
  String
produceEqualsConsumed pp dpstate utxo@(UTxO u) txb =
  let consumedValue = getConsumedValue pp dpstate utxo txb
      producedValue = produced pp dpstate txb
   in ( "\n"
          ++ showTxCerts txb
          ++ "\nConsumed (inputs + refunds + withdrawals) "
          ++ show consumedValue
          ++ "\n  inputs   "
          ++ show (coinBalance (UTxO (Map.restrictKeys u (txb ^. inputsTxBodyL))))
          ++ "\n  refunds  "
          ++ show (keyTxRefunds pp dpstate txb)
          ++ "\n  withdraw "
          ++ show (fold . unWdrl $ txb ^. wdrlsTxBodyL)
          ++ "\nProduced (outputs + fees + deposits) "
          ++ show producedValue
          ++ "\n  outputs  "
          ++ show (balance (txouts txb))
          ++ "\n  fees     "
          ++ show (txb ^. feeTxBodyL)
          ++ "\n  deposits "
          ++ show (totalTxDeposits pp dpstate txb)
      )

-- ========================

{-trace ("balold "++show (utxoBalance us)++" + withdrawal "++show(withdrawals (tx ^. bodyTxL)) ++
                      " =?= balnew "++ show(utxoBalance us') ++
                      "\n Deposits in pre , post state "++ show ((utxosDeposited us,utxosDeposited us')))
-}

showLEDGERrule ::
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    ShelleyEraTxBody era,
    Core.EraTx era
  ) =>
  SlotNo ->
  pp ->
  UTxOState era ->
  DPState (EraCrypto era) ->
  Core.Tx era ->
  String
showLEDGERrule slot pp utxoSt dpstate@(DPState dst pst) tx =
  let ob = (obligationDPState dpstate)
      dep = (utxosDeposited utxoSt)
   in ( "SLOT = "
          ++ show (slot)
          ++ ", keydeposit = "
          ++ show ((getField @"_keyDeposit" pp) :: Coin)
          ++ ", poolDeposit = "
          ++ show ((getField @"_poolDeposit" pp) :: Coin)
          ++ "\n"
          ++ showTxCerts (tx ^. bodyTxL)
          ++ ( if ob == dep
                 then "  *obligationDPState = "
                 else "  ###obligationDPState = "
             )
          ++ "(key "
          ++ show (fold (dsDeposits dst))
          ++ ", pool "
          ++ show (fold (psDeposits pst))
          ++ ")"
          ++ show ob
          ++ ", utxosDeposited = "
          ++ show dep
          ++ ", delta = "
          ++ show (deltaTxDeposit pp dpstate (tx ^. bodyTxL))
      )

showPoolReap ::
  Map.Map (KeyHash 'StakePool c) Coin ->
  Set.Set (KeyHash 'StakePool c) ->
  Coin ->
  Coin ->
  Map.Map (KeyHash 'StakePool c) Coin ->
  EpochNo ->
  PState c ->
  String
showPoolReap _retiringDeposits retired unclaimed refunded remainingDeposits e _ps =
  ( "\nREAPING "
      ++ show e
      ++ "\nretiringdeposits "
      ++ showMap showKeyHash show (_retiringDeposits)
      ++ "\nretiring "
      ++ showListy showKeyHash retired
      ++ "\n refunds "
      ++ show (unclaimed <+> refunded)
      ++ " unclaimed "
      ++ show (unclaimed)
      ++ " amount retiring "
      ++ show (fold _retiringDeposits)
      ++ " amount remaining "
      ++ show (fold remainingDeposits)
  )

showMap :: (t1 -> [Char]) -> (t2 -> [Char]) -> Map.Map t1 t2 -> String
showMap showKey showVal m = unlines (map showpair (Map.toList m))
  where
    showpair (key, val) = showKey key ++ " -> " ++ showVal val

showListy :: Foldable t => (a -> String) -> t a -> String
showListy showElem list = unlines (map showElem (toList list))

showRewardAcct :: RewardAcnt c -> [Char]
showRewardAcct (RewardAcnt {getRwdNetwork = network, getRwdCred = cred}) =
  show network ++ " " ++ (showCred cred)

showWithdrawal :: Wdrl c -> String
showWithdrawal (Wdrl m) = showMap (("   " ++) . showRewardAcct) show m

showIR :: InstantaneousRewards c -> String
showIR (InstantaneousRewards m n x y) =
  unlines
    [ "IRReseves " ++ showMap (("   " ++) . trim 10 . showCred) show m,
      "IRTreasury " ++ showMap (("   " ++) . trim 10 . showCred) show n,
      "DeltaReserves " ++ show x,
      "DeltaTreasury " ++ show y
    ]

showSafeHash :: SafeHash c i -> String
showSafeHash x = trim 12 (show (extractHash x))
