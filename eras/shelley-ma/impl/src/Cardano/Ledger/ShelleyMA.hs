{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA
  ( ShelleyMAEra,
    MaryOrAllegra (..),
    ShelleyTx,
    ShelleyTxOut,
    MATxBody,
    AllegraTxAuxData,

    -- * Deprecated
    Tx,
    ShelleyTx.TxOut,
    TxBody,
    -- PParams,
    AuxiliaryData,
    ShelleyPParamsHKD,
  )
where

import Cardano.Ledger.Core as Core (EraSegWits (..), ProtVerAtMost, EraPParams, EraTxOut, TxOut)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.Core (ShelleyPParamsHKD)
import Cardano.Ledger.Shelley.Tx as ShelleyTx
  ( ShelleyTx,
    ShelleyTxOut,
    Tx,
    TxOut,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AllegraTxAuxData, AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Era (MAClass, MaryOrAllegra (..), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Tx ()
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody, TxBody)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- Uses the default instance of hashScript

instance
  ( MAClass ma c,
    ProtVerAtMost (ShelleyMAEra ma c) 4,
    ProtVerAtMost (ShelleyMAEra ma c) 6,
    EraPParams (ShelleyMAEra ma c),
    EraTxOut (ShelleyMAEra ma c),
    Core.TxOut (ShelleyMAEra ma c) ~ ShelleyTxOut (ShelleyMAEra ma c)
  ) =>
  EraSegWits (ShelleyMAEra ma c)
  where
  type TxSeq (ShelleyMAEra ma c) = ShelleyTxSeq (ShelleyMAEra ma c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = Shelley.bbHash
  numSegComponents = 3
