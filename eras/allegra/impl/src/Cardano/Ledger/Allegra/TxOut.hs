{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.TxOut (scaledMinDeposit) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.PParams (_minUTxOValue)
import Cardano.Ledger.Shelley.TxBody
  ( ShelleyTxOut (..),
    addrEitherShelleyTxOutL,
    valueEitherShelleyTxOutL,
  )

instance Crypto c => EraTxOut (AllegraEra c) where
  {-# SPECIALIZE instance EraTxOut (AllegraEra StandardCrypto) #-}

  type TxOut (AllegraEra c) = ShelleyTxOut (AllegraEra c)

  mkBasicTxOut = ShelleyTxOut

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp _txOut = _minUTxOValue pp
