{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Allegra.Core
  ( AllegraEraTxBody (..),
    module Cardano.Ledger.Shelley.Core,
  )
where

import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Allegra.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (DecodeMint, EncodeMint)
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)

class ShelleyEraTxBody era => AllegraEraTxBody era where
  vldtTxBodyL :: Lens' (TxBody era) ValidityInterval
