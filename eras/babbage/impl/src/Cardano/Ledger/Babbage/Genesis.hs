{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Ledger.Babbage.Genesis
  ( AlonzoGenesis (..),
    extendPPWithGenesis,
    augmentPPWithGenesis,
  )
where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Babbage.PParams.Class (BabbageEraPParams (..), ppCoinsPerUTxOByteL)
import Cardano.Ledger.Babbage.PParams (BabbagePParams, extendPP)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Cardano.Ledger.Core
import Data.Functor.Identity (Identity)
import Lens.Micro

augmentPPWithGenesis ::
  (BabbageEraPParams era1, BabbageEraPParams era2) =>
  PParams era1 ->
  AlonzoGenesis ->
  PParams era2
augmentPPWithGenesis bpp
  -- BabbagePParams
  --   { _minfeeA,
  --     _minfeeB,
  --     _maxBBSize,
  --     _maxTxSize,
  --     _maxBHSize,
  --     _keyDeposit,
  --     _poolDeposit,
  --     _eMax,
  --     _nOpt,
  --     _a0,
  --     _rho,
  --     _tau,
  --     _protocolVersion,
  --     _minPoolCost
  --   }
  AlonzoGenesis
    { coinsPerUTxOWord,
      costmdls, -- = _costmdls,
      prices, -- = _prices,
      maxTxExUnits,--  = _maxTxExUnits,
      maxBlockExUnits, -- = _maxBlockExUnits,
      maxValSize, -- = _maxValSize,
      collateralPercentage, -- = _collateralPercentage,
      maxCollateralInputs -- = _maxCollateralInputs
    } =
    emptyPParams
     & ppMinFeeAL .~ (bpp ^. ppMinFeeAL)
     & ppMinFeeBL .~ (bpp ^. ppMinFeeBL)
     & ppMaxBBSizeL .~ (bpp ^. ppMaxBBSizeL)
      -- _maxTxSize,
      -- _maxBHSize,
      -- _keyDeposit,
      -- _poolDeposit,
      -- _eMax,
      -- _nOpt,
      -- _a0,
      -- _rho,
      -- _tau,
      -- _protocolVersion,
      -- _minPoolCost
     & ppCoinsPerUTxOByteL .~ coinsPerUTxOWord

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  ShelleyPParams era1 ->
  AlonzoGenesis ->
  BabbagePParams era2
extendPPWithGenesis
  pp
  AlonzoGenesis
    { coinsPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
    } =
    extendPP
      pp
      coinsPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs
