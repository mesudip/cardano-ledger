{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Core.Era
  ( Era (..),
  )
where

import qualified Cardano.Ledger.Crypto as CC
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

class (CC.Crypto (EraCrypto era), Typeable era, ProtVerLow era <= ProtVerHigh era) => Era era where
  type EraCrypto era :: Type

  -- | Lowest major protocol version for this era
  type ProtVerLow era :: Nat

  -- | Highest major protocol version for this era. By default se to `ProtVerLow`
  type ProtVerHigh era :: Nat

  type ProtVerHigh era = ProtVerLow era
