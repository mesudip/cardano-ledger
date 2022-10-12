{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Orphans () where

import Cardano.Binary (ToCBOR (..))
import Codec.CBOR.Term (Term, encodeTerm)

instance ToCBOR Term where
  toCBOR = encodeTerm
