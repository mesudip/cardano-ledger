{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Ledger.Alonzo.Language (Language (..), IsLanguage (..))
import Cardano.Ledger.Alonzo.TxInfo (PlutusDebug (..), PlutusDebugInfo (..), debugPlutus)
import Cardano.Ledger.BaseTypes (natVersion)
import System.Environment (getArgs)
import System.IO
import Data.Typeable (Typeable)

main :: IO ()
main = 
  print . 
  (debugPlutus (natVersion @7) :: (forall (l :: Language). (Typeable l, IsLanguage l) => String -> PlutusDebugInfo l)) . 
  head =<< getArgs
