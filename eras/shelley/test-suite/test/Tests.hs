{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules (ShelleyLEDGER)
import System.IO (hSetEncoding, stdout, utf8)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C)
import Test.Cardano.Ledger.Shelley.Pretty (prettyTest)
import Test.Cardano.Ledger.Shelley.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Cardano.Ledger.Shelley.Rewards (rewardTests)
import Test.Cardano.Ledger.Shelley.RulesTests (chainExamples, multisigExamples)
import Test.Cardano.Ledger.Shelley.SafeHash (safeHashTest)
import qualified Test.Cardano.Ledger.Shelley.Serialisation as Serialisation
import Test.Cardano.Ledger.Shelley.UnitTests (unitTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Ledger with Delegation"
    [ minimalPropertyTests @C @(ShelleyLEDGER C),
      rewardTests,
      Serialisation.tests 5,
      chainExamples,
      multisigExamples,
      unitTests,
      prettyTest,
      safeHashTest
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Ledger with Delegation nightly"
    [ propertyTests @C @(ShelleyLEDGER C),
      Serialisation.tests 50
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Ledger with Delegation fast"
    [ Serialisation.tests 1,
      chainExamples,
      multisigExamples,
      unitTests,
      prettyTest,
      safeHashTest
    ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  sodiumInit
  mainWithTestScenario tests
