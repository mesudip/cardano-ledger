{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Tools
  ( evaluateTransactionExecutionUnits,
    evaluateTransactionExecutionUnitsWithLogs,
    TransactionScriptFailure (..),
  )
where

import Cardano.Ledger.Alonzo.Data (Data, Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (knownToNotBe1Phase)
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    CostModel,
    ExUnits (..),
    getEvaluationContext,
  )
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, ScriptPurpose (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (txscripts),
    PlutusDebug (..),
    TranslationError,
    VersionedTxInfo (..),
    exBudgetToExUnits,
    transExUnits,
    transProtocolVersion,
    txInfo,
    valContext,
  )
import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoEraTxWits (..),
    RdmrPtr (..),
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Array (Array, bounds, (!))
import Data.ByteString.Short as SBS (ShortByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Records (HasField (..))
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2

-- | Script failures that can be returned by 'evaluateTransactionExecutionUnits'.
data TransactionScriptFailure c
  = -- | A redeemer was supplied that does not point to a
    --  valid plutus evaluation site in the given transaction.
    RedeemerNotNeeded !RdmrPtr !(ScriptHash c)
  | -- | A redeemer was supplied which points to a script hash which
    -- we cannot connect to a Plutus script.
    RedeemerPointsToUnknownScriptHash !RdmrPtr
  | -- | Missing redeemer. The first parameter is the redeemer pointer which cannot be resolved,
    -- and the second parameter is the map of pointers which can be resolved.
    MissingScript !RdmrPtr !(Map RdmrPtr (ScriptPurpose c, Maybe (ShortByteString, Language), ScriptHash c))
  | -- | Missing datum.
    MissingDatum !(DataHash c)
  | -- | Plutus V1 evaluation error.
    ValidationFailedV1 !PV1.EvaluationError ![Text] PlutusDebug
  | -- | Plutus V2 evaluation error.
    ValidationFailedV2 !PV2.EvaluationError ![Text] PlutusDebug
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn !(TxIn c)
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn !(TxIn c)
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget !PV1.ExBudget
  | -- | There was no cost model for a given version of Plutus in the ledger state
    NoCostModelInLedgerState !Language
  deriving (Show, Eq)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

type RedeemerReport c = Map RdmrPtr (Either (TransactionScriptFailure c) ExUnits)

type RedeemerReportWithLogs c = Map RdmrPtr (Either (TransactionScriptFailure c) ([Text], ExUnits))

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnits' are intended to replace them.
evaluateTransactionExecutionUnits ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    Script era ~ AlonzoScript era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReport (EraCrypto era))
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels =
  fmap (Map.map (fmap snd)) $ evaluateTransactionExecutionUnitsWithLogs pp tx utxo ei sysS costModels

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnitsWithLogs' are intended to replace them.
evaluateTransactionExecutionUnitsWithLogs ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    Script era ~ AlonzoScript era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget with logs of the script.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReportWithLogs (EraCrypto era))
evaluateTransactionExecutionUnitsWithLogs pp tx utxo ei sysS costModels = do
  let getInfo :: Language -> Either (TranslationError (EraCrypto era)) VersionedTxInfo
      getInfo lang = txInfo lang ei sysS utxo tx
  ctx <- sequence $ Map.fromSet getInfo languagesUsed
  pure $
    Map.mapWithKey
      (findAndCount pp ctx)
      (unRedeemers $ ws ^. rdmrsTxWitsL)
  where
    txBody = tx ^. bodyTxL
    ws = tx ^. witsTxL
    dats = unTxDats $ ws ^. datsTxWitsL
    scriptsAvailable = txscripts utxo tx
    AlonzoScriptsNeeded needed = getScriptsNeeded utxo txBody
    neededAndConfirmedToBePlutus = mapMaybe (knownToNotBe1Phase scriptsAvailable) needed
    languagesUsed = Set.fromList [lang | (_, lang, _) <- neededAndConfirmedToBePlutus]

    ptrToPlutusScript = Map.fromList $ do
      (sp, sh) <- needed
      msb <- case Map.lookup sh scriptsAvailable of
        Nothing -> pure Nothing
        Just (TimelockScript _) -> []
        Just (PlutusScript lang bytes) -> pure $ Just (bytes, lang)
      pointer <- case rdptr txBody sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb, sh))

    findAndCount ::
      PParams era ->
      Map Language VersionedTxInfo ->
      RdmrPtr ->
      (Data era, ExUnits) ->
      Either (TransactionScriptFailure (EraCrypto era)) ([Text], ExUnits)
    findAndCount pparams info pointer (rdmr, exunits) = do
      (sp, mscript, sh) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer ptrToPlutusScript
      (script, lang) <- note (MissingScript pointer ptrToPlutusScript) mscript
      inf <- note (RedeemerNotNeeded pointer sh) $ Map.lookup lang info
      let (l1, l2) = bounds costModels
      cm <-
        if l1 <= lang && lang <= l2
          then Right (costModels ! lang)
          else Left (NoCostModelInLedgerState lang)
      args <- case sp of
        Spending txin -> do
          txOut <- note (UnknownTxIn txin) $ Map.lookup txin (unUTxO utxo)
          datum <- case txOut ^. datumTxOutF of
            Datum binaryData -> pure $ binaryDataToData binaryData
            DatumHash dh -> note (MissingDatum dh) $ Map.lookup dh dats
            NoDatum -> Left (InvalidTxIn txin)
          pure [datum, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getPlutusData args

      case interpreter lang (getEvaluationContext cm) maxBudget script pArgs of
        (logs, Left e) -> case lang of
          PlutusV1 ->
            let debug = PlutusDebugV1 cm exunits script pArgs protVer
             in Left $ ValidationFailedV1 e logs debug
          PlutusV2 ->
            let debug = PlutusDebugV2 cm exunits script pArgs protVer
             in Left $ ValidationFailedV2 e logs debug
        (logs, Right exBudget) -> note (IncompatibleBudget exBudget) $ (,) logs <$> exBudgetToExUnits exBudget
      where
        maxBudget = transExUnits . getField @"_maxTxExUnits" $ pparams
        protVer = getField @"_protocolVersion" pparams
        pv = transProtocolVersion protVer
        interpreter lang = case lang of
          PlutusV1 -> PV1.evaluateScriptRestricting pv PV1.Verbose
          PlutusV2 -> PV2.evaluateScriptRestricting pv PV2.Verbose
