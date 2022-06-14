{-# LANGUAGE DeriveAnyClass #-}

module PlutusLedgerApi.Test.EvaluationEvent (
    ScriptEvaluationEvent (..),
    ScriptEvaluationData (..),
    ScriptEvaluationResult (..),
) where

import Plutus.ApiCommon
import PlutusCore.Data qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

import Codec.Serialise (Serialise (..))
import GHC.Generics (Generic)

data ScriptEvaluationResult = ScriptEvaluationSuccess | ScriptEvaluationFailure
    deriving stock (Generic)
    deriving anyclass (Serialise)

data ScriptEvaluationData = ScriptEvaluationData
    { dataProtocolVersion :: ProtocolVersion
    , dataCostParams      :: [Integer]
    , dataBudget          :: ExBudget
    , dataScript          :: SerializedScript
    , dataInputs          :: [PLC.Data]
    }
    deriving stock (Generic)
    deriving anyclass (Serialise)

data ScriptEvaluationEvent
    = PlutusV1Event ScriptEvaluationData ScriptEvaluationResult
    | PlutusV2Event ScriptEvaluationData ScriptEvaluationResult
    deriving stock (Generic)
    deriving anyclass (Serialise)
