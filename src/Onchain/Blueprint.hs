module Onchain.Blueprint where

import Data.Set qualified as Set
import Onchain.Types
import Onchain.WineValidator (wineScriptCompiled)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusTx.Blueprint

import Data.ByteString.Short (fromShort)

myContractBlueprint :: WineMintingParams -> ContractBlueprint
myContractBlueprint mp =
    MkContractBlueprint
        { contractId = Just "Wine Validator"
        , contractPreamble = myPreamble -- defined below
        , contractValidators = Set.singleton (myValidator mp) -- defined below
        , contractDefinitions = deriveDefinitions @[WineMintingParams, WineTokenCIP68Datum, WineMintingRedeemer]
        }

myPreamble :: Preamble
myPreamble =
    MkPreamble
        { preambleTitle = "My Contract"
        , preambleDescription = Just "A simple contract"
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV3
        , preambleLicense = Just "MIT"
        }

myValidator :: WineMintingParams -> ValidatorBlueprint referencedTypes
myValidator mp =
    MkValidatorBlueprint
        { validatorTitle = "My Validator"
        , validatorDescription = Just "An example validator"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "My Validator Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @WineMintingParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "My Redeemer"
                , argumentDescription = Just "A redeemer that does something awesome"
                , argumentPurpose = Set.fromList [Spend, Mint]
                , argumentSchema = definitionRef @WineMintingRedeemer
                }
        , validatorDatum =
            Just
                MkArgumentBlueprint
                    { argumentTitle = Just "My Datum"
                    , argumentDescription = Just "A datum that contains something awesome"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @WineTokenCIP68Datum
                    }
        , validatorCompiled =
            Just $
                compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode $ wineScriptCompiled mp) -- you can optionally provide the compiled code here
        }