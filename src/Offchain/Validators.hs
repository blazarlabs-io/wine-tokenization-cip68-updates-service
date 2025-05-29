module Offchain.Validators where

import PlutusLedgerApi.Data.V1 (PubKeyHash)

import GeniusYield.Types (
    GYPaymentSigningKey,
    GYScript,
    PlutusVersion (PlutusV3),
    getVerificationKey,
    pubKeyHash,
    pubKeyHashToPlutus,
    validatorFromPlutus,
 )
import Onchain.Blueprint (myContractBlueprint)
import Onchain.Types
import Onchain.WineValidator
import Parameters (blueprintFile)
import PlutusTx.Blueprint (writeBlueprint)

mkWineScriptGYFromAdminSkey :: GYPaymentSigningKey -> GYScript 'PlutusV3
mkWineScriptGYFromAdminSkey admin =
    let adminPKH = pubKeyHashToPlutus $ pubKeyHash $ getVerificationKey admin
     in mkWineScriptGY adminPKH

mkWineScriptGY :: PubKeyHash -> GYScript 'PlutusV3
mkWineScriptGY adminPKH = validatorFromPlutus $ wineScriptCompiled (WineMintingParams adminPKH)

writeWineContractBlueprint :: PubKeyHash -> IO ()
writeWineContractBlueprint adminPkh = writeBlueprint blueprintFile (myContractBlueprint (WineMintingParams adminPkh))