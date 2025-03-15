module TestRunners where

import GeniusYield.TxBuilder
import GeniusYield.Types (
    GYAssetClass,
    GYTxOutRef,
    PlutusVersion (PlutusV3),
    assetClassToPlutus,
    pubKeyHashToPlutus,
 )

import GeniusYield.Test.Clb (sendSkeleton')
import GeniusYield.Test.Utils (addRefScript)
import GeniusYield.Types.Script
import Offchain.Interactions
import Offchain.OffchainUtils
import Offchain.Operations
import Onchain.Types
import Onchain.WineValidator

deployReferenceScriptRun :: (GYTxGameMonad m) => User -> User -> m GYTxOutRef
deployReferenceScriptRun to from = asUser to $ do
    let adminPkh = pubKeyHashToPlutus $ userPkh to
    logInfoGreen $ "Deploying reference script, with admin:" <> show adminPkh
    let holderAddr = userAddr from
    let validator = validatorFromPlutus @'PlutusV3 $ wineScriptCompiled (WineMintingParams adminPkh)
    refScript <- addRefScript holderAddr (validatorToScript validator)
    logInfoGreen $ "Reference script depolyed at:" <> show refScript
    return refScript

mintBatchRun ::
    (GYTxGameMonad m) =>
    User ->
    GYTxOutRef ->
    TokenMeta ->
    TokenData ->
    Integer ->
    m GYAssetClass
mintBatchRun user refScript tmeta tdata quantity = do
    asUser user do
        logInfoGreen $ "Minting new batch token : " <> show (userAddr user)
        (skeleton, gyRefAC) <- newBatchSk refScript (userAddr user) (mkWineBatch tmeta tdata (quantity, quantity))
        void $ sendSkeleton' skeleton
        return gyRefAC

updateBatchRun ::
    (GYTxGameMonad m) =>
    User ->
    GYTxOutRef ->
    GYAssetClass ->
    TokenMeta ->
    TokenData ->
    Integer ->
    m ()
updateBatchRun user refScript batchId tmeta tdata quantity = do
    asUser user do
        logInfoGreen $ "Update batch token : " <> show batchId
        skeleton <- updateBatchSk refScript (userAddr user) batchId (mkWineBatch tmeta tdata (quantity, quantity))
        void $ sendSkeleton' skeleton

mintBottleRun ::
    (GYTxGameMonad m) =>
    User ->
    GYTxOutRef ->
    TokenMeta ->
    TokenData ->
    GYAssetClass ->
    m GYAssetClass
mintBottleRun user refScript tmeta tdata batchId = do
    asUser user do
        logInfoGreen $ "Mint bottle token for batch : " <> show batchId
        (skeleton, gyRefAC) <- newBottleSk refScript (userAddr user) (mkWineBottle tmeta tdata (assetClassToPlutus batchId))
        void $ sendSkeleton' skeleton
        return gyRefAC

updateBottleRun ::
    (GYTxGameMonad m) =>
    User ->
    GYTxOutRef ->
    GYAssetClass ->
    TokenMeta ->
    TokenData ->
    GYAssetClass ->
    m ()
updateBottleRun user refScript bottleId tmeta tdata batchId = do
    asUser user do
        logInfoGreen $ "Update bottle token with id : " <> show bottleId
        skeleton <- updateBottleSk refScript (userAddr user) bottleId (mkWineBottle tmeta tdata (assetClassToPlutus batchId))
        void $ sendSkeleton' skeleton
