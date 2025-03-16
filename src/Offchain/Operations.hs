module Offchain.Operations where

import Data.Either.Extra (maybeToEither)
import GHC.Stack
import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import Offchain.OffchainUtils
import Onchain.OnchainUtils
import Onchain.Types
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V3.Tx qualified as V3
import Unsafe.Coerce (unsafeCoerce)

{--  This function returns a Just tuple of the datum and value of a UTxO if the UTxO has inline datum,
otherwise returns Nothing
--}
getInlineDatumAndValue :: GYUTxO -> Either String (GYDatum, GYValue)
getInlineDatumAndValue utxo = case utxoOutDatum utxo of
    GYOutDatumInline datum -> Right (datum, utxoValue utxo)
    _ -> Left "inline datum expected"

lookupUTxOWithStateToken :: (GYTxQueryMonad m) => AssetClass -> GYAddress -> m (Either String GYUTxO)
lookupUTxOWithStateToken refAC addr = do
    gyRefAC <- assetClassFromPlutus' refAC
    utxos <- utxosToList <$> utxosAtAddress addr (Just gyRefAC)
    case utxos of
        [x] -> return (Right x)
        [] -> return $ Left "utxo with state token not found"
        _ -> return $ Left "only one utxo expected"

getWineTokenDatum :: (GYTxQueryMonad m) => AssetClass -> GYAddress -> m (Either String WineTokenCIP68Datum)
getWineTokenDatum refAC addr = do
    utxo <- lookupUTxOWithStateToken refAC addr
    let gyDatum = fst <$> (getInlineDatumAndValue =<< utxo)
    let dt = maybeToEither "invalid adatum" . fromBuiltinData @WineTokenCIP68Datum . datumToPlutus' =<< gyDatum
    return dt

getWineTokenDatumFromValidator :: (GYTxQueryMonad m) => AssetClass -> GYTxOutRef -> m (Either String WineTokenCIP68Datum)
getWineTokenDatumFromValidator refAC refScript = do
    !wineValidatorAddr <- scriptAddress =<< getScriptFromUTxORef refScript
    getWineTokenDatum refAC wineValidatorAddr

getWineTokenFromValidator :: (GYTxQueryMonad m) => AssetClass -> GYTxOutRef -> m (Either String WineToken)
getWineTokenFromValidator refAC refScript = do
    !wineValidatorAddr <- scriptAddress =<< getScriptFromUTxORef refScript
    wtd <- getWineTokenDatum refAC wineValidatorAddr
    return $ fromWineCIP68Datum <$> wtd

getScriptFromUTxORef :: (GYTxQueryMonad m) => GYTxOutRef -> m (GYScript 'PlutusV3)
getScriptFromUTxORef refScript = do
    utxo <- utxoAtTxOutRef' refScript
    case utxoRefScript utxo of
        Nothing -> throwError $ GYApplicationException (CustomException "utxo with refrerence script expected")
        Just anyscr -> case anyscr of
            GYSimpleScript _ -> throwError $ GYApplicationException (CustomException "wrong script type")
            GYPlutusScript gy -> return $ unsafeCoerce gy

txMustUpdateStateToken :: (GYTxUserQueryMonad m, ToData a, ToData b) => GYTxOutRef -> AssetClass -> a -> (GYDatum -> b) -> m (GYTxSkeleton 'PlutusV3)
txMustUpdateStateToken refScript stateTokenId redeemer updateDatumFunction =
    do
        wineValidatorGY <- getScriptFromUTxORef refScript
        let gyRedeemer = redeemerFromPlutusData redeemer
        validatorAddr <- scriptAddress wineValidatorGY
        stateUTxO <- liftEitherToCustomException =<< lookupUTxOWithStateToken stateTokenId validatorAddr
        (gyDatum, v) <- liftEitherToCustomException $ getInlineDatumAndValue stateUTxO
        let updatedWT = updateDatumFunction gyDatum
        isLockingUpdatedDatum <- txMustLockStateWithInlineDatumAndValue validatorAddr updatedWT v
        return $
            mconcat
                [ mustHaveInput
                    GYTxIn
                        { gyTxInTxOutRef = utxoRef stateUTxO
                        , gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript wineValidatorGY) (Just gyDatum) gyRedeemer
                        }
                , isLockingUpdatedDatum
                ]

txIsPayingValueToAddress :: (GYTxUserQueryMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txIsPayingValueToAddress recipient gyValue = do
    return $
        mustHaveOutput
            GYTxOut
                { gyTxOutAddress = recipient
                , gyTxOutDatum = Nothing
                , gyTxOutValue = gyValue
                , gyTxOutRefS = Nothing
                }

txMustLockStateWithInlineDatumAndValue :: (GYTxUserQueryMonad m, ToData a) => GYAddress -> a -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txMustLockStateWithInlineDatumAndValue addr datum gyValue = do
    let gyDatum = datumFromPlutusData datum
    return $
        mustHaveOutput
            GYTxOut
                { gyTxOutAddress = addr
                , gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum)
                , gyTxOutValue = gyValue
                , gyTxOutRefS = Nothing
                }

mintsUserAndRef :: (GYTxUserQueryMonad m) => GYTxOutRef -> WineToken -> m (GYTxSkeleton 'PlutusV3, (GYAssetClass, GYAssetClass))
mintsUserAndRef refScript wineToken = do
    wineMintingPolicyGY <- getScriptFromUTxORef refScript
    seedTxOutRef <- someUTxOWithoutRefScript
    let TxOutRef (TxId txbs) i = txOutRefToPlutus seedTxOutRef
    let seedTxoutRefPlutus = V3.TxOutRef (V3.TxId txbs) i
    let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
    let (tokenRefAC, tokenUserAC) = generateRefAndUserAC (AssetClass (mintingPolicyCurrencySymbol wineMintingPolicyGY, tokenNameFromTxOutRef (V3.TxId txbs) i))
    let gyRedeemer = redeemerFromPlutusData $ MintWineToken seedTxoutRefPlutus wineToken
    gyRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ tokenRefAC)
    gyUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ tokenUserAC)
    gyRefAC <- assetClassFromPlutus' tokenRefAC
    gyUserAC <- assetClassFromPlutus' tokenUserAC
    return --
        ( mconcat
            [ isSpendingSeedUTxO
            , mustMint (GYMintReference refScript wineMintingPolicyGY) gyRedeemer gyRefTN 1
            , mustMint (GYMintReference refScript wineMintingPolicyGY) gyRedeemer gyUserTN 1
            ]
        , (gyRefAC, gyUserAC)
        )

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
    addr <- scriptAddress limboValidatorV2
    addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
    return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())){gyTxOutRefS = Just $ GYPlutusScript sc}

--------
--------
--------
--------
--------
--------
--------

-- |  Mint Batch Token Transaction Skeleton
newBatchSk ::
    (HasCallStack, GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    WineToken ->
    m (GYTxSkeleton 'PlutusV3, GYAssetClass)
newBatchSk refScript adminAddr wineToken = do
    logInfoYellow $ show wineToken
    !wineValidatorAddr <- scriptAddress =<< getScriptFromUTxORef refScript
    (!isMintingBatchRefAndUserTokens, (gyRefAC, gyUserAC)) <- mintsUserAndRef refScript wineToken
    !isLockingBatchRefToken <- txMustLockStateWithInlineDatumAndValue wineValidatorAddr (mkWineCIP68Datum wineToken) (valueSingleton gyRefAC 1)
    !isGettingBatchUserNFT <- txIsPayingValueToAddress adminAddr (valueSingleton gyUserAC 1)
    adminPkh <- addressToPubKeyHash' adminAddr
    return
        ( mconcat
            [ isMintingBatchRefAndUserTokens
            , isLockingBatchRefToken
            , isGettingBatchUserNFT
            , mustBeSignedBy adminPkh
            ]
        , gyRefAC
        )

-- |  Mint Bottle Token Transaction Skeleton
newBottleSk ::
    (HasCallStack, GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    WineToken ->
    m (GYTxSkeleton 'PlutusV3, GYAssetClass)
newBottleSk refScript adminAddr wineToken = do
    batchId <- case wineToken of
        (WineToken _ _ (BottleToken b)) -> return b
        _ -> liftEitherToCustomException $ Left "not a bottle "
    logInfoYellow $ show wineToken
    wineValidatorAddr <- scriptAddress =<< getScriptFromUTxORef refScript
    (isMintingBottleRefAndUserTokens, (gyRefAC, gyUserAC)) <- mintsUserAndRef refScript wineToken
    isLockingBottleRefToken <- txMustLockStateWithInlineDatumAndValue wineValidatorAddr (mkWineCIP68Datum wineToken) (valueSingleton gyRefAC 1)
    isGettingBottleUserNFT <- txIsPayingValueToAddress adminAddr (valueSingleton gyUserAC 1)
    isUpdatingBatchState <- txMustUpdateStateToken refScript batchId MintBottle get1FromBatchDatumUpdate
    adminPkh <- addressToPubKeyHash' adminAddr
    return
        ( mconcat
            [ isMintingBottleRefAndUserTokens
            , isLockingBottleRefToken
            , isGettingBottleUserNFT
            , isUpdatingBatchState
            , mustBeSignedBy adminPkh
            ]
        , gyRefAC
        )
  where
    get1FromBatchDatumUpdate :: GYDatum -> WineTokenCIP68Datum
    get1FromBatchDatumUpdate = mkWineCIP68Datum . get1FromBatch . fromWineCIP68Datum . unsafeFromBuiltinData . datumToPlutus'

-- |  Update Batch Token Transaction Skeleton
updateBatchSk ::
    (HasCallStack, GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    GYAssetClass ->
    WineToken ->
    m (GYTxSkeleton 'PlutusV3)
updateBatchSk refScript adminAddr batchId updatedWT = do
    logInfoYellow $ show updatedWT
    let updatedWTDatum = mkWineCIP68Datum updatedWT
    isUpdatingBatchState <- txMustUpdateStateToken refScript (assetClassToPlutus batchId) (Update updatedWT) (const updatedWTDatum)
    adminPkh <- addressToPubKeyHash' adminAddr
    return $
        mconcat
            [ isUpdatingBatchState
            , mustBeSignedBy adminPkh
            ]

-- |  Update Bottle Token Transaction Skeleton
updateBottleSk ::
    (GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    GYAssetClass ->
    WineToken ->
    m (GYTxSkeleton 'PlutusV3)
updateBottleSk refScript adminAddr bottleId updatedWT = do
    let updatedWTDatum = mkWineCIP68Datum updatedWT
    isUpdatingBatchState <- txMustUpdateStateToken refScript (assetClassToPlutus bottleId) (Update updatedWT) (const updatedWTDatum)
    adminPkh <- addressToPubKeyHash' adminAddr
    return $
        mconcat
            [ isUpdatingBatchState
            , mustBeSignedBy adminPkh
            ]

-- |  Burn User Token Transaction Skeleton
burnUserToken ::
    (GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    GYAssetClass ->
    m (GYTxSkeleton 'PlutusV3)
burnUserToken refScript adminAddr tokenId@(GYToken _ gyUserTN) = do
    adminPkh <- addressToPubKeyHash' adminAddr
    wineMintingPolicyGY <- getScriptFromUTxORef refScript
    let gyRedeemer = redeemerFromPlutusData (Burn (assetClassToPlutus tokenId))
    return $
        mconcat
            [ mustMint (GYMintReference refScript wineMintingPolicyGY) gyRedeemer gyUserTN (negate 1)
            , mustBeSignedBy adminPkh
            ]
burnUserToken _ _ _ = liftEitherToCustomException $ Left "invalid token"

-- |  Burn Reference Token Transaction Skeleton
burnRefToken ::
    (GYTxUserQueryMonad m) =>
    GYTxOutRef ->
    GYAddress ->
    GYAssetClass ->
    m (GYTxSkeleton 'PlutusV3)
burnRefToken refScript adminAddr gyRefAC@(GYToken _ gyRefTN) = do
    let refAC = assetClassToPlutus gyRefAC
    wineValidatorGY <- getScriptFromUTxORef refScript
    let gyRedeemer = redeemerFromPlutusData (Burn refAC)
    validatorAddr <- scriptAddress wineValidatorGY
    stateUTxO <- liftEitherToCustomException =<< lookupUTxOWithStateToken refAC validatorAddr
    (gyDatum, _) <- liftEitherToCustomException $ getInlineDatumAndValue stateUTxO
    adminPkh <- addressToPubKeyHash' adminAddr
    wineMintingPolicyGY <- getScriptFromUTxORef refScript
    return $
        mconcat
            [ mustHaveInput
                GYTxIn
                    { gyTxInTxOutRef = utxoRef stateUTxO
                    , gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript wineValidatorGY) (Just gyDatum) gyRedeemer
                    }
            , mustMint (GYMintReference refScript wineMintingPolicyGY) gyRedeemer gyRefTN (negate 1)
            , mustBeSignedBy adminPkh
            ]
burnRefToken _ _ _ = liftEitherToCustomException $ Left "invalid token"