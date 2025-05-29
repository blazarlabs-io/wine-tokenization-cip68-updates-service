module Offchain.Context where

import GeniusYield.GYConfig ( GYCoreConfig(cfgNetworkId) )

import GeniusYield.TxBuilder
    ( buildTxBody,
      runGYTxBuilderMonadIO,
      runGYTxQueryMonadIO,
      GYTxSkeleton,
      GYTxBuilderMonadIO,
      GYTxQueryMonadIO )
import GeniusYield.Types
    ( GYProviders,
      GYAddress,
      GYTxOutRef,
      GYAssetClass,
      GYPaymentSigningKey,
      PlutusVersion(PlutusV3),
      GYTxBody,
      GYTxOutRefCbor(getTxOutRefHex) )

data ProviderCtx = ProviderCtx
    { ctxCoreCfg :: !GYCoreConfig
    , ctxProviders :: !GYProviders
    }

data WineAdminContext = WineAdminContext
    { wineValidatorRef :: GYTxOutRef
    , winePaymentSigningKey :: GYPaymentSigningKey
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data WineOffchainContext = WineOffchainContext
    { wineAdminCtx :: WineAdminContext
    , providerCtx :: ProviderCtx
    }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a
runQuery ctx q = do
    let nid = cfgNetworkId $ ctxCoreCfg ctx
        providers = ctxProviders ctx
    runGYTxQueryMonadIO nid providers q

-- | Tries to build for given skeleton.
runTx' ::
    ProviderCtx ->
    -- | User's used addresses.
    [GYAddress] ->
    -- | User's change address.
    GYAddress ->
    -- | Browser wallet's reserved collateral (if set).
    Maybe GYTxOutRefCbor ->
    GYTxBuilderMonadIO (GYTxSkeleton v) ->
    IO GYTxBody
runTx' ctx addrs addr collateral skeleton = do
    let nid = cfgNetworkId $ ctxCoreCfg ctx
        providers = ctxProviders ctx
    runGYTxBuilderMonadIO
        nid
        providers
        addrs
        addr
        ( collateral
            >>= ( \c ->
                    Just
                        ( getTxOutRefHex c
                        , True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                        )
                )
        )
        (skeleton >>= buildTxBody)

-- | Tries to build for given skeleton.
runTx ::
    ProviderCtx ->
    -- | User's used addresses.
    [GYAddress] ->
    -- | User's change address.
    GYAddress ->
    -- | Browser wallet's reserved collateral (if set).
    Maybe GYTxOutRefCbor ->
    GYTxBuilderMonadIO (GYTxSkeleton 'PlutusV3, Maybe GYAssetClass) ->
    IO (GYTxBody, Maybe GYAssetClass)
runTx ctx addrs addr collateral skeleton = do
    let nid = cfgNetworkId $ ctxCoreCfg ctx
        providers = ctxProviders ctx

    runGYTxBuilderMonadIO
        nid
        providers
        addrs
        addr
        ( collateral
            >>= ( \c ->
                    Just
                        ( getTxOutRefHex c
                        , True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                        )
                )
        )
        (skeleton >>= \(txSklt, mac) -> (,mac) <$> buildTxBody txSklt)