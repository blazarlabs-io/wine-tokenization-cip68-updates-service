{-# OPTIONS_GHC -Wno-partial-fields #-}

module Offchain.Interactions where

import Control.Exception
import Data.Swagger (ToSchema)
import Data.Tuple.Extra
import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import Offchain.OffchainUtils
import Offchain.Operations
import Onchain.Types hiding (MintBottle)
import PlutusLedgerApi.V1.Value (AssetClass)

data TokenMeta
    = TokenMeta
    { name :: String
    , image :: String
    , description :: String
    }
    deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TokenData
    = TokenData
    { info :: String
    , mdata :: Maybe String
    , minscr :: Maybe String
    }
    deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data WineTokenDT
    = Batch
        { batch_meta :: TokenMeta
        , batch_data :: TokenData
        , batch_quantity :: (Integer, Integer)
        }
    | Bottle
        { bottle_meta :: TokenMeta
        , bottle_data :: TokenData
        , bottle_batch_id :: GYAssetClass
        }
    deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

fromWineTokenMeta :: WineTokenMeta -> TokenMeta
fromWineTokenMeta (WineTokenMeta n d i) = TokenMeta (fromBuiltinByteStringUtf8 n) (fromBuiltinByteStringUtf8 d) (fromBuiltinByteStringUtf8 i)

fromWineTokenData :: WineTokenData -> TokenData
fromWineTokenData (WineTokenData i md is) = TokenData (fromBuiltinByteStringUtf8 i) (fromBuiltinByteStringUtf8 <$> md) (fromBuiltinByteStringUtf8 <$> is)

fromWineToken :: WineToken -> WineTokenDT
fromWineToken (WineToken m d (BatchToken (BatchQuantity q r))) = Batch (fromWineTokenMeta m) (fromWineTokenData d) (q, r)
fromWineToken (WineToken m d (BottleToken batchId)) = Bottle (fromWineTokenMeta m) (fromWineTokenData d) (either throw id $ assetClassFromPlutus' batchId)

mkWineBatch :: TokenMeta -> TokenData -> (Integer, Integer) -> WineToken
mkWineBatch (TokenMeta name image desc) (TokenData info mdatainfo _) (q, r) =
    if q #> 0
        then
            WineToken
                ( WineTokenMeta
                    { wiName = stringToBuiltinByteStringUtf8 name
                    , wiDesc = stringToBuiltinByteStringUtf8 desc
                    , wiImageURI = stringToBuiltinByteStringUtf8 image
                    }
                )
                ( WineTokenData
                    { wdInfoURI = stringToBuiltinByteStringUtf8 info
                    , wdMaybeDataURI = stringToBuiltinByteStringUtf8 #<$> mdatainfo
                    , wdMaybeIncriptionsURI = Nothing
                    }
                )
                ( BatchToken
                    (BatchQuantity q r)
                )
        else throw $ CustomException "invalid quantity"

mkWineBottle :: TokenMeta -> TokenData -> AssetClass -> WineToken
mkWineBottle (TokenMeta name image desc) (TokenData info mdatainfo minscr) batchId =
    WineToken
        ( WineTokenMeta
            { wiName = stringToBuiltinByteStringUtf8 name
            , wiDesc = stringToBuiltinByteStringUtf8 desc
            , wiImageURI = stringToBuiltinByteStringUtf8 image
            }
        )
        ( WineTokenData
            { wdInfoURI = stringToBuiltinByteStringUtf8 info
            , wdMaybeDataURI = stringToBuiltinByteStringUtf8 #<$> mdatainfo
            , wdMaybeIncriptionsURI = stringToBuiltinByteStringUtf8 #<$> minscr
            }
        )
        (BottleToken batchId)

type Quantity = Integer

type BottleID = GYAssetClass
type BatchID = GYAssetClass

data WineAction
    = MintBatch
    | MintBottle
    | UpdateBatch GYAssetClass
    | UpdateBottle GYAssetClass
    | BurnUserToken GYAssetClass
    | BurnRefToken GYAssetClass
    deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data WineInteraction = WineInteraction
    { action :: WineAction
    , token :: Maybe WineTokenDT
    }
    deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getTokenData :: WineTokenDT -> TokenData
getTokenData (Batch _ tdata _) = tdata
getTokenData (Bottle _ tdata _) = tdata

updateTokenData :: WineTokenDT -> TokenData -> WineTokenDT
updateTokenData action new_tdata = case action of
    Batch tmeta _tdata q -> Batch tmeta new_tdata q
    Bottle tmeta _tdata batchId -> Bottle tmeta new_tdata batchId

data WineInteractionCtx = WineInteractionCtx GYTxOutRef GYAddress

interactionToTxSkeleton ::
    (HasCallStack, GYTxUserQueryMonad m) =>
    WineInteractionCtx ->
    WineInteraction ->
    m (GYTxSkeleton 'PlutusV3, Maybe GYAssetClass)
interactionToTxSkeleton (WineInteractionCtx wineValidatorRef adminAddr) interaction = do
    case interaction of
        WineInteraction MintBatch (Just (Batch tmeta tdata q)) -> second Just <$> newBatchSk wineValidatorRef adminAddr (mkWineBatch tmeta tdata q)
        WineInteraction MintBottle (Just (Bottle tmeta tdata batchId)) -> second Just <$> newBottleSk wineValidatorRef adminAddr (mkWineBottle tmeta tdata (assetClassToPlutus batchId))
        WineInteraction (UpdateBatch batchId) (Just (Batch tmeta tdata q)) -> (,Nothing) <$> updateBatchSk wineValidatorRef adminAddr batchId (mkWineBatch tmeta tdata q)
        WineInteraction (UpdateBottle bottleId) (Just (Bottle tmeta tdata batchId)) -> (,Nothing) <$> updateBottleSk wineValidatorRef adminAddr bottleId (mkWineBottle tmeta tdata (assetClassToPlutus batchId))
        WineInteraction (BurnUserToken tokenId) Nothing -> (,Nothing) <$> burnUserToken wineValidatorRef adminAddr tokenId
        WineInteraction (BurnRefToken tokenId) Nothing -> (,Nothing) <$> burnRefToken wineValidatorRef adminAddr tokenId
        _ -> liftEitherToCustomException $ Left "Invalid Wine Interraction"