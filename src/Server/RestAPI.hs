{-# LANGUAGE DeriveAnyClass #-}

module RestAPI where

import Control.Monad.Except

import GeniusYield.Types hiding (description, title)
import Network.HTTP.Types qualified as HttpTypes

import Control.Exception (throw)
import Control.Exception.Extra (try)
import Control.Lens ((&), (.~), (?~))
import Data.Swagger
import Data.Text.Encoding qualified
import IPFS
import Network.Wai.Middleware.Cors
import Offchain.Context (WineAdminContext (..), WineOffchainContext (..), runQuery)
import Offchain.Interactions (
    TokenData,
    TokenMeta,
    WineAction (..),
    WineInteraction (WineInteraction),
    WineTokenDT (Batch, Bottle),
    fromWineToken,
    getTokenData,
    updateTokenData,
 )
import Offchain.Operations
import Offchain.Transactions (runWineTx)
import RIO qualified as BS
import RIO.Text qualified as T
import Servant
import Servant.Swagger
import Servant.Swagger.UI

newtype User = User
    { user :: T.Text
    }
    deriving (Eq, Show)

data TxResp = TxResp
    { txId :: GYTxId
    , tokenRefId :: Maybe GYAssetClass
    }
    deriving (Generic, FromJSON, ToJSON, ToSchema)

mkTxResp :: (GYTxId, Maybe GYAssetClass) -> TxResp
mkTxResp = uncurry TxResp

data WineBatchDTO = WineBatchDTO
    { batch_meta :: TokenMeta
    , batch_data :: TokenData
    , batch_quantity :: (Integer, Integer)
    }
    deriving (Generic, FromJSON, ToJSON, ToSchema)

data WineBottleDTO = WineBottleDTO
    { bottle_meta :: TokenMeta
    , bottle_data :: TokenData
    , bottle_batch_id :: GYAssetClass
    }
    deriving (Generic, FromJSON, ToJSON, ToSchema)

type WineLookupAPI =
    Summary "Retrieve wine NFT details"
        :> Description "Fetches Wine NFT information based on the given token ID."
        :> "wine"
        :> Capture "Token Id" GYAssetClass
        :> Get '[JSON] WineTokenDT
type WineTxAPI =
    "tx"
        :> Capture "wait 1 conf?" Bool
        :> ( Summary "Mint a new batch NFT"
                :> Description "Creates a new batch NFT with metadata and initial quantity."
                :> "mint-batch"
                :> ReqBody '[JSON] WineBatchDTO
                :> Post '[JSON] TxResp
                :<|> Summary "Mint a new bottle NFT"
                    :> Description "Creates a new bottle NFT under an existing batch."
                    :> "mint-bottle"
                    :> ReqBody '[JSON] WineBottleDTO
                    :> Post '[JSON] TxResp
                :<|> Summary "Update an existing batch NFT"
                    :> Description "Modifies metadata and token data of a batch NFT."
                    :> "update-batch"
                    :> Capture "Batch Ref NFT id" GYAssetClass
                    :> ReqBody '[JSON] WineBatchDTO
                    :> Put '[JSON] TxResp
                :<|> Summary "Update an existing bottle NFT"
                    :> Description "Modifies metadata and token data of a bottle NFT."
                    :> "update-bottle"
                    :> Capture "Bottle ref NFT id" GYAssetClass
                    :> ReqBody '[JSON] WineBottleDTO
                    :> Put '[JSON] TxResp
                :<|> Summary "Burn a user-owned wine NFT"
                    :> Description "Deletes an NFT that belongs to a user."
                    :> "burn-user"
                    :> Capture "User NFT id" GYAssetClass
                    :> Delete '[JSON] TxResp
                :<|> Summary "Burn a reference wine NFT"
                    :> Description "Deletes a reference NFT."
                    :> "burn-ref"
                    :> Capture "Ref NFT id" GYAssetClass
                    :> Delete '[JSON] TxResp
           )

type WineAPI = WineTxAPI :<|> WineLookupAPI
type WineAPIPrivate = BasicAuth "user-realm" User :> (WineTxAPI :<|> WineLookupAPI)

type WineREST =
    SwaggerSchemaUI "swagger-ui" "swagger-api.json"
        :<|> WineAPIPrivate

wineAPI :: Proxy WineAPI
wineAPI = Proxy

wineAPIwithSwagger :: Proxy WineREST
wineAPIwithSwagger = Proxy

apiSwagger :: Swagger
apiSwagger =
    toSwagger wineAPI
        & info . title .~ "Wine Tokenization Service"
        & info . version .~ "1.0"
        & info . description ?~ "This is an API for tokenizing batches and bottles of wine"
        & Data.Swagger.info
            . Data.Swagger.license
            ?~ "GPL-3.0 license"
        & host .~ Nothing

wineServer :: WineOffchainContext -> ServerT WineREST IO
wineServer ctx =
    swaggerSchemaUIServerT apiSwagger -- Swagger API
        :<|> const -- usr
            ( txServer ctx -- Tx API
                :<|> handleGetNFT ctx -- Lookup API
            )

txServer :: WineOffchainContext -> ServerT WineTxAPI IO
txServer ctx wait =
    handleMintBatchTx ctx wait
        :<|> handleMintBottleTx ctx wait
        :<|> handleUpdateBatchTx ctx wait
        :<|> handleUpdateBottleTx ctx wait
        :<|> handleBurnUserTx ctx wait
        :<|> handleBurnRefTx ctx wait

handleMintBatchTx :: WineOffchainContext -> Bool -> WineBatchDTO -> IO TxResp
handleMintBatchTx ctx wait (WineBatchDTO i d s) = do
    d' <- addTokenDataToIPFS d
    mkTxResp <$> runWineTx wait ctx (WineInteraction MintBatch (Just (Batch i d' s)))

handleMintBottleTx :: WineOffchainContext -> Bool -> WineBottleDTO -> IO TxResp
handleMintBottleTx ctx wait (WineBottleDTO i d s) = do
    d' <- addTokenDataToIPFS d
    mkTxResp <$> runWineTx wait ctx (WineInteraction MintBottle (Just (Bottle i d' s)))

handleUpdateBatchTx :: WineOffchainContext -> Bool -> GYAssetClass -> WineBatchDTO -> IO TxResp
handleUpdateBatchTx ctx wait batchId (WineBatchDTO i d s) = do
    d' <- addTokenDataToIPFS d
    mkTxResp <$> runWineTx wait ctx (WineInteraction (UpdateBatch batchId) (Just (Batch i d' s)))

handleUpdateBottleTx :: WineOffchainContext -> Bool -> GYAssetClass -> WineBottleDTO -> IO TxResp
handleUpdateBottleTx ctx wait bottleId (WineBottleDTO i d s) = do
    d' <- addTokenDataToIPFS d
    mkTxResp <$> runWineTx wait ctx (WineInteraction (UpdateBottle bottleId) (Just (Bottle i d' s)))

handleBurnUserTx :: WineOffchainContext -> Bool -> GYAssetClass -> IO TxResp
handleBurnUserTx ctx wait tokenId = do
    mkTxResp <$> runWineTx wait ctx (WineInteraction (BurnUserToken tokenId) Nothing)

handleBurnRefTx :: WineOffchainContext -> Bool -> GYAssetClass -> IO TxResp
handleBurnRefTx ctx wait tokenId = do
    mkTxResp <$> runWineTx wait ctx (WineInteraction (BurnRefToken tokenId) Nothing)

handleGetNFT :: WineOffchainContext -> GYAssetClass -> IO WineTokenDT
handleGetNFT (WineOffchainContext (WineAdminContext{..}) providetCtx) tokenId = do
    w <- runQuery providetCtx $ getWineTokenFromValidator (assetClassToPlutus tokenId) wineValidatorRef
    case w of
        Left err -> throw $ err400{errBody = BS.fromString err}
        Right wt -> do
            let wtdt = fromWineToken wt
            let d = getTokenData wtdt
            d' <- getTokenDataFromIPFS d
            return $ updateTokenData wtdt d'

restAPIapp :: Text -> Text -> WineOffchainContext -> Application
restAPIapp usr pass ctx =
    cors (const $ Just simpleCorsResourcePolicy{corsRequestHeaders = [HttpTypes.hContentType]}) $
        serveWithContext wineAPIwithSwagger basicCtx $
            hoistServerWithContext wineAPIwithSwagger (Proxy :: Proxy '[BasicAuthCheck User]) (Servant.Handler . ExceptT . try) $
                wineServer ctx
  where
    basicCtx = basicAuthServerContext usr pass

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: Text -> Text -> BasicAuthCheck User
authCheck usr pass =
    let checkk (BasicAuthData username password) =
            if Data.Text.Encoding.decodeUtf8 username == usr && Data.Text.Encoding.decodeUtf8 password == pass
                then return (Authorized (User usr))
                else return Unauthorized
     in BasicAuthCheck checkk

{- | We need to supply our handlers with the right Context. In this case,
Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
tagged with "foo-tag" This context is then supplied to 'server' and threaded
to the BasicAuth HasServer handlers.
-}
basicAuthServerContext :: Text -> Text -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext usr pass = authCheck usr pass :. EmptyContext