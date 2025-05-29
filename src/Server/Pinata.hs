{-# LANGUAGE NoImplicitPrelude #-}

module Server.Pinata where

import Control.Exception (throw)
import System.IO.Error (userError)
import Data.ByteString.Lazy.Char8 as CL hiding (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.Has
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import RIO
import RIO.Text qualified as Text
import System.IO.Extra (print)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.KeyMap as KM
import Offchain.Interactions (TokenData(..))
import System.Environment (getEnv)
import RIO.Process (ProcessContext, HasProcessContext(..), mkDefaultProcessContext)
import RIO.ByteString.Lazy qualified as Lazy
import Data.Hashable (hash)

-- Newtype wrappers for API credentials
newtype PinataApiKey = PinataApiKey Text
newtype PinataSecretKey = PinataSecretKey Text

-- Pinata API configuration
pinataApiUrl :: String
pinataApiUrl = "https://api.pinata.cloud"

pinataGatewayUrl :: String
pinataGatewayUrl = "https://gateway.pinata.cloud"

-- Get Pinata credentials from environment variables
getPinataCredentials :: IO (Text, Text)
getPinataCredentials = do
    apiKey <- getEnv "PINATA_API_KEY"
    secretKey <- getEnv "PINATA_SECRET_API_KEY"
    return (T.pack apiKey, T.pack secretKey)

data MySimpleApp = MySimpleApp
    { saLogFunc :: !LogFunc
    , saProcessContext :: !ProcessContext
    , saPinataApiKey :: !PinataApiKey
    , saPinataSecretKey :: !PinataSecretKey
    }

instance HasLogFunc MySimpleApp where
    logFuncL :: Lens' MySimpleApp LogFunc
    logFuncL = lens saLogFunc (\x y -> x{saLogFunc = y})

instance HasProcessContext MySimpleApp where
    processContextL :: Lens' MySimpleApp ProcessContext
    processContextL = lens saProcessContext (\x y -> x{saProcessContext = y})

instance Has PinataApiKey MySimpleApp where
    hasLens :: Data.Has.Lens MySimpleApp PinataApiKey
    hasLens = lens saPinataApiKey (\x y -> x{saPinataApiKey = y})

instance Has PinataSecretKey MySimpleApp where
    hasLens :: Data.Has.Lens MySimpleApp PinataSecretKey
    hasLens = lens saPinataSecretKey (\x y -> x{saPinataSecretKey = y})

addToPinata ::
    (MonadIO m) =>
    Text ->
    Text ->
    BS.ByteString ->
    m (Either String Text)
addToPinata apiKey secretKey raw = do
    RIO.traceM $ T.pack "Adding data to Pinata..."
    manager <- liftIO $ newManager tlsManagerSettings
    initialReq <- liftIO $ parseRequest $ pinataApiUrl ++ "/pinning/pinFileToIPFS"
    
    -- Create multipart form data
    let boundary = "------------------------" ++ show (hash raw)
        formData = BS.concat
            [ "--" <> BS.pack boundary <> "\r\n"
            , "Content-Disposition: form-data; name=\"file\"; filename=\"data.txt\"\r\n"
            , "Content-Type: application/octet-stream\r\n\r\n"
            , raw
            , "\r\n"
            , "--" <> BS.pack boundary <> "--\r\n"
            ]
    
    let req = initialReq
            { method = "POST"
            , requestHeaders = 
                [ ("pinata_api_key", TE.encodeUtf8 apiKey)
                , ("pinata_secret_api_key", TE.encodeUtf8 secretKey)
                , ("Content-Type", BS8.pack $ "multipart/form-data; boundary=" ++ boundary)
                ]
            , requestBody = RequestBodyLBS formData
            }
    
    RIO.traceM $ T.pack "Sending request to Pinata..."
    response <- liftIO $ httpLbs req manager
    RIO.traceM $ T.pack $ "Received response from Pinata: " ++ show (responseBody response)
    
    case eitherDecode (responseBody response) of
        Right (Object obj) -> 
            case KM.lookup "IpfsHash" obj of
                Just (String hash) -> do
                    RIO.traceM $ T.pack $ "Successfully added to Pinata. Hash: " ++ T.unpack hash
                    return $ Right hash
                _ -> do
                    RIO.traceM $ T.pack "No IPFS hash in response"
                    return $ Left "No IPFS hash in response"
        Left err -> do
            RIO.traceM $ T.pack $ "Failed to decode response: " ++ err
            return $ Left $ "Failed to decode response: " ++ err

getFromPinata ::
    (MonadIO m) =>
    Text ->
    Text ->
    String ->
    m (Either String Text)
getFromPinata apiKey secretKey cid = do
    RIO.traceM $ T.pack $ "Getting data from Pinata for CID: " ++ cid
    manager <- liftIO $ newManager tlsManagerSettings
    initialReq <- liftIO $ parseRequest $ pinataGatewayUrl ++ "/ipfs/" ++ cid
    
    let req = initialReq
            { method = "GET"
            , requestHeaders = 
                [ ("pinata_api_key", TE.encodeUtf8 apiKey)
                , ("pinata_secret_api_key", TE.encodeUtf8 secretKey)
                ]
            }
    
    RIO.traceM $ T.pack "Sending request to Pinata gateway..."
    response <- liftIO $ httpLbs req manager
    RIO.traceM $ T.pack "Received response from Pinata gateway"
    
    return $ Right $ decodeUtf8Lenient $ BS.toStrict $ responseBody response

addByteStringToPinata :: BS.ByteString -> IO String
addByteStringToPinata val = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    (apiKey, secretKey) <- getPinataCredentials
    
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saPinataApiKey = PinataApiKey apiKey
                    , saPinataSecretKey = PinataSecretKey secretKey
                    }
        runRIO app $ do
            PinataApiKey apiKey <- view hasLens
            PinataSecretKey secretKey <- view hasLens
            result <- addToPinata apiKey secretKey val
            case result of
                Right cid -> do
                    liftIO $ print cid
                    return ("ipfs://" <> Text.unpack cid)
                Left err -> throw $ userError err

addStringToPinata :: String -> IO String
addStringToPinata val = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    (apiKey, secretKey) <- getPinataCredentials
    
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saPinataApiKey = PinataApiKey apiKey
                    , saPinataSecretKey = PinataSecretKey secretKey
                    }
        runRIO app $ do
            PinataApiKey apiKey <- view hasLens
            PinataSecretKey secretKey <- view hasLens
            result <- addToPinata apiKey secretKey (BS.pack val)
            case result of
                Right cid -> do
                    liftIO $ print cid
                    return ("ipfs://" <> Text.unpack cid)
                Left err -> throw $ userError err

addTokenDataToPinata :: TokenData -> IO TokenData
addTokenDataToPinata (TokenData i d s) = do
    infoLink <- addStringToPinata i
    mdataLink <- mapM addStringToPinata d
    minsLink <- mapM addStringToPinata s
    return (TokenData infoLink mdataLink minsLink)

getStringFromPinata :: String -> IO String
getStringFromPinata str = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    (apiKey, secretKey) <- getPinataCredentials
    
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saPinataApiKey = PinataApiKey apiKey
                    , saPinataSecretKey = PinataSecretKey secretKey
                    }
        runRIO app $ do
            PinataApiKey apiKey <- view hasLens
            PinataSecretKey secretKey <- view hasLens
            result <- getFromPinata apiKey secretKey (RIO.drop 7 str)
            case result of
                Right content -> do
                    liftIO $ print content
                    return (Text.unpack content)
                Left err -> throw $ userError err

getTokenDataFromPinata :: TokenData -> IO TokenData
getTokenDataFromPinata (TokenData i d s) = do
    info <- getStringFromPinata i
    mdata <- mapM getStringFromPinata d
    mins <- mapM getStringFromPinata s
    return (TokenData info mdata mins) 
    