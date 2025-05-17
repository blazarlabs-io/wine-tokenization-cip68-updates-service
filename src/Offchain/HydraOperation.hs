{-# LANGUAGE OverloadedStrings #-}

module Offchain.HydraOperation where

import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Aeson (encode, Value(Object), object, (.=))
import Data.Aeson.Types (parseMaybe, Key)
import Onchain.HydraType
import Network.HTTP.Client (newManager, httpLbs, parseRequest, RequestBody(RequestBodyLBS))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Process (readProcess)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Aeson.Key (fromText, toText)
import Debug.Trace (traceM)

cardanoCliPath :: String
cardanoCliPath = "cardano-cli"

walletAddress :: String
walletAddress = "addr_test1wq6f2ajeq3cnns4zs2kvpd7x0pc6tmcmkely45nc054p3ysdeu6cg"  -- Replace with actual address

network :: [String]
network = ["--testnet-magic", "2"]  -- or ["--mainnet"]

socketPath :: String
socketPath = "/root/cardano-node/config/preview/preview.socket"  -- Update this path to your actual socket path

hydraNodeUrl :: String
hydraNodeUrl = "http://localhost:4001"

-- Find the UTXO holding the NFT
findNFTUtxo :: Text -> Integer -> IO (Maybe UTxO)
findNFTUtxo tokenId neededAmount = do
    let cmd = cardanoCliPath
        args = ["query", "utxo", "--address", walletAddress, "--socket-path", socketPath] ++ network
    output <- readProcess cmd args ""
    traceM $ "Output: " ++ output  -- Log the output
    let parsed = decode (BL.pack output) :: Maybe Object
    traceM $ "Parsed: " ++ show parsed  -- Log the parsed result
    let matches = case parsed of
          Just utxos -> do
            (txIx, o) <- KM.toList utxos
            case (parseUTxO tokenId neededAmount (toText txIx) o) of
              Just utxo -> [utxo]
              Nothing -> []
          Nothing -> []
    traceM $ "Found matches: " ++ show matches  -- Log the matches
    return $ Data.Maybe.listToMaybe matches

parseUTxO :: Text -> Integer -> Text -> Value -> Maybe UTxO
parseUTxO tokenId minAmount txIn (Object o) = do
    Object valMap <- KM.lookup "value" o
    tokenVal <- case KM.lookup (fromText tokenId) valMap of
      Just (Number amt) -> case floatingOrInteger amt of
        Right i -> Just i       
        Left _  -> Nothing
      _ -> Nothing
    if tokenVal >= minAmount
        then Just $ UTxO
            { txIn = txIn
            , address = Data.Maybe.fromMaybe "unknown" (KM.lookup "address" o >>= parseMaybe parseJSON)
            , value = HM.fromList [(tokenId, tokenVal)]
            }
        else Nothing
parseUTxO _ _ _ _ = Nothing

-- Commit UTXO to Hydra node
commitUTxO :: UTxO -> IO String
commitUTxO utxo = do
    manager <- newManager tlsManagerSettings
    initialReq <- parseRequest (hydraNodeUrl ++ "/commit")
    let body = encode $ object ["utxo" .= utxo]
    let req = initialReq { method = "POST", requestBody = RequestBodyLBS body }
    response <- httpLbs req manager
    let body = responseBody response
    case eitherDecode body :: Either String Value of
      Right _ -> return ("Committed" :: String)
      Left _ -> return ("error" :: String)
    return "Committed"

-- commitUTxO :: CommitRequest -> IO HydraResponse
-- commitUTxO = postToHydra "/commit"

-- decommitUTxO :: CommitRequest -> IO HydraResponse
-- decommitUTxO = postToHydra "/decommit"

-- updateUTxO :: CommitRequest -> IO HydraResponse
-- updateUTxO = postToHydra "/update"

-- postToHydra :: String -> CommitRequest -> IO HydraResponse
-- postToHydra endpoint reqData = do
--     initReq <- parseRequest (hydraNodeUrl ++ endpoint)
--     let req = setRequestMethod "POST"
--             $ setRequestHeader "Content-Type" ["application/json"]
--             $ setRequestBodyLBS (encode reqData)
--             $ initReq
--     response <- httpLBS req
--     let body = getResponseBody response
--     case eitherDecode body of
--       Right parsed -> return parsed
--       Left _ -> return $ HydraResponse "error" Nothing Nothing


-- -- Update metadata (mocked)
-- updateNFTMetadata :: TokenId -> Text -> IO ()
-- updateNFTMetadata tokenId newMeta = do
--     putStrLn $ "Mock metadata update for " ++ show tokenId ++ " to " ++ T.unpack newMeta
--     -- Replace with logic that updates datum on-chain or via Hydra tx

-- -- Update metadata (mocked)
-- updateNFTMetadata :: TokenId -> Text -> IO ()
-- updateNFTMetadata tokenId newMeta = do
--     putStrLn $ "Mock metadata update for " ++ show tokenId ++ " to " ++ T.unpack newMeta
--     -- Replace with logic that updates datum on-chain or via Hydra tx

-- -- Decommit UTXO (mocked)
-- decommitUtxo :: String -> IO ()
-- decommitUtxo utxoId = do
--     manager <- newManager tlsManagerSettings
--     initialReq <- parseRequest "http://localhost:8082/decommit"
--     let body = encode $ object ["utxo" .= utxoId]
--     let req = initialReq { method = "POST", requestBody = RequestBodyLBS body }
--     _ <- httpLbs req manager
--     putStrLn $ "Decommitted UTXO " ++ utxoId
