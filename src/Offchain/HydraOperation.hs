{-# LANGUAGE OverloadedStrings #-}

module Offchain.HydraOperation where

import Data.Aeson
import Network.HTTP.Conduit hiding (httpLbs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
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
import Data.List (unwords, words)
import Text.Read (readMaybe)

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
findNFTUtxo tokenId _ = do
    let cmd = cardanoCliPath
        args = ["query", "utxo", "--address", walletAddress, "--socket-path", socketPath] ++ network  
    traceM $ "Running command: " ++ unwords (cmd:args)
    
    -- First verify if cardano-cli exists and is executable
    cliExists <- readProcess "which" [cardanoCliPath] ""
    traceM $ "Cardano-cli path check: " ++ cliExists
    
    -- Verify socket exists
    socketExists <- readProcess "ls" ["-l", socketPath] ""
    traceM $ "Socket path check: " ++ socketExists
    
    -- Now try to get UTXOs
    output <- readProcess cmd args ""
    traceM $ "Raw output: " ++ output  -- Log the raw output
    
    if null output
        then do
            traceM "No output from cardano-cli command"
            return Nothing
        else do
            -- Parse the output line by line
            let lines = words output
            traceM $ "Parsed lines: " ++ show lines
            
            -- Look for the token in the output
            let tokenStr = T.unpack tokenId
            case findTokenInOutput lines tokenStr of
                Just (txHash, txIx, amount) -> do
                    traceM $ "Found token in output: " ++ show (txHash, txIx, amount)
                    return $ Just UTxO
                        { txIn = T.pack $ txHash ++ "#" ++ txIx
                        , address = T.pack walletAddress
                        , value = HM.fromList [(tokenId, amount)]
                        }
                Nothing -> do
                    traceM $ "Token " ++ tokenStr ++ " not found in output"
                    return Nothing

findTokenInOutput :: [String] -> String -> Maybe (String, String, Integer)
findTokenInOutput [] _ = Nothing
findTokenInOutput (txHash:txIx:rest) tokenStr = do
    -- Look for the token in the rest of the line
    case findTokenAmount rest tokenStr of
        Just amount -> Just (txHash, txIx, amount)
        Nothing -> findTokenInOutput rest tokenStr
findTokenInOutput (_:rest) tokenStr = findTokenInOutput rest tokenStr

findTokenAmount :: [String] -> String -> Maybe Integer
findTokenAmount [] _ = Nothing
findTokenAmount (x:y:rest) tokenStr
    | x == tokenStr = readMaybe y
    | otherwise = findTokenAmount rest tokenStr
findTokenAmount (_:rest) tokenStr = findTokenAmount rest tokenStr

parseUTxO :: Text -> Text -> Value -> Maybe UTxO
parseUTxO tokenId txIn (Object o) = do
    traceM $ "Parsing UTXO: " ++ show o
    case KM.lookup "value" o of
        Just (Object valMap) -> do
            traceM $ "Value map: " ++ show valMap
            case KM.lookup (fromText tokenId) valMap of
                Just (Number amt) -> do
                    traceM $ "Found token " ++ T.unpack tokenId ++ " with amount: " ++ show amt
                    case floatingOrInteger amt of
                        Right i -> Just $ UTxO
                            { txIn = txIn
                            , address = Data.Maybe.fromMaybe "unknown" (KM.lookup "address" o >>= parseMaybe parseJSON)
                            , value = HM.fromList [(tokenId, i)]
                            }
                        Left _  -> do
                            traceM $ "Failed to convert amount to integer: " ++ show amt
                            Nothing
                _ -> do
                    traceM $ "Token " ++ T.unpack tokenId ++ " not found in value map"
                    Nothing
        _ -> do
            traceM "No value object found in UTXO"
            Nothing
parseUTxO _ _ _ = Nothing

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
