{-# LANGUAGE OverloadedStrings #-}

module Main where

import Offchain.HydraOperation
import Onchain.HydraType
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import Control.Monad (when)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [tokenId, _] -> do  -- Changed to ignore amount
            let tokenIdText = T.pack tokenId

            putStrLn $ "Searching for UTXO with token: " ++ tokenId
            maybeUtxo <- findNFTUtxo tokenIdText 0  -- Amount is ignored now
            
            case maybeUtxo of
                Just utxo -> do
                    putStrLn "Found UTXO:"
                    putStrLn $ "  Transaction Input: " ++ T.unpack (txIn utxo)
                    putStrLn $ "  Address: " ++ T.unpack (address utxo)
                    putStrLn $ "  Value: " ++ show (value utxo)
                    
                    putStrLn "\nAttempting to commit UTXO to Hydra node..."
                    result <- commitUTxO utxo
                    putStrLn $ "Commit result: " ++ result
                
                Nothing -> putStrLn "No matching UTXO found"
        
        _ -> putStrLn "Usage: test-hydra <tokenId> <amount>" 