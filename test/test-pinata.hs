{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO
import System.IO (putStrLn)
import Offchain.Interactions (TokenData(..))
import Pinata
import Data.ByteString.Lazy.Char8 qualified as BS
import System.Environment (setEnv)
import Data.Text (Text)
import System.IO (readFile)
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    -- Set your Pinata credentials here for testing
    setEnv "PINATA_API_KEY" "c84bd99f13c11e6f846d"
    setEnv "PINATA_SECRET_KEY" "2b25c28b7811f8d00af49780497f93b706597e3a5fedd434e834e7c2bba3082b"
    
    putStrLn "Testing Pinata Integration..."
    
--     -- Test 1: Adding a simple string
--     putStrLn "\nTest 1: Adding a simple string"
--     result1 <- addStringToPinata "Hello, Pinata!"
--     putStrLn $ "Result: " ++ result1
    
    -- Test 2: Adding an image file
    putStrLn "\nTest 2: Adding an image file"
    imageData <- LBS.readFile "out/wine.jpg"  -- Using relative path from project root
    result2 <- addByteStringToPinata imageData
    putStrLn $ "Result: " ++ result2
    
    putStrLn "\nAll tests completed!" 