module Main where

-- import GeniusYield.Test.Privnet.Setup
import Test.Tasty (defaultMain, testGroup)
import UnitTests

runTest :: IO ()
runTest =
    -- withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain
        ( testGroup
            "tests"
            [ testGroup
                "Clb"
                [ newBatchClb
                , updateWineBatchClb
                , mintBottleClb
                , updateWineBottleClb
                ]
                -- , testGroup
                --     "Privnet"
                --     [ newBatchPrivnet setup
                --     , updateBatchPrivnet setup
                --     , mintBottlePrivnet setup
                --     , updateBottlePrivnet setup
                --     , hydraTestsPrivnet setup
                --     ]
            ]
        )

main :: IO ()
main = runTest