module UnitTests where

import Control.Monad.Error.Class
import GeniusYield.Test.Clb
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder (GYTxGameMonad)
import GeniusYield.TxBuilder.Errors
import Offchain.Interactions
import Test.Tasty
import TestRunners

newBatchTest :: (GYTxGameMonad m) => TestInfo -> m ()
newBatchTest TestInfo{..} = do
    scrRef <- deployReferenceScriptRun (w1 testWallets) (w1 testWallets)
    void $ mintBatchRun (w1 testWallets) scrRef (TokenMeta "name" "desc" "img") (TokenData "info" (Just "data") Nothing) 10

newBatchClb :: TestTree
newBatchClb =
    testGroup
        "Mint new batch tests"
        [ mkTestFor "[POSITIVE] Minting new wine batch" newBatchTest
        ]

newBatchPrivnet :: Setup -> TestTree
newBatchPrivnet setup =
    testGroup
        "Mint new batch tests"
        [ mkPrivnetTestFor_ "[POSITIVE] Minting new wine batch" newBatchTest
        ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup

---------
---------
---------
---------

updateBatchTestPositive :: (GYTxGameMonad m) => TestInfo -> m ()
updateBatchTestPositive TestInfo{..} = do
    scrRef <- deployReferenceScriptRun (w1 testWallets) (w1 testWallets)
    batchRefAC <- mintBatchRun (w1 testWallets) scrRef (TokenMeta "name" "desc" "img") (TokenData "info" (Just "data") Nothing) 10
    void $ updateBatchRun (w1 testWallets) scrRef batchRefAC (TokenMeta "name2" "desc2" "img2") (TokenData "info2" Nothing Nothing) 10

updateBatchTestNegative :: (GYTxGameMonad m) => TestInfo -> m ()
updateBatchTestNegative TestInfo{..} = do
    scrRef <- deployReferenceScriptRun (w1 testWallets) (w1 testWallets)
    batchRefAC <- mintBatchRun (w1 testWallets) scrRef (TokenMeta "name" "desc" "img") (TokenData "info" (Just "data") Nothing) 10
    void $ updateBatchRun (w1 testWallets) scrRef batchRefAC (TokenMeta "name2" "desc2" "img2") (TokenData "info2" Nothing Nothing) 12 --  updating quantity must fail

updateWineBatchClb :: TestTree
updateWineBatchClb =
    testGroup
        "Update batch tests"
        [ mkTestFor "[POSTIVE] Updating wine batch name" updateBatchTestPositive
        , mkTestFor "[NEGATIVE] Updating wine batch quantity" (mustFail . updateBatchTestNegative)
        ]

updateBatchPrivnet :: Setup -> TestTree
updateBatchPrivnet setup =
    testGroup
        "Update batch tests"
        [ mkPrivnetTestFor_ "[POSTIVE] Updating a wine batch" updateBatchTestPositive
        , mkPrivnetTestFor_
            "[NEGATIVE] Updating wine batch quantity"
            ( handleError
                ( \case
                    GYBuildTxException GYBuildTxBodyErrorAutoBalance{} -> pure ()
                    e -> throwError e
                )
                . updateBatchTestNegative
            )
        ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup

---------
---------
---------
---------

mintBottleTest :: (GYTxGameMonad m) => TestInfo -> m ()
mintBottleTest TestInfo{..} = do
    scrRef <- deployReferenceScriptRun (w1 testWallets) (w1 testWallets)
    batchRefAC <- mintBatchRun (w1 testWallets) scrRef (TokenMeta "name" "desc" "img") (TokenData "info" (Just "data") Nothing) 10
    void $ mintBottleRun (w1 testWallets) scrRef (TokenMeta "name2" "desc2" "img2") (TokenData "info2" Nothing Nothing) batchRefAC

mintBottleClb :: TestTree
mintBottleClb =
    testGroup
        "Mint new bottle tests"
        [ mkTestFor "[POSTIVE] Minting wine bottle" mintBottleTest
        ]

mintBottlePrivnet :: Setup -> TestTree
mintBottlePrivnet setup =
    testGroup
        "Mint new bottle tests"
        [ mkPrivnetTestFor_ "[POSTIVE] Minting wine bottle" mintBottleTest
        ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup

---------
---------
---------
---------

updateBottleTestPositive :: (GYTxGameMonad m) => TestInfo -> m ()
updateBottleTestPositive TestInfo{..} = do
    scrRef <- deployReferenceScriptRun (w1 testWallets) (w1 testWallets)
    batchRefAC <- mintBatchRun (w1 testWallets) scrRef (TokenMeta "name" "desc" "img") (TokenData "info" (Just "data") Nothing) 10
    bottleRefAC <- mintBottleRun (w1 testWallets) scrRef (TokenMeta "name2" "desc2" "img2") (TokenData "info2" Nothing Nothing) batchRefAC
    void $ updateBottleRun (w1 testWallets) scrRef bottleRefAC (TokenMeta "name3" "desc3" "img3") (TokenData "info3" (Just "data1") (Just "data2")) batchRefAC

updateWineBottleClb :: TestTree
updateWineBottleClb =
    testGroup
        "Update bottle tests"
        [ mkTestFor "[POSTIVE] Updating wine bottle " updateBottleTestPositive
        ]

updateBottlePrivnet :: Setup -> TestTree
updateBottlePrivnet setup =
    testGroup
        "Update batch tests"
        [ mkPrivnetTestFor_ "[POSTIVE] Updating a wine bottle " updateBottleTestPositive
        ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup