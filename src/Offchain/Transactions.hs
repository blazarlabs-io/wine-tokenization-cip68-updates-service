module Offchain.Transactions where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import GeniusYield.GYConfig
import GeniusYield.Types
import Offchain.Context
import Offchain.Interactions
import Offchain.OffchainUtils

------------------------------------------------------------------------------------------------

-- * Transaction Submitting

------------------------------------------------------------------------------------------------
submitTxAndWaitForConfirmation :: Bool -> GYProviders -> GYTx -> IO GYTxId
submitTxAndWaitForConfirmation wait ctxProviders gyTx = do
    gyTxId <- gySubmitTx ctxProviders gyTx
    printf (greenColorString "Submitted transaction: \n\t %s") gyTxId
    when wait do
        putStrLn (yellowColorString "Waiting for confirmation ...")
        gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
        putStrLn (yellowColorString "Confirmed")
    return gyTxId

buildWineTx ::
    WineOffchainContext ->
    WineInteraction ->
    IO (GYTxBody, Maybe GYAssetClass)
buildWineTx (WineOffchainContext (WineAdminContext{..}) providetCtx) interaction = do
    let adminAddr = addressFromPaymentKeyHash (cfgNetworkId . ctxCoreCfg $ providetCtx) (paymentKeyHash $ paymentVerificationKey winePaymentSigningKey)
    runTx providetCtx [adminAddr] adminAddr Nothing (interactionToTxSkeleton (WineInteractionCtx wineValidatorRef adminAddr) interaction)

runWineTx :: Bool -> WineOffchainContext -> WineInteraction -> IO (GYTxId, Maybe GYAssetClass)
runWineTx wait ctx interaction = do
    liftIO $ print interaction
    let key = winePaymentSigningKey . wineAdminCtx $ ctx
    (txBody, mac) <- buildWineTx ctx interaction
    let signedTx = signGYTxBody txBody [key]
    txId <- submitTxAndWaitForConfirmation wait (ctxProviders $ providerCtx ctx) signedTx
    return (txId, mac)
