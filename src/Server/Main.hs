module Main where

import Control.Monad qualified

import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text qualified
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.Wai.Handler.Warp (
    HostPreference,
    defaultSettings,
    runSettings,
    setHost,
    setPort,
 )
import Offchain.Context
import Offchain.OffchainUtils
import Parameters
import RIO (IsString (..))
import RestAPI (apiSwagger, restAPIapp)
import System.Environment
import Text.Printf qualified as Printf
import Text.Read (read)

getPortFromEnv :: IO Int
getPortFromEnv = do
    eport <- lookupEnv "PORT"
    case eport of
        Nothing -> return 8082
        Just p -> return (read p)

main :: IO ()
main = do
    args <- getArgs
    let (usr, pass) = case args of
            [x, y] -> (Data.Text.pack x, Data.Text.pack y)
            _ -> ("cardano" :: Text, "lovelace" :: Text) -- Default user and password for Basic Auth
    putStrLn $ yellowColorString "Writing Swagger file ..."
    BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)
    putStrLn $ yellowColorString "Reading atlas configuration file ..."
    atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
    putStrLn $ yellowColorString "Reading admin configuration file ..."
    adminContext <- decodeConfigFile @WineAdminContext adminConfig
    putStrLn $ yellowColorString "Loading Providers ..."
    withCfgProviders atlasConfig (read @GYLogNamespace "wine-server") $ \providers -> do
        let addminAddr = addressFromPaymentKeyHash (cfgNetworkId atlasConfig) (paymentKeyHash . paymentVerificationKey . winePaymentSigningKey $ adminContext)
        putStrLn $ greenColorString $ Printf.printf "Admin address = %s" addminAddr
        let pCtx = ProviderCtx atlasConfig providers
        balance <- runQuery pCtx $ queryBalance addminAddr
        let (lovelaces, _) = valueSplitAda balance
        let ada = lovelaceOf lovelaces
        putStrLn $ greenColorString $ Printf.printf "Admin lovelaces balance = %s" (show ada)

        Control.Monad.when (ada < 1000) $ putStrLn $ yellowColorString "Balance is low ..."

        --- Server
        let wineOffchainContext = WineOffchainContext adminContext pCtx
        let host = "0.0.0.0"
        port <- getPortFromEnv
        putStrLn $ yellowColorString $ "Starting server at : http://" <> host <> ":" <> show port
        putStrLn $ greenColorString $ "Swagget-UI at : http://" <> host <> ":" <> show port <> "/swagger-ui"
        let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings -- host and port customized for heroku
        runSettings settings $ restAPIapp usr pass wineOffchainContext
