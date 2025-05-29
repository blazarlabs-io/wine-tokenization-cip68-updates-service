module Offchain.OffchainUtils where

import Data.Aeson (decodeFileStrict)
import GeniusYield.Imports (HasCallStack)
import GeniusYield.TxBuilder (GYTxMonadException (GYApplicationException), GYTxQueryMonad, gyLogInfo')

import Control.Exception
import Control.Monad.Error.Class
import GeniusYield.HTTP.Errors
import PlutusTx.Builtins.HasOpaque (stringToBuiltinString)
import System.Directory.Extra (doesFileExist)

newtype CustomException = CustomException String
    deriving (Show)

instance Exception CustomException
instance IsGYApiError CustomException

liftEitherToCustomException :: (MonadError GYTxMonadException m) => Either String b -> m b
liftEitherToCustomException = either (throwError . GYApplicationException . CustomException) return

-- --------- TO BE USED OFFCHAIN

stringToBuiltinByteStringUtf8 :: String -> BuiltinByteString
stringToBuiltinByteStringUtf8 = encodeUtf8 . stringToBuiltinString

fromBuiltinByteStringUtf8 :: BuiltinByteString -> String
fromBuiltinByteStringUtf8 = init . tail . show . decodeUtf8

greenColorString :: String -> String
greenColorString s =
    "\n"
        ++ "\ESC[1;32m"
        ++ replicate 100 '='
        ++ "\n"
        ++ s
        ++ "\n"
        ++ replicate 100 '='
        ++ "\ESC[0m"
        ++ "\n"

yellowColorString :: String -> String
yellowColorString s =
    "\n"
        ++ "\ESC[1;93m"
        ++ s
        ++ "\ESC[0m"
        ++ "\n"

blueColorString :: String -> String
blueColorString s =
    "\n"
        ++ "\ESC[1;94m"
        ++ s
        ++ "\ESC[0m"
        ++ "\n"

logInfoGreen :: (GYTxQueryMonad m, HasCallStack) => String -> m ()
logInfoGreen = gyLogInfo' "" . greenColorString

logInfoYellow :: (GYTxQueryMonad m, HasCallStack) => String -> m ()
logInfoYellow = gyLogInfo' "" . yellowColorString

decodeConfigFile :: (FromJSON a) => FilePath -> IO a
decodeConfigFile path = do
    putStrLn $ yellowColorString $ "Parsing config file at " <> show path
    fileExist <- doesFileExist path
    if fileExist
        then do
            putStrLn "Found"
            v <- decodeFileStrict path
            case v of
                Just a -> return a
                Nothing -> error $ "Dedoding " <> show path <> " failed !"
        else
            throw $ CustomException $ show path <> " not found"