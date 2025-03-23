{-# LANGUAGE NoImplicitPrelude #-}

module IPFS where

import Control.Exception (throw)
import Data.ByteString.Lazy.Char8 as CL hiding (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Has
import Network.IPFS
import Network.IPFS qualified as IPFS
import Network.IPFS.Add.Error
import Network.IPFS.Add.Error qualified as IPFS.Add
import Network.IPFS.Process
import Network.IPFS.Process.Error qualified as Process
import Network.IPFS.Types
import Network.IPFS.Types qualified as IPFS
import Offchain.Interactions (TokenData (..))
import RIO
import RIO.ByteString.Lazy qualified as Lazy
import RIO.Process
import RIO.Text qualified as Text
import System.IO.Extra (print)

data MySimpleApp = MySimpleApp
    { saLogFunc :: !LogFunc
    , saProcessContext :: !ProcessContext
    , saBinPath :: !IPFS.BinPath
    , saTimeout :: !IPFS.Timeout
    }
instance HasLogFunc MySimpleApp where
    logFuncL :: Lens' MySimpleApp LogFunc
    logFuncL = lens saLogFunc (\x y -> x{saLogFunc = y})
instance HasProcessContext MySimpleApp where
    processContextL :: Lens' MySimpleApp ProcessContext
    processContextL = lens saProcessContext (\x y -> x{saProcessContext = y})

instance Has IPFS.BinPath MySimpleApp where
    hasLens :: Data.Has.Lens MySimpleApp IPFS.BinPath
    hasLens = lens saBinPath (\x y -> x{saBinPath = y})

instance Has IPFS.Timeout MySimpleApp where
    hasLens :: Data.Has.Lens MySimpleApp IPFS.Timeout
    hasLens = lens saTimeout (\x y -> x{saTimeout = y})

instance MonadLocalIPFS (RIO MySimpleApp) where
    runLocal :: [IPFS.Opt] -> Lazy.ByteString -> RIO MySimpleApp (Either Process.Error RawMessage)
    runLocal opts arg = do
        IPFS.BinPath ipfs <- view hasLens
        IPFS.Timeout secs <- view hasLens
        let opts' = ("--timeout=" <> show secs <> "s") : opts
        runProc readProcess ipfs (byteStringInput arg) byteStringOutput opts' >>= \case
            (ExitSuccess, contents, _) ->
                return $ Right contents
            (ExitFailure _, _, stdErr)
                | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
                    return . Left $ Process.Timeout secs
                | otherwise ->
                    return . Left $ Process.UnknownErr stdErr

addToIPFS ::
    (MonadLocalIPFS m) =>
    Lazy.ByteString ->
    m (Either IPFS.Add.Error Text)
addToIPFS raw =
    IPFS.runLocal ["add", "-HQ"] raw >>= \case
        Right result ->
            case CL.lines result of
                [cid] ->
                    return . Right . decodeUtf8Lenient $ Lazy.toStrict cid
                bad ->
                    return . Left . UnexpectedOutput $ (textDisplay . displayShow) bad
        Left err ->
            return . Left . UnknownAddErr $ (textDisplay . displayShow) err

getFromIPFS ::
    (MonadLocalIPFS m) =>
    String ->
    m (Either Process.Error Text) -- ToJSON
getFromIPFS cid =
    IPFS.runLocal ["cat", cid] "" >>= \case
        Right result -> return . Right . decodeUtf8Lenient . Lazy.toStrict $ result
        Left err ->
            return . Left $ err

addByteStringToIPFS :: BS.ByteString -> IO String
addByteStringToIPFS val = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saBinPath = "ipfs"
                    , saTimeout = 10
                    }
        runRIO app $ do
            result <- addToIPFS val
            case result of
                Right cid -> do
                    liftIO $ print cid
                    return ("ipfs://" <> Text.unpack cid)
                Left err -> throw err

addStringToIPFS :: String -> IO String
addStringToIPFS val = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saBinPath = "ipfs"
                    , saTimeout = 10
                    }
        runRIO app $ do
            result <- addToIPFS (fromString val)
            case result of
                Right cid -> do
                    liftIO $ print cid
                    return ("ipfs://" <> Text.unpack cid)
                Left err -> throw err

addTokenDataToIPFS :: TokenData -> IO TokenData
addTokenDataToIPFS (TokenData i d s) = do
    infoLink <- addStringToIPFS i
    mdataLink <- mapM addStringToIPFS d
    minsLink <- mapM addStringToIPFS s
    return (TokenData infoLink mdataLink minsLink)

getStringFromIPFS :: String -> IO String
getStringFromIPFS str = do
    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
    processContext <- mkDefaultProcessContext
    withLogFunc logOptions $ \logFunc -> do
        let app =
                MySimpleApp
                    { saLogFunc = logFunc
                    , saProcessContext = processContext
                    , saBinPath = "ipfs"
                    , saTimeout = 10
                    }
        runRIO app $ do
            result <- getFromIPFS (RIO.drop 7 str)
            case result of
                Right cid -> do
                    liftIO $ print cid
                    return (Text.unpack cid)
                Left err -> throw err

getTokenDataFromIPFS :: TokenData -> IO TokenData
getTokenDataFromIPFS (TokenData i d s) = do
    info <- getStringFromIPFS i
    mdata <- mapM getStringFromIPFS d
    mins <- mapM getStringFromIPFS s
    return (TokenData info mdata mins)
