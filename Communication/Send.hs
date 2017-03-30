{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Communication.Send where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad            (mapM_, unless, void)
import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Communication.Format
import qualified Course.List              as CL
import           Files.State
import           Frontend.Course.File
import           Frontend.Initialize
import           Settings.Monad.Global

type API = "sync" :> Get '[JSON] ApiResponse

type Global' = Global DownloadSummary
type EnvS' = EnvS DownloadSummary

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = do
    liftIO $ putStrLn "Caught!"
    (envR, envS) <- liftIO setup
    let channel = envS
    asyncResult <- liftIO $ async $ resultTup envR envS
    liftIO $ loopListening channel asyncResult

loopListening :: EnvS' -> Async ApiResponse -> IO ApiResponse
loopListening channel asyncR = do
    next <- readChan channel
    print next
    maybeR <- poll asyncR
    maybe this reshape maybeR
  where
    this = loopListening channel asyncR
    reshape = return . ApiResponse . joinEither . fmap unResponse

resultTup :: EnvR -> EnvS' -> IO ApiResponse
resultTup envR envS = ApiResponse . (\(a,_,_) -> a) <$> runRWST (runExceptT mainG) envR envS

mainG :: Global' [(CL.Course, DownloadSummary)]
mainG = do
    this <- CL.thisTermCourse
    defPath <- lift $ asks getDefaultPath
    states <- mapM (downloadFromCourse defPath) this
    return $ zip this states

setup :: IO (EnvR, EnvS')
setup = do
    maybeEnvR <- tryGetEnvR
    envR <- maybe (error "Config read fault!") return maybeEnvR
    envS <- getEnvS
    return (envR, envS)

joinEither :: Either e (Either e a) -> Either e a
joinEither (Left ex)           = Left ex
joinEither (Right (Left ex))   = Left ex
joinEither (Right (Right val)) = Right val
