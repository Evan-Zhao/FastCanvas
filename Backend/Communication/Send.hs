{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Communication.Send where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Course.File
import qualified Course.List                 as CL
import           Files.State
import           Settings.Exception.Prettify
import           Settings.Monad.Global

type Excepted a = Either SomeException a

type SyncResult = Excepted [(CL.Course, DownloadSummary)]
type CoursesResult = Excepted [CL.Course]

type API = "sync" :> Get '[JSON] SyncResult
      :<|> "courses" :> Get '[JSON] CoursesResult

server :: Server API
server = liftIO sync :<|> liftIO courses

courses :: IO CoursesResult
courses = do
    putStrLn "endpoint: \"courses\""
    runGlobal CL.thisTermCourse

sync :: IO SyncResult
sync = do
    putStrLn "endpoint: \"sync\""
    (channel, asyncResult) <- runGlobalAsync syncG
    loopListening channel asyncResult

loopListening :: EnvS -> Async SyncResult -> IO SyncResult
loopListening channel asyncR = do
    next <- readChan channel
    print next
    maybeR <- poll asyncR
    maybe this reshape maybeR
  where
    this = loopListening channel asyncR
    reshape = return . prettifyException . joinEither

runGlobal :: Global a -> IO (Excepted a)
runGlobal g = do
    (envR, envS) <- setup
    (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS

runGlobalAsync :: Global a -> IO (EnvS, Async (Excepted a))
runGlobalAsync g = do
    (envR, envS) <- setup
    asynced <- async $ (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS
    return (envS, asynced)

syncG :: Global [(CL.Course, DownloadSummary)]
syncG = do
    this <- CL.thisTermCourse
    defPath <- lift $ asks getDefaultPath
    states <- mapM (downloadFromCourse defPath) this
    return $ zip this states

setup :: IO (EnvR, EnvS)
setup = do
    maybeEnvR <- tryGetEnvR
    envR <- maybe (error "Config read fault!") return maybeEnvR
    envS <- getEnvS
    return (envR, envS)

joinEither :: Either e (Either e a) -> Either e a
joinEither (Left ex)           = Left ex
joinEither (Right (Left ex))   = Left ex
joinEither (Right (Right val)) = Right val

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy
