{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Communication.Send where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Course.File
import qualified Course.List                         as CL
import           Files.State
import           Peek.Peek
import           Settings.Exception.GeneralException
import           Settings.Monad.Global

type Excepted a = Either SomeException a
type ExDisplayed a = Either String a

type Sync = [(CL.Course, DownloadSummary)]
type Courses = [CL.Course]

type API = "sync" :> Get '[JSON] (ExDisplayed Sync)
      :<|> "courses" :> Get '[JSON] (ExDisplayed Courses)
      :<|> "peek" :> Capture "path" FilePath :> Get '[JSON] PeekResponse

server :: Server API
server = liftIO sync :<|> liftIO courses :<|> (liftIO . peekAt)

courses :: IO (ExDisplayed Courses)
courses = do
    putStrLn "endpoint: \"courses\""
    leftToString <$> runGlobal CL.thisTermCourse

sync :: IO (ExDisplayed Sync)
sync = do
    putStrLn "endpoint: \"sync\""
    (channel, asyncResult) <- runGlobalAsync syncG
    leftToString <$> loopListening channel asyncResult

loopListening :: EnvS -> Async (Excepted Sync) -> IO (Excepted Sync)
loopListening channel asyncR = do
    next <- readChan channel
    print next
    maybeR <- poll asyncR
    maybe this reshape maybeR
  where
    this = loopListening channel asyncR
    reshape = return . joinEither

runGlobal :: Global a -> IO (Excepted a)
runGlobal g = do
    (envR, envS) <- setup
    (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS

runGlobalAsync :: Global a -> IO (EnvS, Async (Excepted a))
runGlobalAsync g = do
    (envR, envS) <- setup
    asynced <- async $ (\(a,_,_) -> a) <$> runRWST (runExceptT g) envR envS
    return (envS, asynced)

syncG :: Global Sync
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
