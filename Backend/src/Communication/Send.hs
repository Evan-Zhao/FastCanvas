{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Communication.Send where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Endpoint.Courses
import qualified Endpoint.Peek            as P
import           Endpoint.Sync
import           Files.State
import           Settings.Monad.Global

type API = "sync" :> Get '[JSON] (ExDisplayed Sync)
      :<|> "courses" :> Get '[JSON] (ExDisplayed Courses)
      :<|> "peek" :> Capture "path" FilePath :> Get '[JSON] P.PeekResponse

server :: Server API
server = liftIO sync :<|> liftIO courses :<|> (liftIO . P.peekAt)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy


courses :: IO (ExDisplayed Courses)
courses = do
    putStrLn "endpoint: \"courses\""
    leftToString <$> runGlobal thisTermCourse

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

joinEither :: Either e (Either e a) -> Either e a
joinEither (Left ex)           = Left ex
joinEither (Right (Left ex))   = Left ex
joinEither (Right (Right val)) = Right val
