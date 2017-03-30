{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Communication.Send where

import           Control.Monad              (mapM_, unless, void)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Except (runExceptT)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Communication.Format
import           Course.File
import qualified Course.List                as CL
import           Data.Aeson
import           Files.State
import           Settings.Monad.Global

type API = "sync" :> Get '[JSON] ApiResponse

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = do
    envR <- liftIO getEnvR
    envS <- liftIO getEnvS
    result <- liftIO $ resultTup envR envS
    return $ ApiResponse result

main' :: IO ()
main' = do
    envR <- liftIO getEnvR
    envS <- liftIO getEnvS
    result <- liftIO $ resultTup envR envS
    print $ toJSON $ ApiResponse result

resultTup :: EnvR -> EnvS -> IO (Either SomeException [(CL.Course, DownloadState)])
resultTup envR envS = (\(a,_,_) -> a) <$> runRWST (runExceptT mainG) envR envS

mainG :: Global [(CL.Course, DownloadState)]
mainG = do
    liftIO $ putStrLn "Fetching course list...\n"
    this <- CL.thisTermCourse
    defPath <- getDefaultPath <$> lift get
    liftIO $ putStrLn $ "Will download to " ++ defPath ++ "\n"
    states <- mapM (getDownloadResult defPath) this
    return $ zip this states

getDownloadResult :: FilePath -> CL.Course -> Global DownloadState
getDownloadResult folder course = do
    liftIO $ putStrLn $ "\nNow downloading for course " ++ courseName
    liftIO $ putStrLn "Counting files..."
    x <- getRootFromCourse id'
    maybe emptyCourseResponse (nonemptyCourse courseName folder) x
  where
    id' = CL.id course
    courseName = CL.courseShortName course
