{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Monad.State where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (unless)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Binary.Builder        (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Generics
import           System.Directory           (createDirectoryIfMissing,
                                             doesDirectoryExist)
data EnvS = EnvS {
    getUserToken   :: String,
    getDefaultPath :: String
} deriving (Show, Generic)

instance FromJSON EnvS where
    parseJSON (Object obj) = EnvS
                         <$> obj .: "token"
                         <*> obj .: "download_path"
    parseJSON invalid = typeMismatch "EnvS" invalid

instance ToJSON EnvS where
    toJSON envS =
        object [ "token" .= getUserToken envS
               , "download_path" .= getDefaultPath envS
               ]
    toEncoding = genericToEncoding defaultOptions

getEnvS :: IO EnvS
getEnvS = do
    maybeEnvS <- runMaybeT tryReadEnvS
    maybe queryEnvS return maybeEnvS

tryReadEnvS :: MaybeT IO EnvS
tryReadEnvS = do
    file <- maybeTry $ L.readFile configPath
    MaybeT $ return $ decode file
    where
        maybeTry :: IO a -> MaybeT IO a
        maybeTry = MaybeT . fmap (either (const Nothing) Just) . try'
        try' :: IO a -> IO (Either SomeException a)
        try' = try

configPath :: String
configPath = "Assets/Config.json"

queryEnvS :: IO EnvS
queryEnvS = do
    putStrLn "Settings file does not exist or is corrupted; please set your preference as follows:"
    userToken <- promptUserToken
    defaultPath <- promptDefaultPath
    let envS = EnvS { getUserToken   = userToken,
                      getDefaultPath = defaultPath }
    let configString = toLazyByteString $ fromEncoding $ toEncoding envS
    L.writeFile configPath configString
    return envS

promptUserToken :: IO String
promptUserToken = putStrLn "\nPlease input your token from Canvas:" >> promptUserToken'
  where
    tokenStandardLen = 69
    promptUserToken' = do
        token <- getLine
        if length token == tokenStandardLen
            then return token
            else putStrLn "Token has incorrect length; please double check." >> promptUserToken'

promptDefaultPath :: IO String
promptDefaultPath = putStrLn "\nYour favorite path for containing files from Canvas:"
                 >> promptDefaultPath'
  where
    promptDefaultPath' = do
        path <- getLine
        doesExist <- doesDirectoryExist path
        unless doesExist $ do
            putStrLn "Given path does not exist... creating"
            createDirectoryIfMissing True path
        return path
