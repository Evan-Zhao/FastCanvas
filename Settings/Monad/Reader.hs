{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.Reader (
    module Settings.Monad.Reader,
    module Control.Monad.Reader
) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8          as L
import           GHC.Generics

import           Settings.Exception.GeneralException
import           Settings.Monad.Exception

data EnvR = EnvR {
    getHost        :: String,
    getConfigPath  :: String,
    getUserToken   :: String,
    getDefaultPath :: String
} deriving (Show, Generic)

data EnvRFiled = EnvRFiled {
    getUserToken'   :: String,
    getDefaultPath' :: String
} deriving (Show, Generic)

instance FromJSON EnvRFiled where
    parseJSON (Object obj) = EnvRFiled
                         <$> obj .: "token"
                         <*> obj .: "download_path"
    parseJSON invalid = typeMismatch "EnvRFiled" invalid

instance ToJSON EnvRFiled where
    toJSON envRF =
        object [ "token" .= getUserToken' envRF
               , "download_path" .= getDefaultPath' envRF
               ]
    toEncoding = genericToEncoding defaultOptions

merge :: EnvRFiled -> EnvR
merge f = defaultEnvR {
    getUserToken = getUserToken' f,
    getDefaultPath = getDefaultPath' f
}

tryGetEnvRFiled :: FilePath -> IO (Maybe EnvRFiled)
tryGetEnvRFiled path = do
    maybeEnvS <- runExceptT $ tryReadEnvRFiled path
    return $ either (const Nothing) Just maybeEnvS

tryReadEnvRFiled :: IOE SomeException m => FilePath -> m EnvRFiled
tryReadEnvRFiled path = do
    file <- catchIOE $ L.readFile path
    eitherToE $ first fromString $ eitherDecode file

tryGetEnvR :: IO (Maybe EnvR)
tryGetEnvR = do
    let filePath = getConfigPath defaultEnvR
    maybeEnvRF <- tryGetEnvRFiled filePath
    return $ merge <$> maybeEnvRF

defaultEnvR :: EnvR
defaultEnvR = EnvR {
    getHost        = "https://canvas.instructure.com/api/v1/",
    getConfigPath  = "Assets/Config.json",
    getUserToken   = "",
    getDefaultPath = ""
}

type MonadEnvReader m = MonadReader EnvR m
