{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Environment.Reader where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as Bs
import           GHC.Generics

data EnvR = EnvR {
    getHost      :: String,
    getUserToken :: String
} deriving (Show, Generic)

instance FromJSON EnvR where
    parseJSON (Object obj) = EnvR
                         <$> obj .: "host"
                         <*> obj .: "token"
    parseJSON invalid = typeMismatch "EnvR" invalid

instance ToJSON EnvR

configPath :: String
configPath = "Assets/Config.json"

getEnvR :: IO (Either String EnvR)
getEnvR = eitherDecode <$> Bs.readFile configPath
