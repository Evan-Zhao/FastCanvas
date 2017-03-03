{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Environment.Reader where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Generics

data EnvR = EnvR {
    getHost        :: String
} deriving (Show, Generic)

instance FromJSON EnvR where
    parseJSON (Object obj) = EnvR
                         <$> obj .: "host"
    parseJSON invalid = typeMismatch "EnvR" invalid

instance ToJSON EnvR

getEnvR :: IO EnvR
getEnvR = return defaultEnvR

defaultEnvR :: EnvR
defaultEnvR = EnvR {
    getHost        = "https://canvas.instructure.com/api/v1/"
}
