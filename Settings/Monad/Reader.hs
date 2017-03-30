{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Monad.Reader (
    module Settings.Monad.Reader,
    module Control.Monad.Reader
) where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Generics

data EnvR = EnvR {
    getHost       :: String,
    getConfigPath :: String
} deriving (Show, Generic)

getEnvR :: IO EnvR
getEnvR = return defaultEnvR

defaultEnvR :: EnvR
defaultEnvR = EnvR {
    getHost        = "https://canvas.instructure.com/api/v1/",
    getConfigPath  = "Assets/Config.json"
}

type MonadEnvReader m = MonadReader EnvR m
