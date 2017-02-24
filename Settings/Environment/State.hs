{-# LANGUAGE DeriveGeneric #-}

module Settings.Environment.State where

import           Data.Aeson
import           GHC.Generics

data EnvS = EnvS deriving (Show, Generic)

instance FromJSON EnvS

instance ToJSON EnvS

getEnvS :: IO (Either String EnvS)
getEnvS = return $ Right EnvS
