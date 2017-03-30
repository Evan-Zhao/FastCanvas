{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Monad.State (
    EnvS (..),
    MonadEnvState,
    tryGetEnvS,
    module Control.Monad.State
) where

import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8          as L
import           GHC.Generics

import           Settings.Exception.GeneralException
import           Settings.Monad.Exception

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

tryGetEnvS :: FilePath -> IO (Maybe EnvS)
tryGetEnvS path = do
    maybeEnvS <- runExceptT $ tryReadEnvS path
    return $ either (const Nothing) Just maybeEnvS

tryReadEnvS :: MonadIOE SomeException m => FilePath -> m EnvS
tryReadEnvS path = do
    file <- catchIOE $ L.readFile path
    eitherToE $ first fromString $ eitherDecode file

type MonadEnvState m = MonadState EnvS m
