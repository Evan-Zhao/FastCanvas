{-# LANGUAGE DeriveDataTypeable #-}

module Settings.Exception.ResponseCodeException where

import           Control.Exception
import           Data.Typeable
import           Network.HTTP.Simple

data ResponseCodeException =
    ResponseCodeException {
        eRequest      :: Request,
        eResponseCode :: Int
    } deriving (Show, Typeable)

instance Exception ResponseCodeException
