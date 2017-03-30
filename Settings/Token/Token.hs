{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Token.Token where

import           Control.Monad.Except
import qualified Data.ByteString.Char8     as L
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header (hAuthorization)

import           Settings.Monad.Exception
import           Settings.Monad.State

type MonadSIOE e m = (MonadEnvState m, MonadIOE e m)

addTokenTo :: String -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (L.pack $ "Bearer " ++ tokenBS)

addToken :: MonadSIOE e m => Request -> m Request
addToken req = addTokenTo <$> gets getUserToken <*> return req
