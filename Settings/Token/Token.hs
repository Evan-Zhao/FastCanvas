{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Token.Token where

import           Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as L
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  (hAuthorization)

import           Settings.Monad.Exception

tokenFilePath :: FilePath
tokenFilePath = "Assets/TokenRaw/Token.txt"

tokenE :: MonadIOE e m => m L.ByteString
tokenE = catchIOE $ L.readFile tokenFilePath

addTokenTo :: L.ByteString -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (L.toStrict $ "Bearer " `L.append` tokenBS)

addToken :: MonadIOE e m => Request -> m Request
addToken req = addTokenTo <$> tokenE <*> return req
