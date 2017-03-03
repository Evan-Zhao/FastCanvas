{-# LANGUAGE OverloadedStrings #-}

module Settings.Token.Token where

import qualified Data.ByteString.Lazy.Char8 as L
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  (hAuthorization)
import           Settings.Global

tokenFilePath :: FilePath
tokenFilePath = "Assets/TokenRaw/Token.txt"

tokenE :: Global L.ByteString
tokenE = catchIOE $ L.readFile tokenFilePath

addTokenTo :: L.ByteString -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (L.toStrict $ "Bearer " `L.append` tokenBS)

addToken :: Request -> Global Request
addToken req = addTokenTo <$> tokenE <*> return req
