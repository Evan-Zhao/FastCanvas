{-# LANGUAGE OverloadedStrings #-}

module Settings.Token.Token where

import qualified Data.ByteString.Lazy.Char8 as Bs
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  (hAuthorization)
import           Settings.Global

tokenFilePath :: FilePath
tokenFilePath = "Assets/TokenRaw/Token.txt"

tokenE :: Global Bs.ByteString
tokenE = catchE' $ Bs.readFile tokenFilePath

addTokenTo :: Bs.ByteString -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (Bs.toStrict $ "Bearer " `Bs.append` tokenBS)

addToken :: Request -> Global Request
addToken req = addTokenTo <$> tokenE <*> return req
