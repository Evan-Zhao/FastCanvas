{-# LANGUAGE OverloadedStrings #-}

module Settings.Token.Token where

import qualified Data.ByteString.Lazy.Char8 as Bs
import           Except
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  (hAuthorization)

tokenFilePath :: FilePath
tokenFilePath = "Assets/TokenRaw/Token.txt"

tokenE :: MainExcept Bs.ByteString
tokenE = catchMainE $ Bs.readFile tokenFilePath

addTokenTo :: Bs.ByteString -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (Bs.toStrict $ "Bearer " `Bs.append` tokenBS)

addToken :: Request -> MainExcept Request
addToken req = addTokenTo <$> tokenE <*> return req
