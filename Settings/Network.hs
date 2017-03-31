{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Settings.Network  where

import           Control.Monad                       ((>=>))
import           Data.Aeson
import           Data.Aeson.Types                    ()
import qualified Data.ByteString.Char8               as S
import qualified Data.ByteString.Lazy.Char8          as L
import           Data.List                           (isInfixOf)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header           (hAuthorization)

import           Settings.Endpoint.Paginate
import           Settings.Exception.GeneralException
import           Settings.Monad.Exception
import           Settings.Monad.Reader

type RIO m = (MonadEnvReader m, MonadIO m)

type RIOE e m = (MonadEnvReader m, IOE e m)
type RIOE' m = (MonadEnvReader m, IOE SomeException m)

canvasJSON, canvasJSON' :: (FromJSON a, RIOE' m) => String -> m [a]
canvasJSON  = parseRequestAddToken >=> canvasJSONGo
canvasJSON' = parseRequestNoToken >=> canvasJSONGo

canvasJSONGo :: (FromJSON a, RIOE' m) => Request -> m [a]
canvasJSONGo req = do
    resp <- catchIOE $ httpLBS req
    jsons <- maybeToEWith jsonE $ decode $ getResponseBody resp
    nextJSONhref <- nnext <$> getNaviLinks resp
    (jsons ++) <$> maybe (return []) (parseRequestAddToken >=> canvasJSONGo) nextJSONhref
  where
    jsonE = fromString "JSON format error."

canvasLBS, canvasLBS' :: RIOE e m => String -> m L.ByteString
canvasLBS url  = do
    req <- parseRequestAddToken url
    catchIOE $ getResponseBody <$> httpLBS req
canvasLBS' url = do
    req <- parseRequestNoToken url
    catchIOE $ getResponseBody <$> httpLBS req

addTokenTo :: String -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (S.pack $ "Bearer " ++ tokenBS)

addToken :: RIO m => Request -> m Request
addToken req = addTokenTo <$> asks getUserToken <*> return req

isAbsolute :: String -> Bool
isAbsolute = isInfixOf "://"

prependHost :: RIO m => String -> m String
prependHost path = if isAbsolute path then return path
                   else flip (++) path <$> asks getHost

parseRequestAddToken, parseRequestNoToken :: RIOE e m => String -> m Request
parseRequestAddToken url = parseRequestNoToken url >>= addToken
parseRequestNoToken url  = prependHost url >>= (liftIO . parseRequest)
