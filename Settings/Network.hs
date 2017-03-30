{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Settings.Network  where

import           Control.Monad                       ((>=>))
import           Data.Aeson
import           Data.Aeson.Types                    ()
import           Data.Bifunctor
import qualified Data.ByteString.Char8               as S
import qualified Data.ByteString.Lazy.Char8          as L
import           Data.List                           (isInfixOf)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header           (hAuthorization)

import           Settings.Endpoint.Paginate
import           Settings.Exception.GeneralException
import           Settings.Monad.Exception
import           Settings.Monad.Reader

type MonadRIOE e m = (MonadEnvReader m, MonadIOE e m)
type MonadRIOE' m = (MonadEnvReader m, MonadIOE SomeException m)

canvasJSON, canvasJSON' :: (FromJSON a, MonadRIOE' m) => String -> m [a]
canvasJSON  = parseRequestAddToken >=> canvasJSONGo
canvasJSON' = parseRequestNoToken >=> canvasJSONGo

canvasJSONGo :: (FromJSON a, MonadRIOE' m) => Request -> m [a]
canvasJSONGo req = do
    resp <- catchIOE $ httpLBS req
    jsons <- maybeToEWith jsonE $ decode $ getResponseBody resp
    nextJSONhref <- nnext <$> getNaviLinks resp
    (jsons ++) <$> maybe (return []) (parseRequestAddToken >=> canvasJSONGo) nextJSONhref
  where
    jsonE = fromString "JSON format error."

canvasLBS, canvasLBS' :: MonadRIOE e m => String -> m L.ByteString
canvasLBS url  = do
    req <- parseRequestAddToken url
    catchIOE $ getResponseBody <$> httpLBS req
canvasLBS' url = do
    req <- parseRequestNoToken url
    catchIOE $ getResponseBody <$> httpLBS req

addTokenTo :: String -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (S.pack $ "Bearer " ++ tokenBS)

addToken :: MonadRIOE e m => Request -> m Request
addToken req = addTokenTo <$> asks getUserToken <*> return req

isAbsolute :: String -> Bool
isAbsolute = isInfixOf "://"

prependHost :: MonadRIOE e m => String -> m String
prependHost path = if isAbsolute path then return path
                   else flip (++) path <$> reader getHost

parseRequestAddToken, parseRequestNoToken :: MonadRIOE e m => String -> m Request
parseRequestAddToken url = parseRequestNoToken url >>= addToken
parseRequestNoToken url  = prependHost url >>= (liftIO . parseRequest)
