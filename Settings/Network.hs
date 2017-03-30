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
import           Settings.Monad.State

type MonadRIOE e m = (MonadEnvReader m, MonadIOE e m)
type MonadRIOE' m = (MonadEnvReader m, MonadIOE SomeException m)

type MonadSIOE e m = (MonadEnvState m, MonadIOE e m)

type MonadFull e m = (MonadSIOE e m, MonadRIOE e m)
type MonadFull' m = MonadFull SomeException m

canvasJSON, canvasJSON' :: (FromJSON a, MonadFull SomeException m) => String -> m [a]
canvasJSON  = parseRequestAddToken >=> canvasJSONGo
canvasJSON' = parseRequestNoToken >=> canvasJSONGo

canvasJSONGo :: (FromJSON a, MonadFull SomeException m) => Request -> m [a]
canvasJSONGo req = do
    resp <- catchIOE $ httpLBS req
    jsons <- maybeToEWith jsonE $ decode $ getResponseBody resp
    nextJSONhref <- nnext <$> getNaviLinks resp
    (jsons ++) <$> maybe (return []) (parseRequestAddToken >=> canvasJSONGo) nextJSONhref
  where
    jsonE = fromString "JSON format error."

canvasLBS, canvasLBS' :: MonadFull e m => String -> m L.ByteString
canvasLBS url  = do
    req <- parseRequestAddToken url
    catchIOE $ getResponseBody <$> httpLBS req
canvasLBS' url = do
    req <- parseRequestNoToken url
    catchIOE $ getResponseBody <$> httpLBS req

addTokenTo :: String -> Request -> Request
addTokenTo tokenBS = addRequestHeader hAuthorization (S.pack $ "Bearer " ++ tokenBS)

addToken :: MonadSIOE e m => Request -> m Request
addToken req = addTokenTo <$> gets getUserToken <*> return req

isAbsolute :: String -> Bool
isAbsolute = isInfixOf "://"

prependHost :: MonadRIOE e m => String -> m String
prependHost path = if isAbsolute path then return path
                   else flip (++) path <$> reader getHost

parseRequestAddToken, parseRequestNoToken :: MonadFull e m => String -> m Request
parseRequestAddToken url = parseRequestNoToken url >>= addToken
parseRequestNoToken url  = prependHost url >>= (liftIO . parseRequest)
