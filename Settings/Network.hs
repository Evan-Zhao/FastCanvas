{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Settings.Network  where

import           Control.Monad                       ((>=>))
import           Data.Aeson
import           Data.Aeson.Types                    ()
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8          as L
import           Data.List                           (isInfixOf)
import           Network.HTTP.Simple

import           Settings.Endpoint.Paginate
import           Settings.Exception.GeneralException
import           Settings.Monad.Exception
import           Settings.Monad.Reader
import           Settings.Token.Token

type MonadRIOE e m = (MonadEnvReader m, MonadIOE e m)
type MonadRIOE' m = (MonadEnvReader m, MonadIOE' m)

isAbsolute :: String -> Bool
isAbsolute = isInfixOf "://"

prependHost :: MonadRIOE e m => String -> m String
prependHost path = if isAbsolute path then return path
                   else flip (++) path <$> reader getHost

parseRequestAddToken, parseRequestNoToken :: MonadRIOE e m => String -> m Request
parseRequestAddToken url = parseRequestNoToken url >>= addToken
parseRequestNoToken url  = prependHost url >>= (liftIO . parseRequest)

-- canvasJSON :: (FromJSON a, MonadRIOE e m) => String -> m a
-- canvasJSON path = do
--     req <- canvasParseRelDir path
--     catchIOE $ getResponseBody <$> httpJSON req

-- canvasJSON' :: (FromJSON a, MonadIOE e m) => String -> m a
-- canvasJSON' url = do
--     req <- liftIO $ parseRequest url
--     req' <- addToken req
--     catchIOE $ getResponseBody <$> httpJSON req'

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
