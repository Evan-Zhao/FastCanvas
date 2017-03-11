{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Settings.Network (
    canvasParseRequest,
    simpleHttpJSON,
    simpleHttpJSON',
    simpleHttpLBS',
    MonadRIOE
) where

import           Data.Aeson
import           Data.Aeson.Types           ()
import qualified Data.ByteString.Lazy.Char8 as L
import           Network.HTTP.Simple

import           Settings.Monad.Exception
import           Settings.Monad.Reader
import           Settings.Token.Token

type MonadRIOE e m = (MonadEnvReader m, MonadIOE e m)

canvasParseRequest :: MonadRIOE e m => String -> m Request
canvasParseRequest dir = do
    host <- reader getHost
    request <- liftIO $ parseRequest $ host ++ dir
    addToken request

simpleHttpJSON :: (FromJSON a, MonadRIOE e m) => String -> m a
simpleHttpJSON str = do
    req <- canvasParseRequest str
    catchIOE $ getResponseBody <$> httpJSON req

simpleHttpLBS :: MonadRIOE e m => String -> m L.ByteString
simpleHttpLBS str = do
    req <- canvasParseRequest str
    catchIOE $ getResponseBody <$> httpLBS req

simpleHttpJSON' :: (FromJSON a, MonadIOE e m) => String -> m a
simpleHttpJSON' str = do
    req <- liftIO $ parseRequest str
    req' <- addToken req
    catchIOE $ getResponseBody <$> httpJSON req'

simpleHttpLBS' :: MonadIOE e m => String -> m L.ByteString
simpleHttpLBS' str = do
    req <- liftIO $ parseRequest str
    req' <- addToken req
    catchIOE $ getResponseBody <$> httpLBS req
