module Settings.Network (
    canvasParseRequest,
    simpleHttpJSON,
    simpleHttpJSON',
    simpleHttpLBS'
) where

import           Data.Aeson
import           Data.Aeson.Types           ()
import           Data.ByteString.Lazy.Char8
import           Network.HTTP.Simple
import           Settings.Global
import           Settings.Token.Token

canvasParseRequest :: String -> Global Request
canvasParseRequest dir = do
    host <- lift $ getHost <$> ask
    request <- liftIO $ parseRequest $ host ++ dir
    addToken request

simpleHttpJSON :: (FromJSON a) => String -> Global a
simpleHttpJSON str = do
    req <- canvasParseRequest str
    catchIOE $ getResponseBody <$> httpJSON req

simpleHttpLBS :: String -> Global ByteString
simpleHttpLBS str = do
    req <- canvasParseRequest str
    catchIOE $ getResponseBody <$> httpLBS req

simpleHttpJSON' :: (FromJSON a) => String -> Global a
simpleHttpJSON' str = do
    req <- liftIO $ parseRequest str
    req' <- addToken req
    catchIOE $ getResponseBody <$> httpJSON req'

simpleHttpLBS' :: String -> Global ByteString
simpleHttpLBS' str = do
    req <- liftIO $ parseRequest str
    req' <- addToken req
    catchIOE $ getResponseBody <$> httpLBS req
