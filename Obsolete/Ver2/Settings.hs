module Settings.Settings (
    module Settings.Global,
    canvasParseRequest,
    simpleHttpJSON,
    simpleHttpJSON',
    ByteString,
    simpleHttpLBS'
) where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Network.HTTP.Simple
import           Settings.Global
import           Settings.Token.Token

host :: String
host = "https://canvas.instructure.com/api/v1/"

canvasParseRequest :: String -> Global Request
canvasParseRequest dir = do
    request <- lift $ parseRequest $ host ++ dir
    addToken request

simpleHttpJSON :: (FromJSON a) => String -> Global a
simpleHttpJSON str = do
    req <- canvasParseRequest str
    catchE' $ getResponseBody <$> httpJSON req

simpleHttpLBS :: String -> Global ByteString
simpleHttpLBS str = do
    req <- canvasParseRequest str
    catchE' $ getResponseBody <$> httpLBS req

simpleHttpJSON' :: (FromJSON a) => String -> Global a
simpleHttpJSON' str = do
    req <- lift $ parseRequest str
    req' <- addToken req
    catchE' $ getResponseBody <$> httpJSON req'

simpleHttpLBS' :: String -> Global ByteString
simpleHttpLBS' str = do
    req <- lift $ parseRequest str
    req' <- addToken req
    catchE' $ getResponseBody <$> httpLBS req
