module Settings.Settings (
    module Except,
    canvasParseRequest,
    simpleHttpJSON,
    ByteString
) where

import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Except
import           Network.HTTP.Simple
import           Settings.Token.Token

host :: String
host = "https://canvas.instructure.com/api/v1/"

canvasParseRequest :: String -> MainExcept Request
canvasParseRequest dir = do
    request <- lift $ parseRequest $ host ++ dir
    addToken request

simpleHttpJSON :: (FromJSON a) => String -> MainExcept a
simpleHttpJSON str = do
    req <- canvasParseRequest str
    catchMainE $ getResponseBody <$> httpJSON req

simpleHttpLBS :: String -> MainExcept ByteString
simpleHttpLBS str = do
    req <- canvasParseRequest str
    catchMainE $ getResponseBody <$> httpLBS req
