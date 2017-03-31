module Settings.Exception.Prettify.JSONException (
    pJSONException
) where

import           Control.Exception
import           Network.HTTP.Simple

import           Settings.Exception.Prettify.HttpException (getReqStringHost)

pJSONException :: SomeException -> Maybe String
pJSONException ex = extractor <$> fromException ex

extractor :: JSONException -> String
extractor (JSONParseException req _ _) =
    "Server responded with ill-formatted JSON when we request \"" ++
    getReqStringHost (show req) ++ "\"."
extractor (JSONConversionException req _ _) =
    "Server responded with JSON of unexpected format when we request \"" ++
    getReqStringHost (show req) ++ "\"."
