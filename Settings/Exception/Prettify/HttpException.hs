{-# LANGUAGE QuasiQuotes #-}

module Settings.Exception.Prettify.HttpException (
    pHttpException
) where

import           Control.Exception
import           Data.Array
import           Network.HTTP.Simple
import           Text.RE.PCRE.String

pHttpException :: SomeException -> Maybe String
pHttpException ex = extractor <$> fromException ex

extractor :: HttpException -> String
extractor (HttpExceptionRequest req content) =
    show content ++ " while requesting " ++ getReqStringHost (show req)
extractor iuEx@(InvalidUrlException _ _) = show iuEx

getReqStringHost :: String -> String
getReqStringHost reqStr = host ++ path where
    host = takeCaptureOrdn 1 $ reqStr ?=~ [re|host *= *"(.+)"|]
    path = takeCaptureOrdn 1 $ reqStr ?=~ [re|path *= *"(.+)"|]
    takeCaptureOrdn n = capturedText . (! n) . matchArray
