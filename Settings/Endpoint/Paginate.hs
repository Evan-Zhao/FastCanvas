{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Settings.Endpoint.Paginate (
    NaviLinks (..),
    getNaviLinks
) where

import           Control.Exception                   hiding (try)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.ByteString.Lazy.Char8          (ByteString)
import           Data.List                           (lookup)
import           Network.HTTP.Simple
import           Text.Parsec

import           Settings.Exception.GeneralException
import           Settings.Monad.Exception

data NaviLinks = NaviLinks {
    ncurrent :: String,
    nprev    :: Maybe String, -- not if on first page
    nnext    :: Maybe String, -- not if on last page
    nfirst   :: String,
    nlast    :: Maybe String -- not if too expensive to get total pages
}

linkHeaderP = sepBy singleEntryP (char ',') where
    singleEntryP = do
        link <- try linkP
        char ';' *> spaces
        key <- string "rel=" *> keyP
        return (key, link)
    linkP = between (char '<') (char '>') $ many $ satisfy (/='>')
    keyP  = between (char '"') (char '"') $ many $ satisfy (/='"')

getNaviLinks :: MonadError SomeException m => Response a -> m NaviLinks
getNaviLinks resp = do
    let maybeRawLinks = lookup "Link" $ getResponseHeaders resp
    rawLinks <- maybeToEWith linkHeaderMissing maybeRawLinks
    let eitherPairedLinks = parse linkHeaderP "" rawLinks
    pairedLinks <- eitherToE $ first (const formatError) eitherPairedLinks
    let [current, prev, next, first, last] =
            map (`lookup` pairedLinks) ["current", "prev", "next", "first", "last"]
    current' <- maybeToEWith (keyMissing "current") current
    first' <- maybeToEWith (keyMissing "first") first
    return NaviLinks {
        ncurrent = current',
        nprev = prev, nnext = next,
        nfirst = first', nlast = last
    }
  where
    keyMissing name = fromString $
        "API fault while paginating; missing \"" ++ name ++ "\" link."
    formatError = fromString "API fault while paginating; header format incorrect."
    linkHeaderMissing = fromString "API fault while paginating; header \"link\" not found."
