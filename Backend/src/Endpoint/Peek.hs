{-# LANGUAGE DeriveGeneric #-}

module Endpoint.Peek (
    peekAt,
    PeekResponse
) where

import           Control.Exception
import           Control.Monad              (mapM)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (catMaybes)
import           GHC.Generics
import           System.Directory
import           System.FilePath

import           Files.Local.Entry

data PeekRequest = PRequest {
    peekPath :: String
} deriving (Eq, Show, Generic)
instance FromJSON PeekRequest

data PeekResponse = PResponse {
    pathSent    :: FilePath,
    pathSplit   :: [FilePath],
    dirContents :: [FSEntry]
} deriving (Eq, Show, Generic)
instance ToJSON PeekResponse

peekAt :: FilePath -> IO PeekResponse
peekAt path = PResponse path (splitPath path) <$> peekDircontents path

peekDircontents :: FilePath -> IO [FSEntry]
peekDircontents path = either (const []) id <$> eitherResult where
    eitherResult :: IO (Either SomeException [FSEntry])
    eitherResult = try $ fmap catMaybes $
        listDirectory path >>= mapM (fillFSEntry . (path </>))
