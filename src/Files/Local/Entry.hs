{-# LANGUAGE DeriveGeneric #-}

module Files.Local.Entry (
    fillFSEntry,
    FSEntry (..)
) where

import           Control.Exception (SomeException, try)
import           Data.Aeson
import           Data.Time         (UTCTime (..))
import           GHC.Generics
import           System.Directory
import           System.FilePath

data EntryType = Directory | File deriving (Eq, Show, Generic)
instance ToJSON EntryType

data FSEntry = FSEntry {
    shortName :: FilePath,
    fullName  :: FilePath,
    entryType :: EntryType,
    size      :: Integer,
    modTime   :: UTCTime
} deriving (Eq, Show, Generic)
instance ToJSON FSEntry

fillFSEntry :: FilePath -> IO (Maybe FSEntry)
fillFSEntry path = do
    exist <- doesPathExist path
    if exist then eitherToMaybe <$> try' (fillFSEntry' path) else return Nothing
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try
    eitherToMaybe = either (const Nothing) Just
    fillFSEntry' path = FSEntry
                    <$> return (takeFileName path)
                    <*> canonicalizePath path
                    <*> getEntryType path
                    <*> getFileSize path
                    <*> getModificationTime path

getEntryType :: FilePath -> IO EntryType
getEntryType path = (\b -> if b then File else Directory) <$> doesFileExist path
