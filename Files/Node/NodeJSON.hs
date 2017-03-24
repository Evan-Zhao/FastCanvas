{-# LANGUAGE DeriveGeneric #-}

module Files.Node.NodeJSON where

import           Data.Aeson
import           GHC.Generics
import           System.FilePath   (makeValid)

import           Files.Node.FSNode

type FileID = Int
type FolderID = Int

data FileJSON = FileJSON {
    id           :: FileID,
    folder_id    :: FolderID,
    filename     :: String,
    url          :: String,
    size         :: Int,
    display_name :: String
} deriving (Eq, Show, Generic)
instance ToJSON FileJSON
instance FromJSON FileJSON

getFileName :: FileJSON -> String
getFileName = makeValid . display_name

filejsonToNode :: FileJSON -> FSNode
filejsonToNode fj = FileNode {
    relativePath = getFileName fj,
    nodeUrl = url fj
}

data FolderJSON = FolderJSON {
    name             :: String,
    parent_folder_id :: Maybe FolderID,
    files_url        :: String,
    folders_url      :: String,
    files_count      :: Int,
    folders_count    :: Int
} deriving (Eq, Show, Generic)
instance ToJSON FolderJSON
instance FromJSON FolderJSON

folderjsonToNode :: FolderJSON -> FSNode
folderjsonToNode fj = FolderNode { relativePath = name fj }
