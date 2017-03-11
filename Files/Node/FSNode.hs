{-# LANGUAGE MultiParamTypeClasses #-}

module Files.Node.FSNode (
    module Files.Node.FSNode,
    MonadDownload,
    MonadIO
) where

import           Control.Monad.IO.Class     (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import           System.Directory
import           System.FilePath            ((</>))

import           Files.Node.Download
import           Settings.Monad.Exception

type File = L.ByteString

data FSNode = FolderNode {
                relativePath  :: String
              } |
              FileNode   {
                relativePath :: String,
                nodeUrl      :: String
              } deriving (Eq, Show)

writeNode :: MonadDownload e m => FilePath -> FSNode -> m ()
writeNode parent (FolderNode rel)   = liftIO $ createDirectoryIfMissing False $ parent </> rel
writeNode parent (FileNode rel url) = downloadTo url (parent </> rel)

doesExist :: MonadIO m => FilePath -> FSNode -> m Bool
doesExist path node = liftIO $ doesPathExist $ path </> relativePath node
