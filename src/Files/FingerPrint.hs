{-# LANGUAGE OverloadedStrings #-}

module Files.FingerPrint (
    FSDigestTree,
    zipFilePathWithSha1,
    getSha1
) where

import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Lazy   as Lazy
import           Data.Monoid            ((<>))
import           Data.Tree
import           System.Directory
import           System.FilePath        ((</>))

import           Files.Tree

newtype FSDigestTree = FSDigestTree { unFSDigestTree :: Tree (FilePath, String) }
    deriving (Eq, Show)

instance ToJSON FSDigestTree where
    toJSON (FSDigestTree (Node (path, sha) nodes)) =
        object ["filename" .= path, "sha1_digest" .= sha,
                "childrens" .= map (toJSON . FSDigestTree) nodes]

    toEncoding (FSDigestTree (Node (path, sha) nodes)) =
        pairs ("filename" .= path <> "sha1_digest" .= sha <>
               "childrens" .= map (toJSON . FSDigestTree) nodes)

genFingerPrintTree :: MonadIO m => FilePath -> m FSDigestTree
genFingerPrintTree = unfoldDirectoryTree >=> zipFilePathWithSha1

zipFilePathWithSha1 :: MonadIO m => Tree FilePath -> m FSDigestTree
zipFilePathWithSha1 = fmap FSDigestTree
                    . liftIO . traverseTreeFoldPar (</>) mapF ""
  where
    mapF parent this = (,) this <$> getSha1 (parent </> this)

getSha1 :: FilePath -> IO String
getSha1 path = do
    isFile <- doesFileExist path
    if isFile
        then do
            file <- Lazy.readFile path
            return $ show $ hashLazyAlgo file
        else return ""
  where
    hashLazyAlgo :: Lazy.ByteString -> Digest SHA1
    hashLazyAlgo = hashlazy

unfoldDirectoryTree :: MonadIO m => FilePath -> m (Tree FilePath)
unfoldDirectoryTree = unfoldTreeM $ \path -> do
    isDir <- liftIO $ doesDirectoryExist path
    subDirs <- liftIO $ if isDir then listDirectory path else return []
    return (path, subDirs)
