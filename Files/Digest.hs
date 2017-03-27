{-# LANGUAGE OverloadedStrings #-}

module Files.Digest where

import           Control.Monad            ((>=>))
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Foldable
import qualified Data.Map.Strict          as M
import           Data.Monoid              ((<>))
import           Data.Tree
import           System.Directory
import           System.FilePath          ((</>))

import           Files.Tree
import           Settings.Monad.Exception

newtype FSDigestTree = FSDigestTree { unFSDigestTree :: Tree (FilePath, String) }
    deriving (Eq, Show)

instance ToJSON FSDigestTree where
    toJSON (FSDigestTree (Node (path, sha) nodes)) =
        object ["filename" .= path, "sha1_digest" .= sha,
                "childrens" .= map (toJSON . FSDigestTree) nodes]

    toEncoding (FSDigestTree (Node (path, sha) nodes)) =
        pairs ("filename" .= path <> "sha1_digest" .= sha <>
               "childrens" .= map (toJSON . FSDigestTree) nodes)

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

zipFilePathWithSha1 :: MonadIO m => Tree FilePath -> m FSDigestTree
zipFilePathWithSha1 = fmap FSDigestTree
                    . liftIO . traverseTreeFoldPar (</>) mapF ""
  where
    mapF parent this = let full = parent </> this in (,) full <$> liftIO (getSha1 full)

generateDigestOutput :: FilePath -> IO Lazy.ByteString
generateDigestOutput = fmap encode . (unfoldDirectoryTree >=> zipFilePathWithSha1)

generateDigestDict :: FilePath -> IO (M.Map FilePath String)
generateDigestDict path = do
    fsTree <- unfoldDirectoryTree path
    fsDigestTree <- zipFilePathWithSha1 fsTree
    return $ M.fromList $ toList $ unFSDigestTree fsDigestTree
