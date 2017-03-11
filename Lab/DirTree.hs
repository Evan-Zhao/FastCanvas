{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson
import qualified Data.ByteString.Lazy   as Lazy
import           Data.Monoid            ((<>))
import           Data.Tree
import           System.Directory
import           System.FilePath        ((</>))

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
                    . traverseTreeFold (</>) mapF ""
  where
    mapF parent this = (,) this <$> liftIO (getSha1 $ parent </> this)

traverseTreeMonoid :: (Monad m, Monoid a) => (a -> a -> m b) -> Tree a -> m (Tree b)
traverseTreeMonoid = traverseTreeMonoid' mempty where
    traverseTreeMonoid' acc f (Node n ch) = do
        nodeNew <- f acc n
        childrensNew <- mapM (traverseTreeMonoid' (acc `mappend` n) f) ch
        return $ Node nodeNew childrensNew

traverseTreeFold :: (Monad m, Monoid a)
                 => (a -> a -> a) -> (a -> a -> m b)
                 -> a -> Tree a -> m (Tree b)
traverseTreeFold accF mapF acc (Node n ch) = do
    nodeNew <- mapF acc n
    childrensNew <- mapM (traverseTreeFold accF mapF (acc `accF` n)) ch
    return $ Node nodeNew childrensNew

newtype FSDigestTree = FSDigestTree { unFSDigestTree :: Tree (FilePath, String) }
    deriving (Eq, Show)

instance ToJSON FSDigestTree where
    toJSON (FSDigestTree (Node (path, sha) nodes)) =
        object ["filename" .= path, "sha1_digest" .= sha,
                "childrens" .= map (toJSON . FSDigestTree) nodes]

    toEncoding (FSDigestTree (Node (path, sha) nodes)) =
        pairs ("filename" .= path <> "sha1_digest" .= sha <>
               "childrens" .= map (toJSON . FSDigestTree) nodes)

main = do
    path <- getLine
    fsTree <- unfoldDirectoryTree path
    fsDigestTree <- zipFilePathWithSha1 fsTree
    Lazy.writeFile "dig.json" $ encode fsDigestTree
