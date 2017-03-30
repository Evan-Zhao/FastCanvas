{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Files.Structure (
    unfoldFileTree,
    downloadTree,
    renameRoot,
    isSingleNode
) where

import           Control.Monad              (mapM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Foldable
import           Data.Tree
import           System.FilePath            ((</>))

import           Files.Node.FSNode
import           Files.Node.NodeJSON
import           Files.State
import           Files.Tree
import           Settings.Monad.Exception
import           Settings.Network
import           TentativePush

type TreeSeed = Either FileJSON FolderJSON

unfoldFileTree :: MonadFull' m => TreeSeed -> m (Tree FSNode)
unfoldFileTree = unfoldTreeM genTreeSeeds

renameRoot :: String -> Tree FSNode -> Tree FSNode
renameRoot newName (Node root xs) = Node (root { relativePath = newName }) xs

downloadTree :: MonadIO m => FilePath -> Tree FSNode -> m DownloadState
downloadTree parent rootNode = fold <$> downloadTreeUncounted parent rootNode

downloadTreeUncounted :: MonadIO m => FilePath -> Tree FSNode -> m (Tree DownloadState)
downloadTreeUncounted fp tree =
    liftIO $ traverseTreeFoldPar combinator writeAndCount fp tree
  where
    combinator l r = l </> relativePath r

writeAndCount :: MonadIO m => FilePath -> FSNode -> m DownloadState
writeAndCount parent node = do
    exist <- doesExist parent node
    if exist then return singleExState else do
        liftIO $ pipePush $ "Writing on path " ++ path ++ "..."
        result <- liftIO $ runExceptT downloadExceptT
        return $ either singleFaStateWith (const singleSuState) result
  where
    downloadExceptT :: ExceptT SomeException IO ()
    downloadExceptT = writeNode parent node
    path = parent </> relativePath node

genTreeSeeds :: MonadFull' m => TreeSeed -> m (FSNode, [TreeSeed])
genTreeSeeds (Left filej) = return (filejsonToNode filej, [])
genTreeSeeds (Right folderj) = do
    filejsons <- canvasJSON $ files_url folderj
    folderjsons <- canvasJSON $ folders_url folderj
    let seeds = map Left (filejsons :: [FileJSON])
             ++ map Right (folderjsons :: [FolderJSON])
    return (FolderNode $ name folderj, seeds)
