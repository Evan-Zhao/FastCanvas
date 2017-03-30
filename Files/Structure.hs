{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Files.Structure (
    unfoldFileTree,
    downloadTree,
    renameRoot,
    isSingleNode,
    MonadSIO
) where

import           Control.Concurrent.Chan
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
import           Settings.Monad.State
import           Settings.Network
import           TentativePush

type TreeSeed = Either FileJSON FolderJSON
type MonadEnvState' m = MonadEnvState DownloadState m
type MonadSIO m = (MonadIO m, MonadEnvState' m)

unfoldFileTree :: MonadRIOE' m => TreeSeed -> m (Tree FSNode)
unfoldFileTree = unfoldTreeM genTreeSeeds

renameRoot :: String -> Tree FSNode -> Tree FSNode
renameRoot newName (Node root xs) = Node (root { relativePath = newName }) xs

downloadTree :: MonadSIO m => FilePath -> Tree FSNode -> m DownloadState
downloadTree parent rootNode = fold <$> downloadTreeUncounted parent rootNode

downloadTreeUncounted :: (MonadIO m, MonadState (EnvS DownloadState) m) => FilePath -> Tree FSNode -> m (Tree DownloadState)
downloadTreeUncounted fp tree = do
    chan <- get
    liftIO $ traverseTreeFoldPar combinator (writeAndCount chan) fp tree
  where
    combinator l r = l </> relativePath r

writeAndCount :: MonadIO m => EnvS DownloadState -> FilePath -> FSNode -> m DownloadState
writeAndCount chan parent node = do
    exist <- doesExist parent node
    if exist then return singleExState else do
        --liftIO $ pipePush $ "Writing on path " ++ path ++ "..."
        result <- liftIO $ runExceptT downloadExceptT
        let state = either singleFaStateWith (const singleSuState) result
        liftIO $ writeChan chan state
        return state
  where
    downloadExceptT :: ExceptT SomeException IO ()
    downloadExceptT = writeNode parent node
    path = parent </> relativePath node

genTreeSeeds :: MonadRIOE' m => TreeSeed -> m (FSNode, [TreeSeed])
genTreeSeeds (Left filej) = return (filejsonToNode filej, [])
genTreeSeeds (Right folderj) = do
    filejsons <- canvasJSON $ files_url folderj
    folderjsons <- canvasJSON $ folders_url folderj
    let seeds = map Left (filejsons :: [FileJSON])
             ++ map Right (folderjsons :: [FolderJSON])
    return (FolderNode $ name folderj, seeds)
