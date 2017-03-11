{-# LANGUAGE MultiParamTypeClasses #-}

module Files.Structure (
    unfoldFileTree,
    downloadTree,
    renameRoot,
    isSingleNode
) where

import           Control.Monad              (mapM)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Foldable
import           Data.Tree
import           System.FilePath            ((</>))

import           Files.Node.FSNode
import           Files.Node.NodeJSON
import           Files.State
import           Settings.Monad.Exception
import           Settings.Network

downloadTreeUncounted :: MonadIO m => FilePath -> Tree FSNode -> m (Tree DownloadState)
downloadTreeUncounted parent (Node root chs) = do
    rootState <- writeAndCount parent root
    childStates <- mapM (downloadTreeUncounted newPath) chs
    return $ Node rootState childStates
      where
        newPath = parent </> relativePath root

downloadTree :: MonadIO m => FilePath -> Tree FSNode -> m DownloadState
downloadTree parent rootNode = fold <$> downloadTreeUncounted parent rootNode

writeAndCount :: MonadIO m => FilePath -> FSNode -> m DownloadState
writeAndCount parent node = do
    exist <- doesExist parent node
    if exist then return singleExState else do
        liftIO $ putStrLn $ "Writing on path " ++ path ++ "..."
        result <- liftIO $ runExceptT (writeNode parent node :: ExceptT SomeException IO ())
        return $ either singleFaStateWith (const singleSuState) result
  where
    path = parent </> relativePath node

unfoldFileTree :: MonadIOE e m => TreeSeed -> m (Tree FSNode)
unfoldFileTree = unfoldTreeM genTreeSeeds

type TreeSeed = Either FileJSON FolderJSON

genTreeSeeds :: MonadIOE e m => TreeSeed -> m (FSNode, [TreeSeed])
genTreeSeeds (Left filej) = return (filejsonToNode filej, [])
genTreeSeeds (Right folderj) = do
    filejsons <- simpleHttpJSON' $ files_url folderj
    folderjsons <- simpleHttpJSON' $ folders_url folderj
    let seeds = map Left (filejsons :: [FileJSON])
             ++ map Right (folderjsons :: [FolderJSON])
    return (FolderNode $ name folderj, seeds)

renameRoot :: String -> Tree FSNode -> Tree FSNode
renameRoot newName (Node root xs) = Node (root { relativePath = newName }) xs

isSingleNode :: Tree a -> Bool
isSingleNode (Node _ []) = True
isSingleNode _           = False
