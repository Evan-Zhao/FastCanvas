module Files.Structure (
    unfoldFileTree,
    downloadTree,
    renameRoot,
    isSingleNode
) where

import           Control.Monad       (mapM)
import           Data.Foldable
import           Data.Tree
import           Files.Node.FSNode
import           Files.Node.NodeJSON
import           Files.State
import           Settings
import           System.FilePath     ((</>))

downloadTreeUncounted :: FilePath -> Tree FSNode -> GlobalNoE (Tree DownloadState)
downloadTreeUncounted parent (Node root chs) = do
    rootState <- writeAndCount parent root
    childStates <- mapM (downloadTreeUncounted newPath) chs
    return $ Node rootState childStates
      where
        newPath = parent </> relativePath root

downloadTree :: FilePath -> Tree FSNode -> GlobalNoE DownloadState
downloadTree parent rootNode = fold <$> downloadTreeUncounted parent rootNode

writeAndCount :: FilePath -> FSNode -> GlobalNoE DownloadState
writeAndCount parent node = do
    exist <- doesWrite parent node
    if exist then return singleExState else do
        liftIO $ putStrLn $ "Writing on path " ++ path ++ "..."
        result <- runExceptT $ writeNode parent node
        return $ either singleFaStateWith (const singleSuState) result
  where
    path = parent </> relativePath node

unfoldFileTree :: TreeSeed -> Global (Tree FSNode)
unfoldFileTree = unfoldTreeM genTreeSeeds

type TreeSeed = Either FileJSON FolderJSON

genTreeSeeds :: TreeSeed -> Global (FSNode, [TreeSeed])
genTreeSeeds (Left filej) = return (filejsonToNode filej, [])
genTreeSeeds (Right folderj) = do
    filejsons <- simpleHttpJSON' $ files_url folderj :: Global [FileJSON]
    folderjsons <- simpleHttpJSON' $ folders_url folderj :: Global [FolderJSON]
    let seeds = map Left filejsons ++ map Right folderjsons
    return (FolderNode $ name folderj, seeds)

renameRoot :: String -> Tree FSNode -> Tree FSNode
renameRoot newName (Node root xs) = Node (root { relativePath = newName }) xs

isSingleNode :: Tree a -> Bool
isSingleNode (Node _ []) = True
isSingleNode _           = False
