module Files.Tree (
    traverseTreeFold,
    traverseTreeFoldPar,
    isSingleNode
) where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Data.Traversable         (Traversable)
import           Data.Tree

traverseTreeFold :: Monad m
                 => (r -> a -> r) -> (r -> a -> m b)
                 -> r -> Tree a -> m (Tree b)
traverseTreeFold accF mapF acc (Node n ch) = do
    nodeNew <- mapF acc n
    childrensNew <- mapM (traverseTreeFold accF mapF $ acc `accF` n) ch
    return $ Node nodeNew childrensNew

traverseTreeFoldPar :: (r -> a -> r) -> (r -> a -> IO b)
                    -> r -> Tree a -> IO (Tree b)
traverseTreeFoldPar accF mapF acc (Node n ch) = do
    nodeNew <- mapF acc n
    let nextStep = traverseTreeFoldPar accF mapF $ acc `accF` n
    childrensNew <- mapPool 10 nextStep ch
    return $ Node nodeNew childrensNew

mapPool :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

isSingleNode :: Tree a -> Bool
isSingleNode (Node _ []) = True
isSingleNode _           = False
