module Files.Tree (
    traverseTreeFoldPar,
    isSingleNode
) where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Data.Traversable         (Traversable)
import           Data.Tree

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
