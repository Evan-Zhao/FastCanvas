module Files.Tree (
    traverseTreeFold,
    traverseTreeFoldPar,
    isSingleNode
) where

import           Control.Concurrent.Async
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
    childrensNew <- mapConcurrently nextStep ch
    return $ Node nodeNew childrensNew

isSingleNode :: Tree a -> Bool
isSingleNode (Node _ []) = True
isSingleNode _           = False
