module Main (main) where

import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Tree

width = 5

genTree :: (Int, Int) -> Tree Int
genTree (l,r) = evalState (unfoldTreeM_BF generator 0) (l,r) where
    generator :: Int -> State (Int, Int) (Int, [Int])
    generator _ = do
        (now, max) <- get
        let subs = if now >= max then [] else replicate width 0
        put (now+1, max)
        return (now, subs)

expensive2 :: [Int] -> Int -> IO ()
expensive2 xs n = do
    putStrLn $ "Ancestors: " ++ show xs
    putStrLn $ "Self: " ++ show val ++ "\n"
    where
        val = length [ x | x <- takeWhile ((<n) . (^2)) [1..], n `rem` x == 0]

traverseTreeFold :: Monad m
                 => (r -> a -> r) -> (r -> a -> m b)
                 -> r -> Tree a -> m (Tree b)
traverseTreeFold accF mapF acc (Node n ch) = do
    nodeNew <- mapF acc n
    let new' = parMap rseq (traverseTreeFold accF mapF $ acc `accF` n) ch
    childrensNew <- sequence new'
    return $ Node nodeNew childrensNew

main :: IO ()
main = do
    traverseTreeFold (\xs x -> xs ++ [x]) expensive2 [] $ genTree (10^9,10^9+50)
    return ()
