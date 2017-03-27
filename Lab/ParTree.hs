module Main (main) where

import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Tree

width = 3

genTree :: (Int, Int) -> Tree Int
genTree (l,r) = evalState (unfoldTreeM_BF generator 0) (l,r) where
    generator :: Int -> State (Int, Int) (Int, [Int])
    generator _ = do
        (now, max) <- get
        let subs = if now >= max then [] else replicate width 0
        put (now+1, max)
        return (now, subs)

expensive2 :: Int -> Int
expensive2 n = length [ x | x <- takeWhile ((<n) . (^2)) [1..], n `rem` x == 0]

main :: IO ()
main = print $ maximum (chunk `using` parTraversable rseq) where
    chunk = expensive2 <$> genTree (10^9,10^9+1000)
