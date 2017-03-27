module Main (main) where

import           Control.Parallel
import           Control.Parallel.Strategies

expensive :: Int -> Int -> Int
expensive steps 1 = steps
expensive steps x = expensive (steps + 1) $ f x where
    f x = if even x then x `div` 2 else 3*x+1

expensive' :: Int -> Int
expensive' x = expensive 0 (100*x)

expensive2 :: Integer -> Int
expensive2 n = length [ x | x <- takeWhile ((<n) . (^2)) [1..], n `rem` x == 0]

main :: IO ()
main = print $ maximum $ parMap rseq expensive2 [10^9..10^9+1000]
