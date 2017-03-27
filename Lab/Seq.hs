module Main (main) where

import           Control.Parallel

expensive1 :: Int -> Int -> Int
expensive1 steps 1 = steps
expensive1 steps x = expensive1 (steps + 1) $ f x where
    f x = if even x then x `div` 2 else 3*x+1

expensive1' :: Int -> Int
expensive1' x = expensive1 0 (100*x)

expensive2 :: Integer -> Int
expensive2 n = length [ x | x <- takeWhile ((<n) . (^2)) [1..], n `rem` x == 0]

main :: IO ()
main = print $ maximum $ map expensive2 [10^9..10^9+1000]
