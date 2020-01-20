module Lib where

import Data.List

makeDeck :: Int -> Int -> [Int]
makeDeck min max = [min..max] ++ (replicate (diff*3) 0)
    where diff = max - min + 1

handPermutations :: Int -> [Int] -> [[Int]]
handPermutations n d = map (take n) (permutations d)

handScore :: [Int] -> Int
handScore = sum

scoreAtLeast :: Int -> [Int] -> Bool
scoreAtLeast x h = x == (handScore h)

percentHandsAtLeast :: Int -> [[Int]] -> Double
percentHandsAtLeast x hs = 
    let n = length $ filter (scoreAtLeast x) hs
        l = length hs
        in fromIntegral n / fromIntegral l
