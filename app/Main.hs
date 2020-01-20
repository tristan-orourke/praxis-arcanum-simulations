module Main where

import Lib
import RandomDraw
import System.Random
import Numeric

main :: IO ()
main = do
    let deck = makeDeck 1 4
    -- let deck = [1,2,3,4]
    -- let hands = handPermutations 2 deck
    seed  <- newStdGen
    let n = 10000
    let (hs, g') = hands n 4 seed deck
    let percent = percentHandsAtLeast 10 hs
    let percentF = showFFloat (Just 6) percent ""
    print percentF