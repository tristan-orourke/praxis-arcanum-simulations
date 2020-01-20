module RandomDraw where

import System.Random (RandomGen, randomR)
import Data.List

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

draw1 :: RandomGen g => g -> [a] -> (a,[a],g)
draw1 g d = (d!!i, deleteAt i d, g')
    where (i, g') = randomR (0, (length d - 1)) g

draw :: RandomGen g => Int -> g -> [a] -> ([a],[a],g)
draw _ g [] = ([], [], g)
draw 0 g xs = ([], xs, g)
draw n g xs = 
    let (x, xs', g') = draw1 g xs in
        let (hs, xs'', g'') = draw (n-1) g' xs' in
            (x:hs, xs'', g'')

hands :: RandomGen g => Int -> Int -> g -> [a] -> ([[a]], g)
hands n size g xs =
    let hands' :: RandomGen g => [[a]] -> Int -> Int -> g -> [a] -> ([[a]], g)
        hands' hs 0 _ g _ = (hs, g)
        hands' hs n size g xs = 
            let (h, _, g') = draw size g xs
                in hands' ([h]++hs) (n-1) size g' xs
        in hands' [] n size g xs


-- >>> hands