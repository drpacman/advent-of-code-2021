module Day1(parts) where

import Utils(PuzzlePart)
import Debug.Trace

parts :: ( PuzzlePart Int, PuzzlePart Int )
parts = ( part1, part2 )

part1 :: PuzzlePart Int
part1 input = 
    countIncrementingItems (toInt input)

part2 :: PuzzlePart Int 
part2 input = 
    -- trace ("calling count = " ++ show windowed_values) 
    countIncrementingItems windowed_values
    where 
        items = toInt input
        windowed_values = window 3 items
        
toInt :: [String] -> [Int]
toInt = map read

process :: (Int, Int)  -> Int -> (Int, Int)
process (last, count) x  
   | x > last  = (x, count+1)
   | otherwise = (x, count)

countIncrementingItems :: [Int] -> Int
countIncrementingItems items = snd $ foldl process (head items,0) (tail items)
    
window :: Int -> [Int] -> [Int]
window n items
    | length items >= n = sum (take n items) : window n (tail items)
    | otherwise = []

