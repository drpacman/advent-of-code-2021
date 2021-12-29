module Day20(part1, part2) where
import Utils (PuzzlePart)
import Data.List.Split

import Debug.Trace
parse :: [String] -> ([Int], [[Int]])
parse input = (lookupTable, image)
    where
        lookupTable = map toInt (head input)
        image = map (\row -> map toInt row) (drop 2 input)

toInt :: Char -> Int 
toInt c = if c == '#' then 1 else 0

toValue :: Int -> Int -> Int -> [[Int]] -> Int 
toValue infiniteValue x y image =  value
    where 
        bits = [ entry infiniteValue x0 y0 image | y0 <- [y-1, y, y+1], x0 <- [x-1, x, x+1]]
        value = foldl (\acc b -> (acc*2) + b) 0 bits

entry :: Int -> Int -> Int -> [[Int]] -> Int
entry infiniteValue x y image  = if y >= 0 && 
                     y < height  && 
                     x >= 0 &&
                     x < width
                        then 
                          (image !! y) !! x 
                    else 
                        infiniteValue
    where
        height = length image
        width = length (head image)

updateEntry :: [Int] -> Int -> [[Int]] -> [[Int]]
updateEntry lookup iteration image = 
    chunksOf (2 + length image) image' 
    where
        height = length image
        width = length (head image)
        infiniteValue = if even iteration then last lookup else head lookup
        image' = [ lookup !! toValue infiniteValue x y image | y <- [-1..height], x <- [-1..width]]

process :: [String] -> Int -> Int 
process input times =
    sum (concat result)
    where
        (lookup, image) = parse input
        result = foldl (\image i -> updateEntry lookup i image) image [0..(times-1)]

part1 :: PuzzlePart Int 
part1 input = process input 2

part2 :: PuzzlePart Int 
part2 input = process input 50