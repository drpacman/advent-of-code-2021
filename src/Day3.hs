module Day3(part1, part2) where

import Utils(PuzzlePart)
import Debug.Trace
import Data.List (group, transpose, sort)

toBits :: String -> [Bool]
toBits line = map (== '1') line


getMax :: [(Bool, Int)] -> Bool
getMax entry = if x > y then b1 else if x == y then True else b2
    where 
        (b1, x) = head entry
        (b2, y) = head (tail entry) 

toInt :: [Bool] -> Int 
toInt = foldl (\acc x -> 2*acc + if x then 1 else 0) 0

part1 :: PuzzlePart Int
part1 input = 
    toInt epsilon * toInt gamma
    where 
        columns = transpose (map toBits input)
        countsPerColumn = map (map (\g -> (head g, length g)) . group . sort) columns
        gamma = map getMax countsPerColumn
        epsilon = map (not) gamma

calculatePart2 :: [[Bool]] -> (Bool -> Bool) -> Int -> Int
calculatePart2 (row:[]) f _ = toInt row
calculatePart2 rows f pos = calculatePart2 remainder f (pos+1)
    where
        column = (transpose rows) !! pos
        countColumn = map (\g -> (head g, length g)) $ group $ sort column
        requiredValue = f (getMax countColumn)
        remainder = filter (\x ->  x!!pos == requiredValue) rows

part2 :: PuzzlePart Int
part2 input =
    oxygen*co2
    where
        rows = map toBits input
        oxygen = calculatePart2 rows id 0 
        co2 = calculatePart2 rows not 0 
        
        
