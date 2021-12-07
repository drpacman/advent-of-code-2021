module Day7(part1,part2) where
import Utils (PuzzlePart)

import Data.List.Split
import Data.List
import Debug.Trace

calculate :: [String] -> (Int -> Int -> Int) -> Int
calculate input f = minimum values
    where 
        crabs = map (\x -> (head x, length x)) $ group $ sort $ map read (splitOn "," (head input))
        range = [ fst (head crabs) .. fst (last crabs) ]
        values = map (\pos -> foldl (\acc (x,n) -> acc + f (abs (x-pos)) n) 0 crabs) range        

part1 :: PuzzlePart Int 
part1 input = calculate input (\x n -> x*n)

part2 :: PuzzlePart Int 
part2 input = calculate input (\x n -> sum[1..x]*n)
