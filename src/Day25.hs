module Day25(part1, shift) where
import Utils

import Data.List
import Debug.Trace

shift :: Char -> [Char] -> [Char] -> Char -> [Char]
shift c [] output headChar = reverse output
shift c [x] output headChar
    | x == c && headChar == '.' = --trace ("ML - " ++ show x ++ "," ++ show headChar ++ show output) 
                                  x:tail (reverse ('.':output))
    | otherwise = --trace ("OL - " ++ show x ++ "," ++ show headChar ++ show output) 
                  reverse (x:output)
shift c (x1:x2:xs) output headChar
    | x1 == c  && x2 == '.' = --trace ("M - " ++ show x1 ++ "," ++ show x2 ++ show output) 
                              shift c xs (c:'.':output) headChar
    | otherwise =  --trace ("O - " ++ show x1 ++ "," ++ show x2 ++ show output) 
                   shift c (x2:xs) (x1:output) headChar

move :: Char -> [String] -> [String]
move c = map (\row -> shift c row [] (head row))

processRound :: [String] -> [String]
processRound input = transpose $ move 'v' $ transpose $ move '>' input 

run :: [String] -> Int -> Int
run input count = if input' == input then count else run input' (count+1)
    where
        input' = processRound input

part1 :: PuzzlePart Int
part1 input = run input 1