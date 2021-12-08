module Day8(part1, part2) where
import Utils (PuzzlePart)

import Data.List.Split
import Data.List
import Debug.Trace
import Text.Regex.Posix
import Data.Map

parseEntry :: String -> ([String], [String])
parseEntry line = (words wires, words digits)
    where 
        [[_, wires, digits]] = line =~ "([a-g ]+)\\| ([a-g ]*)" :: [[String]]

getEntries :: [String] -> [([String], [String])]
getEntries = Data.List.map parseEntry

-- 8 abcedfg
-- 5  bcdef
-- 2 a cd fg
-- 3 abcd f
-- 7 ab d
-- 9 abcdef
-- 6  bcdefg
-- 4 ab  ef
-- 0 abcde g
-- 1 ab

decode :: [String] -> Map String Int 
decode inputs = fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five,5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
    where 
        sortedInputs = Data.List.map sort inputs
        Just one = find (\x -> length x == 2) sortedInputs
        Just seven = find (\x -> length x == 3) sortedInputs
        Just four = find (\x -> length x == 4) sortedInputs
        Just eight = find (\x -> length x == 7) sortedInputs
        lengthFives = Data.List.filter (\x -> length x == 5) sortedInputs
        Just three = find (\x -> length (one `intersect` x) == 2) lengthFives
        Just five = find (\x -> (x /= three) && (length (four `intersect` x) == 3)) lengthFives
        Just two = find (\x -> (x /= five) && (x /= three)) lengthFives
        lengthSixes = Data.List.filter (\x -> length x == 6) sortedInputs
        Just six = find (\x -> length (one `intersect` x) /=2) lengthSixes
        Just zero = find (\x -> (x /= six) && length (four `intersect` x) == 3) lengthSixes
        Just nine = find (\x -> (x /= zero) && (x /= six)) lengthSixes



decodeRow :: ([String],[String]) -> Int
decodeRow (inputs, digits) = Data.List.foldl (\acc x -> 10 * acc + findWithDefault 0 x decoded) 0 sortedDigits
    where
        sortedDigits = Data.List.map sort digits
        decoded = decode inputs        

part2 :: PuzzlePart Int 
part2 input = sum results
    where 
        rows = getEntries input
        results = Data.List.map decodeRow rows

part1 :: PuzzlePart Int 
part1 input = length result
    where 
        rows = getEntries input
        digits = concat (Data.List.map snd rows)
        result = Data.List.filter (\d-> elem (length d) [2,3,4,7]) digits