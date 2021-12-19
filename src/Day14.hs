{-# LANGUAGE TupleSections #-}
module Day14(part1, part2) where

import Data.Map
import Utils (PuzzlePart)
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Bits (FiniteBits(countLeadingZeros))

type Pair = (Char, Char)

parseInput :: [String] -> (String, Map Pair Char)
parseInput input = (initial, fromList transforms)
    where
        initial = head input
        rest = Data.List.map (\line -> (line !! 0, line !!2)) $ Data.List.map words $ Data.List.drop 2 input
        transforms =Data.List.map (\entry -> (((fst entry) !! 0, (fst entry) !! 1), snd entry !! 0)) rest

createPairs :: String -> [Pair] -> [Pair]
createPairs [] pairs = pairs
createPairs [x] pairs = pairs
createPairs (x:xs) pairs = createPairs xs ((x, head xs):pairs)

generateNext :: Map Pair Char -> Pair -> [Pair]
generateNext m (a,b) = [(a,x), (x,b)]
    where
        Just x = Data.Map.lookup (a,b) m


step :: Map Pair Char -> String -> String
step m curr = concatMap (\p -> [(fst p), (snd p)]) nextStep ++ [end]
    where
        pairs = reverse $ createPairs curr []
        generator = generateNext m
        nextStep = Data.List.map head $ Data.List.map (generator) pairs
        end = last curr


updateCounts :: Map Pair Char -> Map Pair Int -> Map Pair Int
updateCounts t counts = Data.List.foldl (\m (p,n) -> alter (addOrInsert n) p m) empty nextCounts
    where
        next = generateNext t
        -- generate new pairs
        nextCounts = concatMap (\(p,n) -> (Data.List.map (\p1 -> (p1,n)) (next p))) $ toList counts  
        
addOrInsert :: Int -> Maybe Int -> Maybe Int
addOrInsert n (Just x) = Just (x + n)
addOrInsert n Nothing = Just n

calculate :: [String] -> Int -> Int 
calculate input rounds =maximum (elems summedSingles) - minimum  (elems summedSingles)
    where 
        (start, transforms) = parseInput input
        firstChar = head start
        lastChar = last start
        initialPairs = fromList $ Data.List.map (\g -> (head g, length g)) $ group $ sort $ createPairs start []
        stepper = updateCounts transforms
        result = (iterate stepper initialPairs) !! rounds
        counts = concatMap (\((a,b),n) -> [(a,n), (b,n)]) $ toList result
        -- add one for the first and last initial character
        countsWithStartAndEnd = (firstChar,1):(lastChar,1): counts
        -- count the chars and divide by 2 (everything has been double counted)
        -- first group by char and count
        groups = group $ sort countsWithStartAndEnd
        -- sum up each group of (char, count) groups
        singles = Data.List.map (\xs -> (fst (head xs), (snd (head xs))*(length xs))) groups
        countsByCharacter = Data.List.foldl (\m (k, v) -> alter (addOrInsert v) k m ) empty singles
        summedSingles = Data.Map.map (\x -> div x 2) countsByCharacter

-- part1BruteForce :: PuzzlePart  Int
-- part1BruteForce input = fst (last counts) - fst (head counts)
--     where
--         (start, transforms) = parseInput input 
--         steps = step transforms 
--         result = iterate steps start !! 10
--         counts = sort $ Data.List.map (\p -> (length p, head p)) $ group $ sort result

part1 :: PuzzlePart Int 
part1 input = calculate input 10

part2 :: PuzzlePart Int 
part2 input = calculate input 40