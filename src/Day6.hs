module Day6(part1, part2) where
import Utils (PuzzlePart)

import Data.List.Split
import qualified Data.List (map, group, sort)
import Debug.Trace
import Data.Map 

runRound :: Map Int Int -> Map Int Int
runRound entries = if Data.Map.member 6 withNewEntries 
                     then 
                        Data.Map.adjust (+newCount) 6 withNewEntries
                    else
                        Data.Map.insert 6 newCount withNewEntries
                    where 
                        -- find how count of entries with zero days left
                        newCount = findWithDefault 0 0 entries 
                        -- drop them
                        droppedZeros = delete 0 entries
                        -- decrement all the remaining entries
                        existing = Data.Map.mapKeys (\x -> x-1) droppedZeros 
                        -- add new entries, they start with 8 days
                        withNewEntries = Data.Map.insert 8 newCount existing

calculate :: [String] -> Int -> Int
calculate input n = Data.Map.foldl (+) 0 result
    where 
        -- create Map of value -> count
        seed = Data.List.map (\x -> (head x, length x)) $ Data.List.group $ Data.List.sort $ Data.List.map read (splitOn "," (head input))
        result = iterate runRound (fromList seed) !! n

part1 :: PuzzlePart Int 
part1 input = calculate input 80

part2 :: PuzzlePart Int 
part2 input = calculate input 256