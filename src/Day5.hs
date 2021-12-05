{-# LANGUAGE TupleSections #-}

module Day5(part1, part2) where
import Utils (PuzzlePart)
import Data.List
import Data.List.Split ( splitOn )
import Debug.Trace

type Coord = (Int, Int)
type Vent = (Coord, Coord)

toVents:: [String] -> [Vent]
toVents =  map toVent

toVent:: String -> Vent 
toVent input = ((startX, startY), (endX, endY))
    where
        entries = words input
        [startX,startY] = map read (splitOn "," (head entries))
        [endX,endY] = map read (splitOn "," (last entries))

straightEntries :: Vent -> [Coord]
straightEntries ((x1,y1),(x2,y2)) 
  | x1 == x2 && y1 > y2 = map (x1,) [y2..y1]
  | x1 == x2 && y1 < y2 = map (x1,) [y1..y2]
  | x1 > x2 && y1 == y2 = map (,y1) [x2..x1]
  | x1 < x2 && y1 == y2 = map (,y1) [x1..x2]
  | otherwise = []

diagonalEntries :: Vent -> [Coord]
diagonalEntries ((x1,y1),(x2,y2)) 
  | x1 > x2 && y1 > y2 = zip [x1,x1-1..x2] [y1,y1-1..y2]
  | x1 > x2 && y1 < y2 = zip [x1,x1-1..x2] [y1..y2]
  | x1 < x2 && y1 > y2 = zip [x1..x2] [y1,y1-1..y2]
  | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
  | otherwise = []

countDuplicates :: [String] -> [ Vent -> [Coord] ] -> Int
countDuplicates input entries = length $ filter (\c -> length c > 1) allCoords
    where 
        vents = toVents input
        expandedVents = filter (/= []) $ foldl (\acc f -> acc ++ map f vents) [] entries
        allCoords = group . sort $ concat expandedVents

part1 :: PuzzlePart Int 
part1 input = countDuplicates input [straightEntries]

part2 :: PuzzlePart Int 
part2 input = countDuplicates input [straightEntries, diagonalEntries]