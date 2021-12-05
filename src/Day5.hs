module Day5(part1, part2) where
import Utils (PuzzlePart)
import Data.List
import Data.List.Split ( splitOn )
import Debug.Trace

type Coord = (Int, Int)
type Vent = (Coord, Coord)

toVents:: [String] -> [Vent]
toVents input =  map toVent input

toVent:: String -> Vent 
toVent input = ((startX, startY), (endX, endY))
    where
        entries = words input
        startX:startY:[] = map read (splitOn "," (head entries))
        endX:endY:[] = map read (splitOn "," (last entries))

straightEntries :: Vent -> [Coord]
straightEntries ((x1,y1),(x2,y2)) 
  | x1 == x2 && y1 > y2 = map (\y -> (x1, y)) [y2..y1]
  | x1 == x2 && y1 < y2 = map (\y -> (x1, y)) [y1..y2]
  | x1 > x2 && y1 == y2 = map (\x -> (x, y1)) [x2..x1]
  | x1 < x2 && y1 == y2 = map (\x -> (x, y1)) [x1..x2]
  | otherwise = []

diagonalEntries :: Vent -> [Coord]
diagonalEntries ((x1,y1),(x2,y2)) 
  | x1 > x2 && y1 > y2 = zip [x1,x1-1..x2] [y1,y1-1..y2]
  | x1 > x2 && y1 < y2 = zip [x1,x1-1..x2] [y1..y2]
  | x1 < x2 && y1 > y2 = zip [x1..x2] [y1,y1-1..y2]
  | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
  | otherwise = []

part1 :: PuzzlePart Int 
part1 input = length $ filter (\c -> length c > 1) allCoords
    where 
        vents = toVents input
        straight = filter (/= []) $ map straightEntries vents
        allCoords = group . sort $ concat straight

part2 :: PuzzlePart Int 
part2 input = length $ filter (\c -> length c > 1) allCoords
    where 
        vents = toVents input
        straight = filter (/= []) $ map straightEntries vents
        diagonal = filter (/= []) $ map diagonalEntries vents
        allCoords = group . sort $ concat straight ++ concat diagonal