module Day9(part1, part2) where
import Utils (PuzzlePart)
import Data.Maybe (catMaybes)
import Data.Char
import Debug.Trace
import Data.List

type Coord = (Int, Int)

getValue:: [[Int]] -> Coord -> Int 
getValue array (x,y) = array !! y !! x

getNeighbours:: [[Int]] -> Coord -> [Coord]
getNeighbours cave (x, y) = catMaybes [ up, down, left, right ]
    where 
        up = if y > 0 then Just (x, y - 1) else Nothing
        down = if y + 1 < length cave then Just (x, y + 1) else Nothing
        left  = if x > 0 then Just (x - 1, y) else Nothing
        right = if x + 1 < length (head cave) then Just ((x + 1), y) else Nothing
  
isLowest:: [[Int]] -> Coord -> Bool 
isLowest cave coord = all (> value) neighbours
    where 
        checkCave = getValue cave
        value = checkCave coord
        neighbours = map checkCave (getNeighbours cave coord)

findBasinEntries:: [[Int]] -> [Coord] -> [Coord] -> [Coord]
findBasinEntries cave [] visited = visited
findBasinEntries cave (coord:unvisited) visited = result
    where
        caveNeighours = getNeighbours cave
        value = getValue cave
        visited' = coord:visited
        -- determine neighbours for this point which aren't a 9 (edge of basin)
        neighbours = filter (\x -> (value x) /= 9) $ caveNeighours coord
        -- determine any new entries to visit
        unvisited' =  union unvisited (neighbours \\ visited')
        result = findBasinEntries cave unvisited' visited'

toCave :: [String] -> [[Int]]
toCave input = map (\x -> map digitToInt x) input

findLowPoints :: [[Int]] -> [Coord]
findLowPoints cave = filter lowest entries
    where
        entries = [ (x,y) | y <- [0..(length cave)-1], x <- [0..(length (head cave))-1] ]
        lowest = isLowest cave

part2 :: PuzzlePart Int 
part2 input = product (map length top3)
    where
        cave = toCave input
        caveBasinEntries = findBasinEntries cave
        lowPoints = findLowPoints cave
        basins = map (\c -> caveBasinEntries [c] []) lowPoints
        sortedBasinsDesc = reverse (sortOn length basins)
        top3 = take 3 sortedBasinsDesc

part1 :: PuzzlePart Int 
part1 input = sum $ map (+1) $ map values lowPointEntries
    where
        cave = toCave input
        lowPointEntries = findLowPoints cave
        values = getValue cave