{-# LANGUAGE TypeSynonymInstances #-}
module Day15(part1, part2) where

import Utils (PuzzlePart)
import Data.Char (digitToInt)
import Data.Map as Map
import Data.List
import Data.Ord
import Debug.Trace

type Grid = [[Int]]
type Coord = (Int, Int)

newtype ParentWithCost = Path (Coord, Int) deriving (Eq)

instance Ord ParentWithCost where
    compare (Path (_, c1)) (Path(_, c2)) = compare c1 c2

data Dijkstra = Dijkstra {
    shortestDists :: Map Coord (Coord, Int),
    distances :: Map Coord (Coord, Int),
    current :: Coord
}

toGrid :: [String] -> Grid
toGrid input = Data.List.map (Data.List.map digitToInt) input

value :: Coord -> Grid -> Int
value  (x, y) grid = grid !! y !! x

neighbours :: Grid -> Coord -> [Coord]
neighbours grid (x,y) = Data.List.filter (\(x,y) -> x >= 0 && x < width && y >= 0 && y < height ) $ [ (x-1,y), (x+1, y), (x,y-1), (x,y+1)]
    where
        width = length (head grid)
        height = length grid

initDijsktra :: Grid -> Dijkstra
initDijsktra grid = Dijkstra { shortestDists = shortestDists, distances = distances, current = (0,0) }
    where
        shortestDists = fromList [ ((0,0), ((0,0),0)) ]
        width = length (head grid)
        height = length grid
        distances = Map.delete (0,0) $ Map.fromList [((x,y), ((x,y),999999999)) | y <- [0..height-1], x <- [0..width-1]]

updateValue :: Coord -> Int -> Maybe (Coord, Int) -> Maybe (Coord, Int) 
updateValue parent x (Just (p, n)) = if n > x then Just (parent, x) else Just (p, n)
updateValue parent x Nothing  = Just (parent, x)

walkGrid :: Grid -> Dijkstra -> Coord -> Dijkstra
walkGrid grid dijkstra target = if c == target then  dijkstra' else walkGrid grid dijkstra' target
    where 
        -- get neighbours for current that aren't in the existing shortest path
        curr = trace (show (length (distances dijkstra))) current dijkstra
        nextEntries = Data.List.filter (\c -> notMember c (shortestDists dijkstra)) $ neighbours grid curr
        -- get distance to current node
        (_, dist) = (shortestDists dijkstra) ! curr       
        -- for each neighbour, update distance with new min value
        distances' = Data.List.foldl (\ds c -> alter (updateValue curr (dist+(value c grid))) c ds) (distances dijkstra) nextEntries
        -- get minimal entry 
        (c, (parent, distance)) = Data.List.minimumBy (comparing (\(c, (p, n)) -> n)) $ toList distances'        
        -- update values
        dijkstra' = Dijkstra { 
            shortestDists = Map.insert c (parent, distance) (shortestDists dijkstra), 
            distances = Map.delete c distances', 
            current = c 
        }

calculatePath :: Dijkstra -> Coord -> [Coord]
calculatePath dijkstra target = reverse $ Data.List.unfoldr (\c -> if c == (0,0) then Nothing else Just (c, fst (dists ! c))) target
    where
        dists= shortestDists dijkstra


expandRow ::  Int -> [Int] -> [Int]
expandRow n row = Data.List.concatMap (\i -> (Data.List.map (\x -> if x+i > 9 then x+i - 9 else x+i ) row)) [n..n+4]

expandGrid :: Grid -> Int -> Grid
expandGrid grid n = Data.List.map (expandRow n) grid

part1  :: PuzzlePart Int 
part1 input = snd ((shortestDists ds') ! target)
    where 
        grid = toGrid input
        ds = initDijsktra grid
        width = length (head grid)
        height = length grid
        target = (width-1, height-1)
        ds' = walkGrid grid ds target

part2 :: PuzzlePart Int
part2 input = snd ((shortestDists ds') ! target)
    where 
        baseGrid = toGrid input
        expandedGrid = Data.List.concatMap (expandGrid baseGrid) [0..4]
        ds = initDijsktra expandedGrid
        width = length (head expandedGrid)
        height = length expandedGrid
        target = (width-1, height-1)
        ds' = walkGrid expandedGrid ds target
