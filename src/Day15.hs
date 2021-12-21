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

newtype Node = Node (Coord, Int) deriving (Eq)

instance Ord Node where
    compare (Node (_, c1)) (Node(_, c2)) = compare c1 c2

data Dijkstra = Dijkstra {
    shortestDists :: Map Coord Node,
    distances :: Map Coord Node,
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
        shortestDists = fromList [ ((0,0), Node ((0,0),0)) ]
        width = length (head grid)
        height = length grid
        distances = Map.empty

updateValue :: Coord -> Int -> Maybe Node -> Maybe Node
updateValue parent x (Just (Node(p, n))) = if n > x then Just (Node (parent, x)) else Just (Node (p, n))
updateValue parent x Nothing  = Just (Node(parent, x))

walkGrid :: Grid -> Dijkstra -> Coord -> Dijkstra
walkGrid grid dijkstra target = if c == target then  dijkstra' else walkGrid grid dijkstra' target
    where 
        -- get neighbours for current that aren't in the existing shortest path
        curr = current dijkstra
        nextEntries = Data.List.filter (\c -> notMember c (shortestDists dijkstra)) $ neighbours grid curr
        -- get distance to current node
        Node (_, dist) = (shortestDists dijkstra) ! curr       
        -- for each neighbour, update distance with new min value
        distances' = Data.List.foldl (\ds c -> let 
                                                   updater = updateValue curr
                                                   entryValue = value c grid
                                                   newValue = dist+entryValue
                                                in
                                                    alter (updater newValue) c ds) (distances dijkstra) nextEntries
        -- get minimal entry 
        (c, Node (parent, distance)) = Data.List.minimumBy (comparing (\(c, Node (p, n)) -> n)) $ toList distances'        
        -- update values
        dijkstra' = Dijkstra { 
            shortestDists = Map.insert c (Node(parent, distance)) (shortestDists dijkstra), 
            distances = Map.delete c distances', 
            current = c 
        }

calculatePath :: Dijkstra -> Coord -> [Coord]
calculatePath dijkstra target = reverse $ 
                Data.List.unfoldr (\c -> 
                    if c == (0,0) 
                        then Nothing 
                    else 
                        let 
                            dists= shortestDists dijkstra
                            Node (parent, _) = dists ! c
                        in
                            Just (c, parent)
                 ) target


expandRow ::  Int -> [Int] -> [Int]
expandRow n row = Data.List.concatMap (\i -> (Data.List.map (\x -> if x+i > 9 then x+i - 9 else x+i ) row)) [n..n+4]

expandGrid :: Grid -> Int -> Grid
expandGrid grid n = Data.List.map (expandRow n) grid

part1  :: PuzzlePart Int 
part1 input = result
    where 
        grid = toGrid input
        ds = initDijsktra grid
        width = length (head grid)
        height = length grid
        target = (width-1, height-1)
        ds' = walkGrid grid ds target
        Node (_, result) = (shortestDists ds') ! target

part2 :: PuzzlePart Int
part2 input = result
    where 
        baseGrid = toGrid input
        expandedGrid = Data.List.concatMap (expandGrid baseGrid) [0..4]
        ds = initDijsktra expandedGrid
        width = length (head expandedGrid)
        height = length expandedGrid
        target = (width-1, height-1)
        ds' = walkGrid expandedGrid ds target
        Node (_, result) = (shortestDists ds') ! target
