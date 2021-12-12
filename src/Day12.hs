module Day12(part1, part2) where
import Utils (PuzzlePart)

import Data.Map
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char
import Data.Monoid (All)

-- path is the nodes and a flag indicating if a small cave can be revisited
type Path = ([String], Bool)
type Graph = Map String [String]
type AllowedEntryFilter = (String -> Path -> Bool)

updateEntry :: String -> [String] -> Maybe [String]
updateEntry x xs = Just (Data.List.filter (/= "start") $ Data.List.union [x] xs)

-- add child node name to both items
updateGraph :: Graph -> [String] -> Graph
updateGraph graph (a:b:[]) = Data.Map.update (updateEntry a) b $ Data.Map.update (updateEntry b) a graph

toGraph :: [String] -> Graph
toGraph input = Data.List.foldl (\g entry -> updateGraph g entry) initialGraph entries
    where 
        entries = Data.List.map (splitOn "-") input
        initialGraph = fromList $ Data.List.map (\x -> (x,[])) $ nub (concat entries)

extendPathPart :: Path -> String -> Maybe Path
extendPathPart (path, False) node = if not (isLower (head node)) || notElem node path then Just ((node:path), False) else Nothing
extendPathPart (path, True) node = if isLower (head node) && elem node path then Just ((node:path), False) else Just ((node:path), True)

extendPath :: Graph -> Path -> [Path]
extendPath graph ("end":xs,canRevisit) = [("end":xs, canRevisit)]
extendPath graph path = if Data.List.null paths then [path] else paths
    where
        node = head (fst path)
        Just nodePath = Data.Map.lookup node graph
        paths = catMaybes $ Data.List.map (extendPathPart path) nodePath

nextPathsPart :: Graph -> [Path] -> [Path]
nextPathsPart graph paths = 
    if paths == paths' then
        paths
    else
        nextPathsPart graph paths' 
    where 
        paths' = concatMap (extendPath graph) paths

countPaths :: [String] -> Path -> Int 
countPaths input init = length completePaths
    where 
        graph = toGraph input
        paths = nextPathsPart graph [init] 
        completePaths = Data.List.filter (elem "end") $ Data.List.map fst paths

part1 :: PuzzlePart Int 
part1 input = countPaths input (["start"], False)

part2 :: PuzzlePart Int 
part2 input = countPaths input (["start"], True)
