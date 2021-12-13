module Day13(part1, part2) where
import Utils (PuzzlePart)
import Data.List.Split
import Debug.Trace
import Data.List
import Data.String

type Coord = (Int, Int)
type Fold = [Coord] -> [Coord]

toCoords :: [String] ->  [Coord]
toCoords input = map toCoord $ fst $ span (\x -> length x > 0) input

toCoord :: String -> Coord
toCoord line = (entries !! 0, entries !! 1)
    where entries = map read $ splitOn "," line

toFolds :: [String] -> [Fold]
toFolds input = map toFold $ tail $ snd $ span (\x -> (length x > 0)) input

toFold :: String -> Fold
toFold line = if axis == 'x' then foldX value else foldY value
    where 
        axis= line !! 11
        value=read $ drop 13 line

foldY :: Int -> Fold
foldY yPos coords = map (\(x,y) -> if y > yPos then (x, y - (2 * (y-yPos))) else (x,y)) coords
    
foldX :: Int -> Fold
foldX xPos coords = map (\(x,y) -> if x > xPos then (x - (2*(x-xPos)),y) else (x,y)) coords

part1 :: PuzzlePart Int 
part1 input = length (nub coords')
    where 
        coords = toCoords input
        folds = toFolds input
        coords' = (head folds) coords

render :: [Coord] -> [String]
render coords = Data.List.Split.chunksOf (width+1) items
    where 
        width = maximum $ map fst coords
        height = maximum $ map snd coords
        items = [ if (x,y) `elem` coords then '*' else ' ' | y <- [0..height], x <- [0..width] ]

dropDuplicates :: Eq a => [a] -> [a] -> [a] 
dropDuplicates (x:xs) (r:rs) = if x == r
                                then dropDuplicates xs (r:rs)
                              else 
                                dropDuplicates xs (x:r:rs)
dropDuplicates [] result = result
dropDuplicates (x:xs) [] = dropDuplicates xs [x]


part2 :: PuzzlePart String
part2 input = concat $ map (\x -> '\n':x) lines
    where 
        coords = toCoords input
        folds = toFolds input
        coords' = dropDuplicates (sort $ foldl (\acc f -> f acc) coords folds) []
        lines = render coords'
