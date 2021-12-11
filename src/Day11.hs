module Day11(part1, part2) where
import Utils (PuzzlePart)
import Data.Char
import Debug.Trace
import Data.Maybe
import Data.List
import Data.List.Split

type Coord = (Int, Int)
type Grid = [[Int]]

toGrid :: [String] -> Grid
toGrid = map (map digitToInt)

getValue:: Grid -> Coord -> Int 
getValue array (x,y) = array !! y !! x

gridHeight :: Grid -> Int 
gridHeight = length
        
gridWidth :: Grid -> Int 
gridWidth grid = length (head grid)

getNeighbours:: Grid -> Coord -> [Coord]
getNeighbours grid (x, y) = catMaybes [ up, upright, right, downright, down, downleft, left, upleft ]
    where 
        width = gridHeight grid
        height = gridWidth grid
        up = if y > 0 then Just (x, y - 1) else Nothing
        down = if y + 1 < height then Just (x, y + 1) else Nothing
        left  = if x > 0 then Just (x - 1, y) else Nothing
        right = if x + 1 < width then Just (x + 1, y) else Nothing
        upright = if x + 1 < width  && y > 0 then Just (x + 1, y - 1) else Nothing
        downright = if x + 1 < width && y + 1 < height then Just (x + 1, y + 1) else Nothing
        upleft  = if x > 0  && y > 0 then Just (x - 1, y - 1) else Nothing
        downleft = if x > 0 && y + 1 < height then Just (x - 1, y + 1) else Nothing
        
neighbouringFlashes :: Grid -> Coord -> Int
neighbouringFlashes grid (x,y) = length $ filter (>=10) neighourValues
    where
        neighourValues = map (getValue grid) $ getNeighbours grid (x,y)

getUpdatedValue :: Grid -> Coord -> Int 
getUpdatedValue grid coord
    | currentValue > 9 = 0
    | currentValue == 0 = 0
    | otherwise = currentValue + neighbouringFlashes grid coord
    where
        currentValue = getValue grid coord

flash :: Grid -> Grid
flash grid =
    if allDone then grid'
    else
        flash grid'
    where    
        width = gridWidth grid
        height = gridHeight grid    
        updatedValues = [ getUpdatedValue grid (x,y) | y <- [0..height-1], x <- [0..width-1] ]   
        allDone = isNothing $ find (>=10) updatedValues     
        grid' = chunksOf width updatedValues

updateGrid :: (Int -> Int) -> Grid -> Grid
updateGrid f = map (map f)

scoreGrid :: Grid -> Int 
scoreGrid grid = length $ filter (==0) $ concat grid

doRound :: Grid -> Grid 
doRound grid = flash $ updateGrid (+1) grid

doRoundKeepScore :: (Grid, Int) -> (Grid, Int)
doRoundKeepScore (grid, total) = (grid', total + score)
    where
        grid' = doRound grid
        score = scoreGrid grid'

part1 :: PuzzlePart Int 
part1 input = score
    where 
        grid = toGrid input
        (finalGrid, score) = iterate doRoundKeepScore (grid,0) !! 100

countRoundsUntilAllFlashing :: Grid -> Int -> Int 
countRoundsUntilAllFlashing grid count = 
    if allFlashing then count
    else countRoundsUntilAllFlashing grid' count+1
    where 
        grid' = doRound grid
        allFlashing = all (==0) $ concat grid'

part2 :: PuzzlePart Int 
part2 input = countRoundsUntilAllFlashing grid 1
    where 
        grid = toGrid input
