module Day2(part1,part2) where

import Utils(PuzzlePart)
import Debug.Trace

data Direction = Forward Int 
               | Up Int
               | Down Int deriving (Show)

toDirection :: String -> Direction
toDirection s = 
    case words s of
        ["forward",x] -> Forward (read x::Int)
        ["up",x] -> Up (read x::Int)
        ["down",x] -> Down (read x::Int)
        otherwise -> trace(show otherwise) error "Unexpected direction"

calculatePostionPart1 :: [Direction] -> (Int, Int)
calculatePostionPart1 = 
    foldl (\(x,y) direction -> 
            case direction of
                Forward n -> (x+n,y)
                Up n -> (x, y-n)
                Down n -> (x, y+n)
        ) (0,0)

calculatePostionPart2 :: [Direction] -> (Int, Int, Int)
calculatePostionPart2 = 
    foldl (\(x,y,aim) direction -> 
            case direction of
                Forward n -> (x+n,y+(aim * n), aim)
                Up n -> (x, y, aim-n)
                Down n -> (x, y, aim+n)
        ) (0,0,0)

part1 :: PuzzlePart Int
part1 input = 
    x * y    
    where (x,y) = calculatePostionPart1 (map toDirection input)

part2 :: PuzzlePart Int
part2 input = 
    x * y    
    where (x,y,_) = calculatePostionPart2 (map toDirection input)
        
