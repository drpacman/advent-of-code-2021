module Day4(part1,part2) where

import Utils (PuzzlePart)
import Data.List (find, transpose, sort, intersect)
import Data.List.Split (splitOn, chunksOf)
import Data.Maybe ( isJust )
import Debug.Trace ()

data Bingo = Game [Int] [Board] deriving (Show)
type Board = [[Int]]

toBoard :: [String] -> Board
toBoard = map parseLine

parseLine :: [Char] -> [Int]
parseLine line = map read $ words line

toBingo :: [String] -> Bingo
toBingo input = 
    Game balls boards
    where 
        balls = map read $ splitOn "," (head input)
        blocks = map tail $ chunksOf 6 (tail input)
        boards = map toBoard blocks 

findWinningBoard :: Bingo -> [Int] -> ([Int], Board)
findWinningBoard (Game (b:balls) boards) x = 
    case winner of 
        Just w -> (used, w)
        Nothing ->  findWinningBoard (Game balls boards) used
    where 
        used = b:x
        winner = find (isWinningBoard used) boards
findWinningBoard (Game _ boards) x = error "No result"

findLastWinningBoard :: Bingo -> [Int] -> ([Int], Board)
-- keep going with last board till it wins
findLastWinningBoard (Game (b:balls) [board]) x = 
    if isWinningBoard used board 
        then
            (used, board)
        else findLastWinningBoard (Game balls [board]) used
    where 
        used = b:x
        winner = isWinningBoard used board
-- keep dropping winning boards till there is one left
findLastWinningBoard (Game (b:balls) boards) x = findLastWinningBoard (Game balls boards') used
    where 
        used = b:x
        boards' = filter (\b -> not (isWinningBoard used b)) boards
findLastWinningBoard (Game [] boards) x = error "No result"

-- Is a winning board if all items in row or all items in column are in the list of balls
isWinningBoard :: [Int] -> Board -> Bool 
isWinningBoard balls board = isJust matchingRow || isJust matchingCol
    where
        sortedColumns = map sort $ transpose board
        matchingRow = find (\b -> length (intersect balls b) == 5) board
        matchingCol = find (\b -> length (intersect balls b) == 5) $ transpose board

score :: [Int] -> Board -> Int 
score balls board = 
    totalUnused * finalBall
    where
        boardElems = concat board
        unused = filter (\b -> not (b `elem` balls) ) boardElems
        finalBall = head balls
        totalUnused = sum unused

part1 :: PuzzlePart Int 
part1 input = score balls board
    where
        bingo = toBingo input
        (balls, board) = findWinningBoard bingo []
        

part2 :: PuzzlePart Int 
part2 input = score balls board
    where
        bingo = toBingo input
        (balls, board) = findLastWinningBoard bingo []
