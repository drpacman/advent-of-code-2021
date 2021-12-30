module Day21(part1, part2) where

import Debug.Trace
import Utils (PuzzlePart)
import Data.List (partition, sort)

takeTurn :: Int -> Int -> Int
takeTurn roll pos = mod (pos + roll) 10

rollDie :: Int -> Int
rollDie die =
    roll die + roll (die+1) + roll (die+2)
    where
        roll = \die -> 1 + mod die 100

playRoundPart1 :: Int -> (Int, Int) -> (Int,Int)
playRoundPart1 die (pos, score) = (pos', score + 1 + pos')
    where
        pos' = takeTurn (rollDie die) pos

playPart1 :: Int -> (Int, Int) -> (Int, Int) -> Int
playPart1 die p1 p2
   | p1score >= 1000 = snd p2 * (die + 3)
   | p2score >= 1000 = p1score * (die + 6)
   | otherwise = playPart1 (die+6) (p1pos, p1score) (p2pos, p2score)
    where
        (p1pos, p1score) = playRoundPart1 die p1
        (p2pos, p2score) = playRoundPart1 (die+3) p2

part1 :: Int
part1 = playPart1 0 (8,0) (2,0)

-- value and number of times it will occur 
rollPart2 :: [(Int, Int)]
rollPart2 = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

data GameState = GameState {
    turn :: Int,
    pos :: Int,
    score :: Int,
    count :: Integer
} deriving (Show)

--round pos score count
playRoundPart2 :: [GameState] -> [GameState]
playRoundPart2 = concatMap (\part ->
                        map (\(roll,times) ->
                                let pos' = takeTurn roll (pos part) in
                                GameState {
                                    turn = 1 + turn part,
                                    pos = pos',
                                    score = score part + 1 + pos',
                                    count = count part * toInteger times
                                }
                        ) rollPart2
                    )

data GameOutcome = GameOutcome {
    iter :: Int,
    totalPlays :: Integer,
    totalComplete :: Integer
} deriving (Show)

allOutcomes :: [GameState] -> [GameOutcome] -> [GameOutcome]
allOutcomes [] outcomes = reverse outcomes
allOutcomes incomplete outcomes = allOutcomes incomplete' (outcome:outcomes)
    where
        nextStates = playRoundPart2 incomplete
        total = sum (map count nextStates)
        (complete, incomplete') = partition (\state -> score state >=21 ) nextStates
        completeTotal = sum (map count complete)
        outcome = GameOutcome { iter = turn (head nextStates), totalPlays = total, totalComplete = completeTotal }
 
outcomesForPosition :: Int -> [GameOutcome]
outcomesForPosition startPos = allOutcomes [GameState{ turn = 0, pos = startPos, score = 0 , count = 1 }] []

part2 :: Integer
part2 = maximum [winsP1, winsP2]
    where
        player1 = outcomesForPosition 8
        player2 = outcomesForPosition 2
        rounds = length player1
        (winsP1, winsP2) = foldl (\acc round -> 
                          let 
                              lastRoundPlayer2 = player2 !! (round - 1)
                              currRoundPlayer1 = player1 !! round
                              currRoundPlayer2 = player2 !! round
                          in
                          -- wins is total of complete for the current round * total of incomplets for player 2 for the previous round
                          (fst acc + (totalComplete currRoundPlayer1 * (totalPlays lastRoundPlayer2 - totalComplete lastRoundPlayer2)),
                          -- wins is total of complete for player 2 for the current round * total of incompletea for player 1 for the current round
                           snd acc + (totalComplete currRoundPlayer2 * (totalPlays currRoundPlayer1 - totalComplete currRoundPlayer1)))) (0,0) [1..(rounds-1)]