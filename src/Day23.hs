{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day23(part1, part2) where
import Utils (PuzzlePart)
import Data.Maybe
import Data.Sequence (update, fromList)
import Data.Foldable (toList)
import Data.List
import Debug.Trace
import qualified Data.Set
import qualified Data.PQueue.Min
import Data.PQueue.Min (MinQueue)
data Amphipod = A | B | C | D deriving (Eq, Show)

data Room = Room Amphipod [Maybe Amphipod]
data Board = Board {
    hallway :: [Maybe Amphipod],
    rooms :: [Room]
} 
data Game = Game {
    board :: Board,
    gameMoves :: [Move],
    cost :: Int
} deriving (Show)
type Move = (Amphipod, Int, String)
roomPositions = [2,4,6,8]

instance Show Board where
    show b = concatMap (maybe "." show) (hallway b) ++ show (rooms b)

instance Show Room where
    show (Room a xs) = show (map (maybe "."  show) xs)

instance Ord Game where
    compare g1 g2 = compare (cost g1) (cost g2)

instance Eq Game where
    (==) g1 g2 = show (board g1) == show (board g2) && cost g1 == cost g2

isCompleteRoom :: Room -> Bool
isCompleteRoom (Room x xs) = all (\y -> y == Just x) xs
 
hasCandidatesToMoveInRoom :: Room -> Bool
hasCandidatesToMoveInRoom (Room x contents) = any (\case 
                                                    Just y -> y/=x 
                                                    _ -> False) contents

canMoveToHallwayPositionFromRoom :: Board -> Int -> Int -> Bool
canMoveToHallwayPositionFromRoom board pos roomId
  -- can't move to position outside a room
  | pos `elem` roomPositions = False
  -- make sure route is clear
  | pos < roomPos = all isNothing ( drop pos $ take (roomPos+1) hall)
  -- make sure route is clear
  | pos > roomPos = all isNothing ( drop roomPos $ take (pos+1) hall)
  | otherwise = False
  where
      roomPos = roomPositions !! roomId
      hall = hallway board

isCompleteGame :: Game -> Bool
isCompleteGame game = all isCompleteRoom (rooms (board game))

-- from room to hallway position
nextRoomMoves :: Board -> [(Int, Int)]
nextRoomMoves board =
    -- try and update any rooms which are not complete
    [ (roomId, hallwayPosition)
      | hallwayPosition <- [0..10],
        roomId <- [0..3],
        let room =candidateRooms !! roomId,
        hasCandidatesToMoveInRoom room,
        canMoveToHallwayPositionFromRoom board hallwayPosition roomId ]
    where
        candidateRooms = rooms board


canMoveToRoom :: Room -> Amphipod -> Bool
canMoveToRoom (Room x contents) a = x==a && all (\e -> isNothing e || fromJust e == x) contents

-- make sure route is clear to room
canMoveToRoomFromHallwayPosition :: Board -> Int -> Int -> Bool
canMoveToRoomFromHallwayPosition board hallwayPos roomId
  | hallwayPos < roomPos = all isNothing (drop (hallwayPos+1) $ take roomPos hall)
  | hallwayPos > roomPos = all isNothing (drop roomPos $ take hallwayPos hall)
  | otherwise = False
  where
      hall = hallway board
      roomPos = roomPositions !! roomId

-- from hallway position to room
nextHallwayMoves :: Board -> [(Int, Int)]
nextHallwayMoves board =
    -- try and update any item in the hallway
    [ (hallwayPosition, roomId)
      | hallwayPosition <- filter (\x -> isJust (hallway' !! x)) [0..10],
        roomId <- [0..3],
        let room = rooms' !! roomId,
        let  Just amphipod = hallway' !! hallwayPosition,
        canMoveToRoom room amphipod,
        canMoveToRoomFromHallwayPosition board hallwayPosition roomId
    ]
    where
        hallway' = hallway board
        rooms' = rooms board

moveCost :: Move -> Int
moveCost (A, d, _) = d
moveCost (B, d, _) = d * 10
moveCost (C, d, _) = d * 100
moveCost (D, d, _) = d * 1000

moveFromHallwayToRoom :: Game -> Int -> Int -> Game 
moveFromHallwayToRoom game pos roomId =
    let
        rooms' = rooms (board game)
        hallway' = hallway (board game)
        Just a = hallway' !! pos
        distance = abs(2*(roomId+1)-pos)
        Room x contents = rooms' !! roomId
        (room', d, m) =  case Data.List.elemIndex Nothing (reverse contents) of
                        Just n -> 
                            let 
                                roomPos = (length contents - n - 1) 
                            in
                            ( Room x (take roomPos contents ++ [Just a] ++ drop (roomPos+1) contents), 
                             1 + distance + roomPos, 
                             "Hall " ++ show pos ++ " to room " ++ show roomId ++ " pos " ++ show roomPos)
                        Nothing -> error ( "Room already full! - " ++ show game)
    in
        Game {
            board = Board {
                hallway = toList $ update pos Nothing $ fromList hallway',
                rooms = toList $ update roomId room' $ fromList rooms'
            },
            gameMoves = (a, d, m):gameMoves game,
            cost = cost game + moveCost (a, d, m)
        }

moveFromRoomToHallway :: Game -> Int -> Int -> Game
moveFromRoomToHallway game roomId pos =
    let
        rooms' = rooms (board game)
        hallway' = hallway (board game)
        distance = abs(2*(roomId+1) - pos)
        Room x contents = rooms' !! roomId        
        (room', a, d, m) = 
            let 
                emptySlots = takeWhile isNothing contents
                entries = dropWhile isNothing contents  
                roomPos = length emptySlots
            in
            ( Room x (emptySlots ++ [Nothing] ++ tail entries), 
              fromJust (head entries), 
              distance + roomPos + 1,
              "Room " ++ show roomId ++ " pos " ++ show roomPos ++ " to hallway pos " ++ show pos)
    in
        Game {
            board = Board {
                hallway = toList $ update pos (Just a) $ fromList hallway',
                rooms = toList $ update roomId room' $ fromList rooms'
            },
            gameMoves = (a, d, m):gameMoves game,
            cost = cost game + moveCost (a, d, m)
        }

updateGame :: Game -> [Game]
updateGame game = hallwayChangeGames ++ roomChangeGames
    where
        board' = board game
        rooms' = rooms board'
        hallway' = hallway board'
        hallwayMoves = nextHallwayMoves board'
        hallwayChangeGames =
            map (uncurry (moveFromHallwayToRoom game)) hallwayMoves
        roomMoves = nextRoomMoves board'
        roomChangeGames =
           map (uncurry (moveFromRoomToHallway game)) roomMoves

findBestGame :: Game -> Data.Set.Set String -> MinQueue Game -> Game
findBestGame game visited priorityQueue
    | isCompleteGame game = game
    | Data.Set.member key visited =
        findBestGame (head (Data.PQueue.Min.take 1 priorityQueue)) visited (Data.PQueue.Min.drop 1 priorityQueue)
    | otherwise =
        let 
            visited' =  Data.Set.insert key visited 
            games = updateGame game
            (visited'', newGames) = foldl (\(vs, gs) g -> 
                                                let key' = show (board g) in
                                                    if not (Data.Set.member key' vs) then
                                                        (Data.Set.insert key' vs, g:gs)
                                                    else
                                                        (vs, gs)
                                                ) (visited',[]) $ sort games
            priorityQueue' = foldr Data.PQueue.Min.insert priorityQueue newGames
        in
            findBestGame (head (Data.PQueue.Min.take 1 priorityQueue')) visited' (Data.PQueue.Min.drop 1 priorityQueue')
    where
        key = show (board game)
        

example :: Board
example = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A [Just B, Just A],
        Room B [Just C, Just D],
        Room C [Just B, Just C],
        Room D [Just D, Just A]
    ]
}

example2 :: Board
example2 = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A [Just B, Just D, Just D, Just A],
        Room B [Just C, Just C, Just B, Just D],
        Room C [Just B, Just B, Just A, Just C],
        Room D [Just D, Just A, Just C, Just A]
    ]
}

puzzle :: Board
puzzle = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A [Just B, Just C],
        Room B [Just C, Just D],
        Room C [Just A, Just D],
        Room D [Just B, Just A]
    ]
}

puzzle2 :: Board
puzzle2 = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A [Just B, Just D, Just D, Just C],
        Room B [Just C, Just C, Just B, Just D],
        Room C [Just A, Just B, Just A, Just D],
        Room D [Just B, Just A, Just C, Just A]
    ]
}

solve :: Board -> Game 
solve b = findBestGame game Data.Set.empty Data.PQueue.Min.empty 
    where
        game = Game { board = b, gameMoves = [], cost = 0}

part1 :: Int
part1 = --trace (show result) 
        cost (solve puzzle)

part2 :: Int
part2 = --trace (show result) 
        cost (solve puzzle2)