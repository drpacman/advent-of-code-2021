{-# LANGUAGE TupleSections #-}
module Day23(part1, Board, Room, canMoveToHallwayPositionFromRoom, nextHallwayMoves, moveFromRoomToHallway) where
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

data Room = Room Amphipod (Maybe Amphipod, Maybe Amphipod) 
data Board = Board {
    hallway :: [Maybe Amphipod],
    rooms :: [Room]
} 
data Game = Game {
    board :: Board,
    gameMoves :: [Move],
    cost :: Int
} deriving (Show)
type Move = (Amphipod, Int)
roomPositions = [2,4,6,8]

instance Show Board where
    show b = concatMap (maybe "." show) (hallway b) ++ show (rooms b)

instance Show Room where
    show (Room a (x, y)) = "[" ++ maybe "."  show x ++ maybe "." show y ++ "]"

instance Ord Game where
    compare g1 g2 = compare (cost g1) (cost g2)

instance Eq Game where
    (==) g1 g2 = show (board g1) == show (board g2) && cost g1 == cost g2

isCompleteRoom :: Room -> Bool
isCompleteRoom (Room x (Just y, Just z))
 | x==y && x==z = True
 | otherwise = False
isCompleteRoom (Room _ (_, _)) = False

hasCandidatesToMoveInRoom :: Room -> Bool
hasCandidatesToMoveInRoom  room = 
    case contents of
        (Nothing, Nothing) -> False 
        (Just y, Just z) | x==y && x==z -> False
        (Nothing, Just z) | x == z -> False 
        _ -> True
    where 
        Room x contents =room

canMoveToRoom :: Room -> Amphipod -> Bool
canMoveToRoom (Room x (Just y, Just z)) a = False
canMoveToRoom (Room x (Nothing, Just y)) a
  | x==a && y == a = True
  | otherwise = False
canMoveToRoom (Room x (Nothing, Nothing)) a
  | x==a = True
  | otherwise = False

-- make sure route is clear to room
canMoveToRoomFromHallwayPosition :: Board -> Int -> Int -> Bool
canMoveToRoomFromHallwayPosition board hallwayPos roomId
  | hallwayPos < roomPos = all isNothing (drop (hallwayPos+1) $ take roomPos hall)
  | hallwayPos > roomPos = all isNothing (drop roomPos $ take hallwayPos hall)
  | otherwise = False
  where
      hall = hallway board
      roomPos = roomPositions !! roomId

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

moveCost :: (Amphipod, Int) -> Int
moveCost (A, d) = d
moveCost (B, d) = d * 10
moveCost (C, d) = d * 100
moveCost (D, d) = d * 1000

moveFromHallwayToRoom :: Game -> Int -> Int -> Game 
moveFromHallwayToRoom game pos roomId =
    let
        rooms' = rooms (board game)
        hallway' = hallway (board game)
        Just a = hallway' !! pos
        distance = 1 + abs(2*(roomId+1)-pos)
        (room', d) =
            case rooms' !! roomId of
                Room x (Nothing, Nothing) -> (Room x (Nothing, Just a), distance + 1)
                Room x (Nothing, Just y) -> (Room x (Just a, Just y), distance)
                _ -> error ( "Room already full! - " ++ show game)
    in
        Game {
            board = Board {
                hallway = toList $ update pos Nothing $ fromList hallway',
                rooms = toList $ update roomId room' $ fromList rooms'
            },
            gameMoves = (a, d):(gameMoves game),
            cost = (cost game) + moveCost (a, d)
        }

moveFromRoomToHallway :: Game -> Int -> Int -> Game
moveFromRoomToHallway game roomId pos =
    let
        rooms' = rooms (board game)
        hallway' = hallway (board game)
        distance = 1 + abs(2*(roomId+1) - pos)
        (room', a, d) =
            case rooms' !! roomId of
                Room x (Just a, Just b) -> (Room x (Nothing, Just b), a, distance)
                Room x (Nothing, Just a) -> (Room x (Nothing, Nothing), a, distance + 1)
                _ -> error ("Room empty! " ++ show game)
    in
        Game {
            board = Board {
                hallway = toList $ update pos (Just a) $ fromList hallway',
                rooms = toList $ update roomId room' $ fromList rooms'
            },
            gameMoves = (a, d):gameMoves game,
            cost = (cost game) + moveCost (a, d)
        }

updateGame :: Game -> [Game]
updateGame game = if null hallwayChangeGames then roomChangeGames else hallwayChangeGames
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
findBestGame game visited priorityQueue =
    if isCompleteGame game then
        game
    else
        findBestGame (head (Data.PQueue.Min.take 1 priorityQueue')) visited' (Data.PQueue.Min.drop 1 priorityQueue')
    where
        visited' =  Data.Set.insert (show (board game)) visited 
        games = updateGame game
        (visited'', newGames) = foldl (\(vs, gs) g -> 
                                            let key = show (board g) in
                                                if not (Data.Set.member key vs) then
                                                    (Data.Set.insert key vs, g:gs)
                                                else
                                                    (vs, gs)
                                            ) (visited',[]) $ sort games
        priorityQueue' = foldl (\q g -> Data.PQueue.Min.insert g q) priorityQueue newGames

example :: Board
example = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A (Just B, Just A),
        Room B (Just C, Just D),
        Room C (Just B, Just C),
        Room D (Just D, Just A)
    ]
}

puzzle :: Board
puzzle = Board {
    hallway = replicate 11 Nothing,
    rooms = [
        Room A (Just B, Just C),
        Room B (Just C, Just D),
        Room C (Just A, Just D),
        Room D (Just B, Just A)
    ]
}

part1 :: Int
part1 = trace (show result) cost result
    where
        b = puzzle
        game = Game {
            board = b,
            gameMoves = [],
            cost = 0
        }
        result = findBestGame game Data.Set.empty Data.PQueue.Min.empty 