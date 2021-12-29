module Day19(part1, part2) where
import Utils
import Data.List
import Data.Maybe
import Data.List.Split
import Debug.Trace
import qualified Data.Map
import qualified Control.Applicative as Map

type Coord = (Int, Int, Int)

rotations :: [Coord -> Coord ]
rotations = [\(x,y,z) -> (x,y,z),
             \(x,y,z) -> (-x,y,z),
             \(x,y,z) -> (x,-y,z),
             \(x,y,z) -> (x,y,-z),
             \(x,y,z) -> (-x,-y,z),
             \(x,y,z) -> (-x,y,-z),
             \(x,y,z) -> (x,-y,-z),
             \(x,y,z) -> (-x,-y,-z)]

variations :: [Coord -> Coord]
variations = [\(x,y,z) -> (x,y,z),
              \(x,y,z) -> (x,z,y),
              \(x,y,z) -> (y,x,z),
              \(x,y,z) -> (y,z,x),
              \(x,y,z) -> (z,x,y),
              \(x,y,z) -> (z,y,x)]

transforms :: [Coord -> Coord] 
transforms = [ v.r | v <- variations, r <- rotations ]

-- shift co-ordinates to be relative to a specific beacon
shiftToOffset :: Coord -> [Coord] -> [Coord]
shiftToOffset (x,y,z) coords = map (\(x1, y1, z1) -> (x - x1, y - y1, z - z1)) coords


toScans :: [String] -> [Coord] -> [[Coord]] -> [[Coord]]
toScans [] cs scans = reverse ((reverse cs):scans)
toScans input cs scans = 
    if null (head input) then
        -- start next scan
        toScans (drop 2 input) [] ((reverse cs):scans)
    else 
        -- add next coord to current scan
        toScans (tail input) ((toCoord (head input)):cs) scans

toCoord :: String -> Coord
toCoord coords = (read x, read y, read z)
    where
        (x,y,z) = case wordsBy (==',') coords of 
                        (x:y:z:[]) -> (x,y,z)
                        w -> error (show w)

-- shift co-ordinates to be relative to a specific beacon for both the target (base) and the candidate
-- if any combination has 12 or more of the same entries we have found a match
match :: [Coord] -> [Coord] -> Maybe (Coord, Coord)
match candidate base = 
    if length matchedBeacons > 0 
        then
            head matchedBeacons
        else 
            Nothing  
    where
        shiftedCandidatesWithBeacon = map (\beacon -> (beacon, shiftToOffset beacon candidate)) candidate
        shiftedBasesWithBeacon = map (\beacon -> (beacon, shiftToOffset beacon base)) base
        beaconWithIntersections = map (\(candidateBeacon, c) -> 
                            map (\(baseBeacon, b) -> 
                                ((baseBeacon, candidateBeacon), Data.List.intersect b c)
                            ) shiftedBasesWithBeacon
                        ) shiftedCandidatesWithBeacon
        matchedBeacons = filter isJust $ map (\xs -> 
                            let 
                                matchingBeacons = find (\((b1,b2),shared)-> (length shared) >= 12) xs 
                            in
                                case matchingBeacons of
                                    Just ((b1,b2), _) ->
                                        Just (b1,b2)
                                    Nothing ->
                                        Nothing
                  ) beaconWithIntersections 
        

findBeacons :: [Coord] -> [Coord] -> Maybe ([Coord], Coord)
findBeacons candidate base = if null ts then Nothing else head ts
    where 
        ts = filter isJust $ map (\transform -> 
                    case match (map transform candidate) base of
                        Just (baseBeacon, candidateBeacon) ->
                            let 
                                candidateBeacons = map transform candidate
                                (x1,y1,z1) = baseBeacon
                                (x2,y2,z2) = candidateBeacon
                                (offsetX, offsetY, offsetZ) = (x2-x1, y2-y1, z2-z1)
                                shiftedBeacons = map (\(x, y, z) -> (x - offsetX, y - offsetY, z - offsetZ)) candidateBeacons
                            in
                                --trace ("base: " ++ show candidate ++ " - Shifted - " ++ show shiftedBeacons) 
                                Just (shiftedBeacons, (offsetX, offsetY, offsetZ))
                        Nothing ->
                            Nothing
                ) transforms

resolveScans :: [[Coord]] -> [[Coord]] -> [([Coord], Coord)] -> ([Coord], [Coord])
resolveScans [] _ beaconsWithOffsets = 
    let
        beacons = foldl union [] (map fst beaconsWithOffsets)
        offsets = foldl (\acc (_, offset) -> offset:acc ) [] beaconsWithOffsets
    in
        (beacons, offsets)
resolveScans unmatched targets beaconsWithOffsets =
    case trace ("Finding beacons " ++ show (length unmatched)) findBeacons candidate target of
        Just (bs, offset) ->
            let 
                beaconsWithOffsets' = (bs,offset):beaconsWithOffsets 
                targets = map fst beaconsWithOffsets'
            in
            resolveScans (tail unmatched) targets beaconsWithOffsets'
        Nothing ->
            if length targets == 1 then
                let 
                    -- rotate the scans as we search for the next match
                    unmatched' = tail unmatched ++ [candidate]
                    targets = map fst beaconsWithOffsets
                in
                --trace "Rotated scan"
                resolveScans unmatched' targets beaconsWithOffsets 
            else
                --trace "Try next target set of beacons"
                resolveScans unmatched (tail targets) beaconsWithOffsets 
    where
        candidate = head unmatched
        target = head targets   

part1 :: PuzzlePart Int
part1 input = trace (show beacons) length beacons
    where
        scans = toScans (tail input) [] []
        scan0 = head scans
        (beacons, _) = resolveScans ((tail scans)) [scan0] [(scan0, (0,0,0))]

part2 :: PuzzlePart Int
part2 input = maximum differences
    where
        scans = toScans (tail input) [] []
        scan0 = head scans
        (_, offsets) = resolveScans ((tail scans)) [scan0] [(scan0, (0,0,0))]
        scannerPairs = [ (a,b) | a <- offsets, b <- offsets ]
        differences = map (\((x1,y1,z1),(x2,y2,z2)) -> (abs(x1-x2) + abs(y1-y2) + abs(z1-z2))) scannerPairs
