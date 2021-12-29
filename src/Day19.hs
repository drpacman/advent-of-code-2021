module Day19(part1) where
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
        

findBeacons :: [Coord] -> [Coord] -> Maybe (Coord, Coord -> Coord, [Coord])
findBeacons candidate base = if null ts then Nothing else head ts
    where 
        ts = filter isJust $ map (\transform -> 
                    case match (map transform candidate) base of
                        Just ((x1,y1,z1), (x2,y2,z2)) ->
                            let 
                                candidateBeacons = map transform candidate
                                offset = (x1-x2, y1-y2, z1-z2)
                            in
                                 Just (offset, transform, shiftToOffset offset candidateBeacons)
                        Nothing ->
                            Nothing
                ) transforms
-- walk scans
-- for current scan, find next match, transform and offset
-- apply transform to current transform, apply offset to current offset
-- find next matching scan from scans
resolveScans :: [[Coord]] -> [[Coord]] ->  Data.Map.Map [Coord] (Coord, (Coord->Coord)) -> [Coord] -> [Coord]
resolveScans [] _ scanMap beacons = --beacons
    Data.Map.foldrWithKey (\k (o,t) bs -> union bs (shiftToOffset o (map t k))) Map.empty scanMap
resolveScans unmatched targets scanMap beacons =
    case trace ("finding beacons " ++ show (length unmatched)) findBeacons candidate target of
        -- we found a match, get its offset, transform and list of beacons relative to the current scan
        Just (offset, transform, bs) ->
            let 
                -- transform matching scan to be same as scanner zero
                scan' = map transform candidate
                (x,y,z) = offset
                ((x0, y0, z0), t) = (scanMap Data.Map.! target)
                offsetFromZero' = (x+x0, y+y0, z+z0)
                scanMap' = Data.Map.insert scan' (offsetFromZero', transform) scanMap 
                -- transform beacons relative to scanner zero
                beaconsRelativeToZero = map transform bs
                beacons' = union beacons (shiftToOffset offsetFromZero' beaconsRelativeToZero)
            in
            trace ("Found beacon for " ++ show (head scan') ++ 
                   "--" ++ 
                   show (x,y,z) ++ 
                   " -- " ++ 
                   show (transform (1,2,3)) ++ 
                   " -- " ++ 
                   show offsetFromZero')
            resolveScans (tail unmatched) (Data.Map.keys scanMap) scanMap' beacons'
        Nothing ->
            if length targets == 1 then
                let 
                    -- rotate the scans as we search for the next match
                    unmatched' = tail unmatched ++ [candidate]
                in
                trace ("Rotated scan")
                resolveScans unmatched' (Data.Map.keys scanMap) scanMap beacons 
            else
                trace ("Try next target")
                resolveScans unmatched (tail targets) scanMap beacons 
    where
        candidate = head unmatched
        target = head targets   

part1 :: PuzzlePart Int
part1 input = trace (show beacons) length beacons
    where
        scans = toScans (tail input) [] []
        scan0 = (head scans)
        scanMap = Data.Map.fromList [( scan0, ((0,0,0), id) )]
        beacons = resolveScans (reverse (tail scans)) [scan0] scanMap []
