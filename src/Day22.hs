{-# LANGUAGE TupleSections #-}
module Day22(part1, part2) where
import Utils (PuzzlePart)
import Data.List.Split 
import Debug.Trace
import Text.Regex.Posix
import Data.Maybe
import Data.List (groupBy, sortOn)

newtype Cuboid = Cuboid ((Int, Int, Int), (Int, Int, Int)) deriving (Show, Eq, Ord)

toCuboid :: String -> (Bool, Cuboid)
toCuboid line =
    (value == "on", Cuboid((read xmin, read ymin, read zmin),(read xmax,read ymax,read zmax)))
    where
        pat = "(on|off) x=([-,0-9]*)..([-,0-9]*),y=([-,0-9]*)..([-,0-9]*),z=([-,0-9]*)..([-,0-9]*)"
        (_,_,_,[value, xmin, xmax, ymin, ymax, zmin, zmax]) = line =~ pat :: (String, String, String, [String])

size :: Cuboid -> Int
size cube  = (1 + abs (xmax - xmin)) * (1 + abs (ymax - ymin)) * (1 + abs (zmax - zmin))
    where
        Cuboid((xmin, ymin, zmin), (xmax, ymax, zmax)) = cube

intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect cubeA cubeB =
     if 
        ((axMax >= bxMin && axMin <= bxMax) &&
        (ayMax >= byMin && ayMin <= byMax) &&
        (azMax >= bzMin && azMin <= bzMax))
        ||
        ((bxMax >= axMin && bxMin <= axMax) &&
         (byMax >= ayMin && byMin <= ayMax) &&
         (bzMax >= azMin && bzMin <= azMax))        
        then
            Just (Cuboid ((maximum [axMin, bxMin], maximum [ayMin, byMin], maximum [azMin, bzMin]),
                         (minimum [axMax, bxMax], minimum [ayMax, byMax], minimum [azMax, bzMax])))
        else
            Nothing
    where
        Cuboid ((axMin,ayMin,azMin), (axMax, ayMax, azMax)) = cubeA
        Cuboid ((bxMin,byMin,bzMin), (bxMax, byMax, bzMax)) = cubeB

intersectionSize :: (Bool, Cuboid) -> Int
intersectionSize (isOn, cube) =
    sign * size cube
    where
        sign = if isOn then 1 else -1

updateIntersection :: Bool -> Bool -> Bool
updateIntersection True True = False 
updateIntersection False True = False
updateIntersection True False = True
updateIntersection False False = True

compact :: [(Bool, Cuboid)] -> [(Bool, Cuboid)]
compact entries = concatMap (\group -> foldl (\acc entry -> 
                                         if null acc then
                                             [entry]
                                         else 
                                             let 
                                                (b1, i1) = head acc
                                                (b2, i2) = entry
                                            in
                                                if b1 /= b2 then tail acc else entry:acc
                                        ) [head group] (tail group)
                        ) groups
    where
        groups = groupBy (\a b -> snd a == snd b) $ sortOn snd entries


-- does curent cube intersect with any existing cubes?
-- if on the total is current + cube - intersect (current, cube) 
-- if off the total is current - (intersect (current, cube))
countOn :: [(Bool, Cuboid)] -> [Cuboid] -> [(Bool, Cuboid)] -> Int
countOn [] adds intersects =    
    sumOfAdds + sumOfIntersections
    where sumOfAdds = sum (map size adds)
          sumOfIntersections = sum (map intersectionSize intersects)
countOn instructions adds intersects
    | isOn = countOn (tail instructions) (cube:adds) intersects'
    | otherwise = countOn (tail instructions) adds intersects'
    where
        (isOn, cube) = head instructions
        newIntersects = map (False,) $ mapMaybe (intersect cube) adds
        adjustments = mapMaybe (\(isIntersectionOn, cuboid) ->
                                    case cube `intersect` cuboid of
                                        Just intersection -> Just (updateIntersection isOn isIntersectionOn, intersection)
                                        _ -> Nothing
                                ) intersects
        intersects' =  compact (intersects ++ newIntersects ++ adjustments)

part1 :: PuzzlePart Int
part1 input = countOn filteredCubes [] []
    where
        cubes = map toCuboid input
        filteredCubes = filter (\(_, c) -> 
                                    let 
                                        Cuboid((xmin, ymin,zmin), (xmax,ymax,zmax))=c 
                                    in
                                        all (>=(-50)) [xmin, ymin, zmin] && all (<=50) [xmax,ymax,zmax]) cubes

part2 :: PuzzlePart Int
part2 input = countOn cubes [] []
    where
        cubes = map toCuboid input