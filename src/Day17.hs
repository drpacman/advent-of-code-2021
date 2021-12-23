module Day17(part1, part2) where
import Utils (PuzzlePart)
import Debug.Trace

type Target = (Int, Int, Int, Int)

hitsTarget :: Target -> (Int, Int) -> (Int,Int) -> Bool 
hitsTarget (xmin, xmax, ymin, ymax) (x, dx) (y, dy) 
  | y < ymin || x > xmax = False 
  | x >= xmin && x <= xmax && y >= ymin && y <=ymax = True
  | otherwise = 
      hitsTarget (xmin, xmax, ymin, ymax) (x+dx, dx') (y+dy, dy')      
        where
            dx' = if dx > 0 then dx - 1 else if dx < 0 then dx + 1 else 0
            dy' = dy - 1

sumValues :: Int -> Int -> Int 
sumValues 0 total = 0
sumValues n total = sumValues (n-1) total+n

part1 :: Target -> Int 
part1 target = trace (show (length experiments)) sumValues maxY 0
    where 
        (_, xmax, ymin, _) = target
        experiments = [(dx,dy) | dy <- [ymin..(abs ymin)], dx <- [1..xmax], hitsTarget target (0,dx) (0,dy)]
        maxY = maximum (map snd experiments)

part2 :: Target -> Int 
part2 target = length experiments
    where 
        (_, xmax, ymin, _) = target
        experiments = [(dx,dy) | dy <- [ymin..(abs ymin)], dx <- [0..xmax], hitsTarget target (0,dx) (0,dy)]
