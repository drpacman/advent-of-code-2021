module Main where

import qualified Day1 (parts)
import Utils(PuzzlePart)
import Text.Printf

run :: (Show a) => String -> Int -> PuzzlePart a -> IO ()
run filename n part =  
  do
    contents <- readFile ("inputs/" ++ filename)    
    printf "Part %d => %s\n" n $ show (part (lines contents))

main :: IO ()
main = do
  run "day1.txt" 1 part1
  run "day1.txt" 2 part2
  where (part1, part2) = Day1.parts
