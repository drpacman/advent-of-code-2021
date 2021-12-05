module Main where

import Day5
import Utils(PuzzlePart)
import Text.Printf

main :: IO ()
main = do
  contents <- readFile ("inputs/day5.txt")    
  printf "Part 1 => %s\n" $ show (part1 (lines contents))
  printf "Part 2 => %s\n" $ show (part2 (lines contents))
