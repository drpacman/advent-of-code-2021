module Main where

import Day9
import Utils(PuzzlePart)
import Text.Printf

main :: IO ()
main = do
  contents <- readFile ("inputs/day9.txt")    
  printf "Part 1 => %s\n" $ show (part1 (lines contents))
  printf "Part 2 => %s\n" $ show (part2 (lines contents))
