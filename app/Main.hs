module Main where

import Day25
import Utils(PuzzlePart)
import Text.Printf

main :: IO ()
main = do
  contents <- readFile ("inputs/day25.txt")    
  printf "Part 1 => %s\n" $ show (part1 (lines contents))
