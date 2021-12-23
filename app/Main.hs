module Main where

import Day17
import Utils(PuzzlePart)
import Text.Printf

main :: IO ()
main = do
  printf "Part 1 => %s\n" $ show (part1 (56,76,-162,-134))
  printf "Part 2 => %s\n" $ show (part2 (56,76,-162,-134))
