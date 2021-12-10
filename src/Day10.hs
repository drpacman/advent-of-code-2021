module Day10(part1, part2) where
import Utils (PuzzlePart)
import Data.Either
import Debug.Trace
import Data.List

parse :: [Char] -> [Char] -> Either Char [Char]
parse [] chunks = Right chunks
parse (x:xs) chunks = 
    case x of '[' -> parse xs (x:chunks)
              ']' -> if head chunks /= '[' then trace (show chunks) Left x else parse xs (tail chunks)
              '(' -> parse xs (x:chunks)
              ')' -> if head chunks /= '(' then Left x else parse xs (tail chunks)
              '{' -> parse xs (x:chunks)
              '}' -> if head chunks /= '{' then Left x else parse xs (tail chunks)
              '<' -> parse xs (x:chunks)
              '>' -> if head chunks /= '<' then Left x else parse xs (tail chunks)
              _ -> parse xs chunks
              
score :: Either Char a -> Int
score (Left error) = 
    case error of 
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137
        _ -> 0

scoreChar :: Char -> Int
scoreChar c = 
    case c of 
        '(' -> 1
        '[' -> 2
        '{' -> 3
        '<' -> 4
        _ -> 0

scoreLine :: [Char] -> Int 
scoreLine line = foldl (\acc c -> acc*5 + scoreChar c) 0 line

part1 :: PuzzlePart Int 
part1 input = trace (show results) sum scores
    where
        results = map (\line -> trace (show "\nLine " ++ line) parse line []) input
        errors = filter isLeft results
        scores = map score errors

part2 :: PuzzlePart Int 
part2 input = trace (show scores) (sort scores) !! (middleScore - 1)
    where
        results = map (\line -> trace (show "\nLine " ++ line) parse line []) input
        entries = filter isRight results
        scores = map (\(Right remainder) -> scoreLine remainder) entries
        middleScore = div (1 + length scores) 2