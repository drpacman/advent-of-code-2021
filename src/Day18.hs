module Day18(part1, part2) where
import Utils (PuzzlePart)

import Data.Char
import Data.List (partition)
import Debug.Trace
import Data.Maybe

data Elem = Value Int | Pair Elem Elem deriving (Eq)

instance Show Elem where
    show (Value i) = show i
    show (Pair e1 e2) = "[" ++ show e1 ++ "," ++ show e2 ++ "]"

toElems :: [String] -> [Elem]
toElems input = map (\xs -> fst (toElem xs)) input

toElem :: String -> (Elem, String)
toElem input =
    case head input of
        '[' -> 
            let 
                (e1, rest) = toElem (tail input)
                -- skip the comma and parse second element
                (e2, rest') = toElem (dropWhile (\x -> x == ']' || x == ',') rest)
            in
                (Pair e1 e2, rest')
        _ -> 
            let 
                (ds, rest) = span isDigit input
            in
                (Value (read ds), rest)


type Explosion = (Maybe Int, Maybe Int)
appliedExplosion = Just(Nothing, Nothing)

updateLeft :: Elem -> Int -> Elem 
updateLeft (Pair p1 p2) n = Pair (updateLeft p1 n) p2
updateLeft (Value v) n = Value (v+n)

updateRight :: Elem -> Int -> Elem 
updateRight (Pair p1 p2) n = Pair p1 (updateRight p2 n)
updateRight (Value v) n = Value (v+n)

explode :: Elem -> Int -> (Elem, Maybe Explosion)
explode pair depth =
    case pair of
        Value v -> (Value v, Nothing)
        Pair (Pair (Value l) (Value r)) (Value v) | depth == 3 -> (Pair (Value 0) (Value (v+r)), Just (Just l, Nothing))
        Pair (Pair (Value l) (Value r)) (Pair (Value l2) (Value r2)) | depth == 3 -> (Pair (Value 0) (Pair (Value (l2+r)) (Value r2)), Just (Just l, Nothing))
        Pair (Value v) (Pair (Value l) (Value r)) | depth == 3 -> (Pair (Value (v+l)) (Value 0), Just (Nothing, Just r))
        Pair l r ->
            let 
                (l', el) = explode l (depth+1)
            in
                case el of
                    -- apply right hand value to the leftmost node of the right hand tree
                    Just (Nothing, Just v) -> (Pair l' (updateLeft r v), appliedExplosion)
                    -- we are already on the left, let the explosion bubble up
                    Just (Just v, Nothing) -> (Pair l' r, el)                                
                    -- already exploded, do nothing
                    Just x -> (Pair l' r, appliedExplosion)
                    Nothing ->
                        let 
                            (r', el) =  explode r (depth+1)
                        in
                            case el of 
                                -- apply right hand value to the rightmost node of the left hand tree
                                Just (Just v, Nothing) -> ((Pair (updateRight l v) r'), appliedExplosion)
                                -- we are already on the right, let the explosion bubble up
                                Just (Nothing, Just v) -> (Pair l r', el)
                                -- already exploded, do nothing
                                Just x -> (Pair l r', appliedExplosion)
                                Nothing -> (Pair l r, Nothing)

split :: Elem -> Elem 
split (Pair e1 e2) = if s1 == e1 then Pair e1 (split e2) else Pair s1 e2
    where 
        s1 = split e1
split (Value elem) = if elem >= 10 then 
                            if even elem 
                            then 
                                Pair (Value (div elem 2)) (Value (div elem 2))
                            else
                                Pair (Value (div elem 2)) (Value (1+(div elem 2)))
                        else 
                            (Value elem)

process :: Elem -> Elem
process elem = if isJust explosion then
                    process elem'
               else 
                   let 
                       elem'' = split elem'
                    in
                        if elem'' == elem'
                            then
                                elem'
                            else
                                trace (show "Split") process elem''    
    where
        (elem', explosion) = explode elem 0

magnitude :: Elem -> Int 
magnitude (Value e) = e
magnitude (Pair e1 e2) = (3 * magnitude e1) + (2 * magnitude e2)

part1 :: PuzzlePart Int 
part1 input = trace (show result) magnitude result
    where 
        elems = toElems input
        result = foldl (\acc p -> process (Pair acc p)) (head elems) (tail elems)

allPairs :: [Elem] -> [Elem] -> [Elem]
allPairs [e] pairs = pairs
allPairs elems pairs = allPairs (tail elems) (newPairs ++ pairs) 
    where 
        p = head elems
        newPairs = concatMap (\e -> [process (Pair p e), process (Pair e p)]) (tail elems)

part2 :: PuzzlePart Int 
part2 input = trace (show result) result
    where 
        elems = toElems input
        pairs = allPairs elems []
        result = maximum $ map magnitude pairs
