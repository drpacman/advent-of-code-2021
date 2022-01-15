module Day24(part1, part2) where
import Utils
import Data.Char
import Data.Map
import Data.List
import Text.Regex.Posix
import Debug.Trace
import GHC.IO.Handle (NewlineMode(inputNL))

registerIndex :: Char -> Int 
registerIndex c = Data.Char.ord c - Data.Char.ord 'w'

applyUpdate :: Char -> [Char] -> (Int -> Int -> Int) -> (Machine -> Machine)
applyUpdate a b f (reg,io) = 
    let
        r1 = reg ! a
        r2 = if head b `elem` ['w', 'x','y','z'] then reg ! head b else read b
    in
        (Data.Map.insert a (f r1 r2) reg, io)

applyInput :: Char -> (Machine -> Machine)
applyInput a (reg,io) = trace ("w" ++ show(14-length io) ++ " - " ++ show (reg,io)) (Data.Map.insert a (head io) reg, tail io)

type Machine = (Map Char Int, [Int])
type Instruction =  Machine -> Machine

parseInstruction :: String -> Instruction
parseInstruction instr = 
    case ins of
        ("inp":[a]:_) -> applyInput a
        ["add",[a],_,b] -> applyUpdate a b (+)
        ["mul",[a],_,b] -> applyUpdate a b (*)
        ["div",[a],_,b] -> applyUpdate a b div
        ["mod",[a],_,b] -> applyUpdate a b mod
        ["eql",[a],_,b] -> applyUpdate a b (\x y -> if x == y then 1 else 0)
        _ -> error ("Unexpected entry " ++ show instr)
    where
      (_,_,_,ins) = instr =~ "(inp|add|mul|div|mod|eql) ([wxyz])( ([0-9wxyz\\-]+))?" :: (String, String, String, [String])

isValid :: Machine -> Bool 
isValid (reg, _) = 0 == reg ! 'z' 

parse :: [String] -> [Instruction]
parse = Data.List.map parseInstruction

newMachine :: [Int] -> Machine 
newMachine value = (fromList [('w',0), ('x',0), ('y',0), ('z',0)], value ) 

run :: Machine -> [Instruction] -> Machine
run machine program = Data.List.foldl (\m instr-> instr m) machine program 

findFirstValidInput :: [Instruction] -> [Int] -> [Int]
findFirstValidInput program input 
  | isValid result = input
  | otherwise = findFirstValidInput program (nextSmallest input)    
    where
    machine = newMachine input 
    result = run machine program

nextSmallest :: [Int] -> [Int]
nextSmallest input = fst $  Data.List.foldr 
                                (\i (v, carry) -> 
                                    if not carry then
                                        (i:v, False)
                                    else if 
                                        i == 1 then (9:v, True) 
                                    else (i-1:v, False)) ([], True 
                                ) input
-- w0 = 9
-- w5 = w4+4
-- w7 = w6+2
-- w8 = w3+8
-- w10 = w9
-- w11 = w8 + 1
-- w12 = w2 - 5
-- w13 = w1 + 6

-- w0=9
-- w1=2
-- w2=9
-- w3=1
-- w4=9
-- w5=9
-- w6=1
-- w7=w6+2=3
-- w8=w3+8=9
-- w9=1
-- w10=w9=1
-- w11=w8+1=???
-- w12 =w1+7=9
-- w13 =w1+6=8
part1 :: PuzzlePart Int
part1 input = trace (show machine) 0
    where 
        program = parse input
        machine = run (newMachine [9,2,9,1,5,9,7,9,9,9,9,4,9,8]) program 

part2 :: PuzzlePart Int
part2 input = trace (show machine) 0
    where 
        program = parse input
        machine = run (newMachine [2,1,6,1,1,5,1,3,9,1,1,1,8,1]) program
