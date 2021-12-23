module Day16(part1, part2) where
import Utils (PuzzlePart)
import Debug.Trace
import Data.List
import GHC.Conc (pseq)

toDecimal :: [Int] -> Int
toDecimal bits = foldl (\acc x -> (2*acc)+x) 0 bits

toBinary :: [Char] -> [Int]
toBinary input = concatMap toBits input

toBits :: Char -> [Int]
toBits c 
 | c == '0' = [0,0,0,0]
 | c == '1' = [0,0,0,1]
 | c == '2' = [0,0,1,0]
 | c == '3' = [0,0,1,1]
 | c == '4' = [0,1,0,0]
 | c == '5' = [0,1,0,1]
 | c == '6' = [0,1,1,0]
 | c == '7' = [0,1,1,1]
 | c == '8' = [1,0,0,0]
 | c == '9' = [1,0,0,1]
 | c == 'A' = [1,0,1,0]
 | c == 'B' = [1,0,1,1]
 | c == 'C' = [1,1,0,0]
 | c == 'D' = [1,1,0,1]
 | c == 'E' = [1,1,1,0]
 | c == 'F' = [1,1,1,1]

readValue :: Int -> [Int] -> (Int, [Int])
readValue bitCount bits = (toDecimal (take bitCount bits), drop bitCount bits)

getVersion :: [Int] -> (Int, [Int])
getVersion = readValue 3

getTypeId :: [Int] -> (Int, [Int])
getTypeId = readValue 3

getNumber :: [Int] -> [Int] -> (Int, [Int])
getNumber (1:bits) result = getNumber (drop 4 bits) (result ++ (take 4 bits))
getNumber (0:bits) result = (toDecimal (result ++ (take 4 bits)), drop 4 bits)

getSubPacketsLength :: [Int] -> (Int, [Int])
getSubPacketsLength = readValue 15

getSubPacketCount :: [Int] -> (Int, [Int])
getSubPacketCount = readValue 11

data Packet = Number Int Int | Packet Int Int [Packet]

parsePacket :: [Int] -> (Packet, [Int])
parsePacket bits = 
    let 
        (version, bits') = getVersion bits
        (typeId, bits'') = getTypeId bits'
    in
        case typeId of
            4 -> let (n,p) = getNumber bits'' [] in (Number version n, p)
            _ -> if head bits'' == 0
                    then
                        let 
                            (len, bits''') = getSubPacketsLength (tail bits'')
                            childPackets = Data.List.unfoldr (\bs -> 
                                if bs == [] then
                                    Nothing 
                                else
                                    Just (parsePacket bs)
                                ) (take len bits''')
                        in
                            (Packet version typeId childPackets, drop len bits''')
                    else
                        let 
                            (count, bits''') = getSubPacketCount (tail bits'')
                            (childPackets, bits'''') = parsePackets bits''' count [] 
                        in
                            (Packet version typeId childPackets, bits'''')

parsePackets :: [Int] -> Int -> [Packet] -> ([Packet], [Int])
parsePackets bits 0 packets = (reverse packets, bits)
parsePackets bits count packets = parsePackets bits' (count - 1) (packet:packets)
                                    where 
                                        (packet, bits') = parsePacket bits

score :: Packet -> Int -> Int 
score (Number v _) n = n + v
score (Packet v t ps) n = foldl (\acc p -> score p acc) (n+v) ps 

scorePart2 :: Packet -> Int 
scorePart2 (Number v n) = n
scorePart2 (Packet v 0 ps) = sum (map scorePart2 ps)
scorePart2 (Packet v 1 ps) = product (map scorePart2 ps)
scorePart2 (Packet v 2 ps) = minimum (map scorePart2 ps)
scorePart2 (Packet v 3 ps) = maximum (map scorePart2 ps)
scorePart2 (Packet v 5 [p1,p2]) = if scorePart2 p1 > scorePart2 p2 then 1 else 0 
scorePart2 (Packet v 6 [p1,p2]) = if scorePart2 p1 < scorePart2 p2 then 1 else 0 
scorePart2 (Packet v 7 [p1,p2]) = if scorePart2 p1 == scorePart2 p2 then 1 else 0 

part1 :: PuzzlePart Int 
part1  input = score p 0
    where 
        bits = toBinary (head input)
        (p, _) = parsePacket bits

part2 :: PuzzlePart Int 
part2  input = scorePart2 p
    where 
        bits = toBinary (head input)
        (p, _) = parsePacket bits