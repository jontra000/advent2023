module P18 (run1, run2, inputLocation) where

import Lib (Coord)
import Data.Char (digitToInt)

data Instruction = Instruction Char Int

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 = solve . parse2

inputLocation :: String
inputLocation = "inputs/input18"

parse :: String -> [Instruction]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Instruction
parseLine (dirStr:lenStr:_) = Instruction (head dirStr) (read lenStr)
parseLine _ = error "bad input"

solve :: [Instruction] -> Int
solve instructions = calcArea (vertices instructions) + borderDiff instructions

vertices :: [Instruction] -> [Coord]
vertices = scanl applyInstruction (0,0)

applyInstruction :: Coord -> Instruction -> Coord
applyInstruction (x,y) (Instruction 'R' len) = (x+len, y)
applyInstruction (x,y) (Instruction 'L' len) = (x-len, y)
applyInstruction (x,y) (Instruction 'U' len) = (x, y-len)
applyInstruction (x,y) (Instruction 'D' len) = (x, y+len)
applyInstruction _ _ = error "bad direction"

calcArea :: [Coord] -> Int
calcArea points =
    let ys = map snd points
        xs = map fst points
        xPlus = tail xs ++ [head xs]
        xMinus = (last xs) : init xs
    in  sum (map shoelaceTerm (zip3 ys xPlus xMinus)) `div` 2

shoelaceTerm :: (Int, Int, Int) -> Int
shoelaceTerm (y, xPlus, xMinus) = y * (xMinus - xPlus)

parse2 :: String -> [Instruction]
parse2 = map (parseLine2 . last . words) . lines

parseLine2 :: String -> Instruction
parseLine2 s =
    let len = parseHex (take 5 (drop 2 s))
        dir = parseDir (s !! 7)
    in  Instruction dir len

parseHex :: String -> Int
parseHex = foldl (\acc x -> acc * 16 + digitToInt x) 0

parseDir :: Char -> Char
parseDir '0' = 'R'
parseDir '1' = 'D'
parseDir '2' = 'L'
parseDir '3' = 'U'
parseDir _ = error "bad direction code"

borderDiff :: [Instruction] -> Int
borderDiff instructions = sum (map (\(Instruction _ len) -> len) instructions) `div` 2 + netCorners instructions `div` 4

netCorners :: Num a => [Instruction] -> a
netCorners instructions = sum $ zipWith cornerPolarity instructions (tail instructions ++ [head instructions])

cornerPolarity :: Num a => Instruction -> Instruction -> a
cornerPolarity (Instruction dir1 _) (Instruction dir2 _)
    | isInsideTurn dir1 dir2 = -1
    | otherwise = 1

isInsideTurn :: Char -> Char -> Bool
isInsideTurn 'R' 'U' = True
isInsideTurn 'U' 'L' = True
isInsideTurn 'L' 'D' = True
isInsideTurn 'D' 'R' = True
isInsideTurn _ _ = False