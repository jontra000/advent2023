module P18 (run1, run2, inputLocation) where

import Lib (Coord)
import Data.Char (digitToInt)

data Direction = DirUp | DirDown | DirLeft | DirRight
data Instruction = Instruction Direction Int

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 = solve . parse2

inputLocation :: String
inputLocation = "inputs/input18"

parse :: String -> [Instruction]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Instruction
parseLine (dirStr:lenStr:_) = Instruction (parseDirection1 dirStr) (read lenStr)
parseLine _ = error "bad input"

parseDirection1 :: String -> Direction
parseDirection1 ('R':_) = DirRight
parseDirection1 ('L':_) = DirLeft
parseDirection1 ('U':_) = DirUp
parseDirection1 ('D':_) = DirDown
parseDirection1 _ = error "Bad direction"

solve :: [Instruction] -> Int
solve instructions = calcArea (vertices instructions) + borderDiff instructions

vertices :: [Instruction] -> [Coord]
vertices = scanl applyInstruction (0,0)

applyInstruction :: Coord -> Instruction -> Coord
applyInstruction (x,y) (Instruction DirRight len) = (x+len, y)
applyInstruction (x,y) (Instruction DirLeft len) = (x-len, y)
applyInstruction (x,y) (Instruction DirUp len) = (x, y-len)
applyInstruction (x,y) (Instruction DirDown len) = (x, y+len)

calcArea :: [Coord] -> Int
calcArea points =
    let ys = map snd points
        xs = map fst points
        xPlus = tail xs ++ [head xs]
        xMinus = last xs : init xs
    in  sum (map shoelaceTerm (zip3 ys xPlus xMinus)) `div` 2

shoelaceTerm :: (Int, Int, Int) -> Int
shoelaceTerm (y, xPlus, xMinus) = y * (xMinus - xPlus)

parse2 :: String -> [Instruction]
parse2 = map (parseLine2 . last . words) . lines

parseLine2 :: String -> Instruction
parseLine2 s =
    let len = parseLength2 s
        dir = parseDirection2 (s !! 7)
    in  Instruction dir len

parseLength2 :: String -> Int
parseLength2 = parseHex . lengthSection

lengthSection :: String -> String
lengthSection = take 5 . drop 2

parseHex :: String -> Int
parseHex = foldl (\acc -> (+ acc * 16)) 0 . map digitToInt

parseDirection2 :: Char -> Direction
parseDirection2 '0' = DirRight
parseDirection2 '1' = DirDown
parseDirection2 '2' = DirLeft
parseDirection2 '3' = DirUp
parseDirection2 _ = error "bad direction code"

borderDiff :: [Instruction] -> Int
borderDiff instructions = totalLength instructions `div` 2 + netCorners instructions `div` 4

totalLength :: [Instruction] -> Int
totalLength = sum . map instructionLength

instructionLength :: Instruction -> Int
instructionLength (Instruction _ len) = len

netCorners :: [Instruction] -> Int
netCorners = sum . zipWithSelfOffset cornerPolarity

zipWithSelfOffset :: (b -> b -> c) -> [b] -> [c]
zipWithSelfOffset f xs = zipWith f xs (tail xs ++ [head xs])

cornerPolarity :: Instruction -> Instruction -> Int
cornerPolarity (Instruction dir1 _) (Instruction dir2 _)
    | isInsideTurn dir1 dir2 = -1
    | otherwise = 1

isInsideTurn :: Direction -> Direction -> Bool
isInsideTurn DirRight DirUp = True
isInsideTurn DirUp DirLeft = True
isInsideTurn DirLeft DirDown = True
isInsideTurn DirDown DirRight = True
isInsideTurn _ _ = False