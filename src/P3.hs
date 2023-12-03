module P3 (run1, run2, inputLocation) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (isDigit)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
data Input = Input (M.Map Coord Char) [(Int, S.Set Coord)] deriving Show
    
run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input3"

parse :: String -> Input
parse = foldl parseLine (Input M.empty []) . zip [0..] . lines

parseLine :: Input -> (Int, String) -> Input
parseLine input (yCoord, line) = parseLine' input yCoord (zip [0..] line)

parseLine' :: Input -> Int -> [(Int, Char)] -> Input
parseLine' input _ [] = input
parseLine' input y ((_, '.'):line) = parseLine' input y line
parseLine' input y s@((x, c):line)
    | isDigit c = parseNumber input y s
    | otherwise = addSymbol x y c input line

parseNumber :: Input -> Int -> [(Int, Char)] -> Input
parseNumber (Input symbols numbers) y line =
    let (newDigits, remainingLine) = span (isDigit . snd) line
        partNumber = read (map snd newDigits)
        coords = S.fromList $ map ((`Coord` y) . fst) newDigits
        numbers' = (partNumber, coords) : numbers
    in  parseLine' (Input symbols numbers') y remainingLine

addSymbol :: Int -> Int -> Char -> Input -> [(Int, Char)] -> Input
addSymbol x y c (Input symbols numbers) = parseLine' (Input (M.insert (Coord x y) c symbols) numbers) y

solve1 :: Input -> Int
solve1 = sum. partNumbers

partNumbers :: Input -> [Int]
partNumbers (Input symbols numbers) = map fst $ filter (isPartNumber (S.fromList (M.keys symbols)) . snd) numbers

isPartNumber :: S.Set Coord -> S.Set Coord -> Bool
isPartNumber symbolLocs = not . null . S.intersection symbolLocs . neighbours

neighbours :: S.Set Coord -> S.Set Coord
neighbours = S.unions . S.map neighbours'

neighbours' :: Coord -> S.Set Coord
neighbours' (Coord x y) = S.fromList [Coord x' y' | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1]]

solve2 :: Input -> Int
solve2 (Input symbols numbers) = sum $ map (gearValue numbers) $ M.keys $ M.filter (=='*') symbols

gearValue :: [(Int, S.Set Coord)] -> Coord -> Int
gearValue numbers c =
    let neighbouringNumbers = filter (isNeighbour c) numbers
    in  if length neighbouringNumbers == 2
        then product (map fst neighbouringNumbers)
        else 0

isNeighbour :: Coord -> (Int, S.Set Coord) -> Bool
isNeighbour c = S.member c . neighbours . snd