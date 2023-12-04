module P3 (run1, run2, inputLocation) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (isDigit)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
type SymbolLocations = M.Map Coord Char
type NumberLocation = (Int, S.Set Coord)
data Input = Input SymbolLocations [NumberLocation] deriving Show
    
run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input3"

initialInput :: Input
initialInput = Input M.empty []

parse :: String -> Input
parse = foldl parseLine initialInput . zip [0..] . lines

parseLine :: Input -> (Int, String) -> Input
parseLine input (y, line) = parseLine' input y (zip [0..] line)

parseLine' :: Input -> Int -> [(Int, Char)] -> Input
parseLine' input _ [] = input
parseLine' input y ((_, '.'):line) = parseLine' input y line
parseLine' input y s@((x, c):line)
    | isDigit c = parseNumber input y s
    | otherwise = addSymbol x y c input line

parseNumber :: Input -> Int -> [(Int, Char)] -> Input
parseNumber input y line =
    let (numberStr, remainingLine) = span (isDigit . snd) line
        input' = addNumber input y numberStr
    in  parseLine' input' y remainingLine

addNumber :: Input -> Int -> [(Int, Char)] -> Input
addNumber (Input symbols numbers) y numberStr =
    let partNumber = readNumber numberStr
        coords = numberCoords y numberStr
        numbers' = (partNumber, coords) : numbers
    in  Input symbols numbers'

readNumber :: [(Int, Char)] -> Int
readNumber = read . map snd

numberCoords :: Int -> [(Int, Char)] -> S.Set Coord
numberCoords y = S.fromList . map ((`Coord` y) . fst)

addSymbol :: Int -> Int -> Char -> Input -> [(Int, Char)] -> Input
addSymbol x y c (Input symbols numbers) =
    let symbols' = M.insert (Coord x y) c symbols
        input' = Input symbols' numbers
    in  parseLine' input' y

solve1 :: Input -> Int
solve1 = sum. partNumbers

partNumbers :: Input -> [Int]
partNumbers (Input symbols numbers) = map fst $ filter (isPartNumber (symbolCoords symbols)) numbers

symbolCoords :: SymbolLocations -> S.Set Coord
symbolCoords = S.fromList . M.keys

isPartNumber :: S.Set Coord -> NumberLocation -> Bool
isPartNumber symbolLocs = coordsMatch symbolLocs . numberNeighbours

numberNeighbours :: NumberLocation -> S.Set Coord
numberNeighbours = neighbours . snd

coordsMatch :: S.Set Coord -> S.Set Coord -> Bool
coordsMatch a = not . null . S.intersection a

neighbours :: S.Set Coord -> S.Set Coord
neighbours = S.unions . S.map neighbours'

neighbours' :: Coord -> S.Set Coord
neighbours' (Coord x y) = S.fromList [Coord x' y' | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1]]

solve2 :: Input -> Int
solve2 = sum . map gearRatio . gears

gears :: Input -> [[NumberLocation]]
gears (Input symbols numbers) = filter isGear $ map (`neighbouringNumbers` numbers) $ gearSymbolLocations symbols

gearSymbolLocations :: SymbolLocations -> [Coord]
gearSymbolLocations = M.keys . M.filter (=='*')

neighbouringNumbers :: Coord -> [NumberLocation] -> [NumberLocation]
neighbouringNumbers c = filter (isNeighbour c)

isGear :: [NumberLocation] -> Bool
isGear = (==2) . length

gearRatio :: [NumberLocation] -> Int
gearRatio = product . map fst

isNeighbour :: Coord -> (Int, S.Set Coord) -> Bool
isNeighbour c = S.member c . neighbours . snd