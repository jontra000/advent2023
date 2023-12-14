module P13 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (elemIndex, transpose)
import Data.Maybe (fromMaybe)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input13"

parse :: String -> [[String]]
parse = splitOn [""] . lines

solve1 :: [[String]] -> Int
solve1 = sum . map solveNote

solve2 :: [[String]] -> Int
solve2 = sum . map solveNote2

solveNote2 :: [String] -> Int
solveNote2 note = 100 * solveNote2' note + solveNote2' (transpose note)

solveNote2' :: [String] -> Int
solveNote2' note = (1+) $ fromMaybe (-1) $ elemIndex 1 $ map (isSmudgedReflection note) [1..(length note)]

solveNote :: [String] -> Int
solveNote note = 100 * solveVertical note + solveHorizontal note

solveVertical :: [String] -> Int
solveVertical note = isReflection note 1

solveHorizontal :: [String] -> Int
solveHorizontal = solveVertical . transpose

isReflection :: [String] -> Int -> Int
isReflection note i
    | i == length note = 0
    | otherwise =
        let (a, b) = splitAt i note
        in  if and $ zipWith (==) (reverse a) b
            then i
            else isReflection note (i+1)

isSmudgedReflection :: [String] -> Int -> Int
isSmudgedReflection note i
    | i == length note = 0
    | otherwise =
        let (a, b) = splitAt i note
        in  errorCount (reverse a) b

errorCount :: [String] -> [String] -> Int
errorCount a b = length $ filter (==False) $ zipWith (==) (concat a) (concat b)