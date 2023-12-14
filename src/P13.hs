module P13 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (transpose, find)
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
solve1 = sum . map (summarise 0)

solve2 :: [[String]] -> Int
solve2 = sum . map (summarise 1)

summarise :: Int -> [String] -> Int
summarise errors note = summariseHorizontal errors note + summariseVertical errors note

summariseHorizontal :: Int -> [String] -> Int
summariseHorizontal errors = (100 *) . findReflection' errors

summariseVertical :: Int -> [String] -> Int
summariseVertical errors = findReflection' errors . transpose

findReflection' :: Int -> [String] -> Int
findReflection' errors note = fromMaybe 0 $ find (reflectionHasErrors errors note) [1..(length note - 1)]

reflectionHasErrors :: Int -> [String] -> Int -> Bool
reflectionHasErrors errors note = (==errors) . reflectionErrors note

reflectionErrors :: [String] -> Int -> Int
reflectionErrors note i =
    let (a, b) = splitAt i note
    in  errorCount (reverse a) b

errorCount :: [String] -> [String] -> Int
errorCount a b = length $ filter (==False) $ matchingChars a b

matchingChars :: [String] -> [String] -> [Bool]
matchingChars a b = zipWith (==) (concat a) (concat b)