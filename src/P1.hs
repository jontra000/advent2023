module P1 (run1, run2, inputLocation) where

import Data.List (isPrefixOf)
import Data.Char (isDigit, digitToInt)

run1 :: String -> Int
run1 = solve . parse1

run2 :: String -> Int
run2 = solve . parse2

inputLocation :: String
inputLocation = "inputs/input1"

parse1 :: String -> [[Int]]
parse1 = map parseLine1 . lines

solve :: [[Int]] -> Int
solve = sum . map extractValue

extractValue :: [Int] -> Int
extractValue xs = head xs * 10 + last xs

parseLine1 :: String -> [Int]
parseLine1 [] = []
parseLine1 (c:cs)
    | isDigit c = digitToInt c : parseLine1 cs
    | otherwise = parseLine1 cs

parse2 :: String -> [[Int]]
parse2 = map parseLine2 . lines

parseLine2 :: String -> [Int]
parseLine2 [] = []
parseLine2 s@(c:cs)
    | isDigit c = digitToInt c : parseLine2 cs
    | "one" `isPrefixOf` s = 1 : parseLine2 cs
    | "two" `isPrefixOf` s = 2 : parseLine2 cs
    | "three" `isPrefixOf` s = 3 : parseLine2 cs
    | "four" `isPrefixOf` s = 4 : parseLine2 cs
    | "five" `isPrefixOf` s = 5 : parseLine2 cs
    | "six" `isPrefixOf` s = 6 : parseLine2 cs
    | "seven" `isPrefixOf` s = 7 : parseLine2 cs
    | "eight" `isPrefixOf` s = 8 : parseLine2 cs
    | "nine" `isPrefixOf` s = 9 : parseLine2 cs
    | otherwise = parseLine2 cs