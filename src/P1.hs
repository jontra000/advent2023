module P1 (run1, run2, inputLocation) where

import Data.List (find)
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input1"

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map extractValue

extractValue :: String -> Int
extractValue s =
    let digitLeft = getFirstDigit s
        digitRight = getFirstDigit (reverse s)
    in  10 * digitLeft + digitRight

getFirstDigit :: String -> Int
getFirstDigit = digitToInt . fromJust . find isDigit

solve2 :: [String] -> Int
solve2 = sum . map (extractValue2 . extractDigits)

extractValue2 :: [Int] -> Int
extractValue2 s = head s * 10 + last s
    
extractDigits :: String -> [Int]
extractDigits [] = []
extractDigits s@(c:cs)
    | isDigit c = (digitToInt c) : extractDigits cs
    | otherwise = case s of 'o':'n':'e':_ -> 1 : extractDigits cs
                            't':'w':'o':_ -> 2 : extractDigits cs
                            't':'h':'r':'e':'e':_ -> 3 : extractDigits cs
                            'f':'o':'u':'r':_ -> 4 : extractDigits cs
                            'f':'i':'v':'e':_ -> 5 : extractDigits cs
                            's':'i':'x':_ -> 6 : extractDigits cs
                            's':'e':'v':'e':'n':_ -> 7 : extractDigits cs
                            'e':'i':'g':'h':'t':_ -> 8 : extractDigits cs
                            'n':'i':'n':'e':_ -> 9 : extractDigits cs
                            _ -> extractDigits cs