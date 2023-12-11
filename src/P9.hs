module P9 (run1, run2, inputLocation) where

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse :: String -> [[Int]]
parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

solve1 :: [[Int]] -> Int
solve1 = sum . map solveLine

solveLine :: [Int] -> Int
solveLine xs = last xs + solveDiff (diff xs)

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

solveDiff :: [Int] -> Int
solveDiff xs
    | all (==0) xs = 0
    | otherwise = solveLine xs

solve2 :: [[Int]] -> Int
solve2 = solve1 . map reverse