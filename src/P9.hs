module P9 (run1, run2, inputLocation) where

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

solve1 :: [[Int]] -> Int
solve1 = sum . map solveLine

solveLine :: [Int] -> Int
solveLine xs =
    let diff = zipWith (-) (tail xs) xs
    in  if all (==0) diff
        then last xs
        else last xs + solveLine diff

solve2 :: [[Int]] -> Int
solve2 = solve1 . map reverse