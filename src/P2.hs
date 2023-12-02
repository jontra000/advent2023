module P2 (run1, run2, inputLocation) where

import Data.List (isPrefixOf)

data CubeSet = CubeSet Int Int Int -- red, green, blue
    
run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map (checkPossible . reverse . words)

checkPossible :: [String] -> Int
checkPossible [idStr,_] = read (init idStr)
checkPossible (colour:amount:res)
    | "red" `isPrefixOf` colour && (read amount > 12) = 0
    | "green" `isPrefixOf` colour && (read amount > 13) = 0
    | "blue" `isPrefixOf` colour && (read amount > 14) = 0
    | otherwise = checkPossible res
checkPossible x = error ("bad tokens: " ++ unwords x)

solve2 :: [String] -> Int
solve2 = sum . map gamePower

gamePower :: String -> Int
gamePower = power . minimumCubeSet (CubeSet 0 0 0) . reverse . words

minimumCubeSet :: CubeSet -> [String] -> CubeSet
minimumCubeSet cubeSet [_, _] = cubeSet
minimumCubeSet (CubeSet red green blue) (colour:amount:res)
    | "red" `isPrefixOf` colour = minimumCubeSet (CubeSet (max red (read amount)) green blue) res
    | "green" `isPrefixOf` colour =  minimumCubeSet (CubeSet red (max (read amount) green) blue) res
    | "blue" `isPrefixOf` colour = minimumCubeSet (CubeSet red green (max (read amount) blue)) res
    | otherwise = error ("bad colour: " ++ colour)
minimumCubeSet cubeSet _ = cubeSet

power :: CubeSet -> Int
power (CubeSet red green blue) = red * green * blue
