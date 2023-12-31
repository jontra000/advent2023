module P14 (run1, run2, inputLocation) where

import Data.List (transpose, elemIndices, elemIndex)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input14"

parse :: String -> [String]
parse = transpose . lines

solve1 :: [String] -> Int
solve1 = load . map tiltRow

solve2 :: [String] -> Int
solve2 = load . findCycle [] (1000000000+1) . iterate doCycle

doCycle :: [String] -> [String]
doCycle = tiltAndRotate . tiltAndRotate . tiltAndRotate . tiltAndRotate

tiltAndRotate :: [String] -> [String]
tiltAndRotate = reverse .transpose . map tiltRow

tiltRow :: String -> String
tiltRow = tiltRow' [] []

tiltRow' :: String -> String -> String -> String
tiltRow' xs pending [] = reverse $ pending ++ xs
tiltRow' xs [] ('O':res) = tiltRow' ('O' : xs) [] res
tiltRow' xs pending ('O':res) = tiltRow' ('O' : xs) pending res
tiltRow' xs pending ('#':res) = tiltRow' ('#' : pending ++ xs) [] res
tiltRow' xs pending (x:res) = tiltRow' xs (x:pending) res

findCycle :: Eq a => [a] -> Int -> [a] -> a
findCycle cache limit (x:xs) = 
    case elemIndex x cache of
        Just i -> fastForwardCycle cache limit (i+1)
        Nothing -> findCycle (x:cache) limit xs
findCycle _ _ [] = error "end of infinite list"

fastForwardCycle :: [a] -> Int -> Int -> a
fastForwardCycle cache limit cycleLength =
    let offset = length cache - cycleLength
        solutionIndex = (limit - offset) `mod` cycleLength
    in  cache !! (cycleLength - solutionIndex)    

load :: [String] -> Int
load = sum . map loadRow

loadRow :: String -> Int
loadRow = sum . map (+1) . elemIndices 'O' . reverse