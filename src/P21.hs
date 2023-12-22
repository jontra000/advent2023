{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P21 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap, Coord)
import Data.Maybe (fromJust)
import Data.List (find, minimumBy)
import Data.Function (on)

run1 :: String -> Int
run1 = solve1 . parse

parseDup :: Int -> String -> M.Map Coord Char
parseDup x = parse . dupRows x . dupColumns x

dupColumns :: Int -> String -> [[Char]]
dupColumns x = map (concat . replicate x) . lines

dupRows :: Int -> [String] -> String
dupRows x = unlines . concat . replicate x

run2 :: String -> Int
run2 = solve2 . parseDup 5

inputLocation :: String
inputLocation = "inputs/input21"

solve1 :: M.Map Coord Char -> Int
solve1 = length . M.filter (<=64) . M.filter ((==0) . (`mod` 2)) . bestPaths

startingPosition :: M.Map c Char -> c
startingPosition = fst . fromJust . find ((=='S') . snd) . M.toList

solve2 :: M.Map Coord Char -> Int
solve2 input = (+ (-2)) $ removeLines limit $ solveTessellated limit (2*dimension `div` 5) start input
    where limit = 26501365
          dimension = getDimension input
          start = (dimension `div` 2, dimension `div` 2)

solveTessellated :: Int -> Int -> Coord -> M.Map Coord Char -> Int
solveTessellated limit dimension start = sum . map (solveQuadrant limit dimension) . quadrants dimension start

solveQuadrant :: Int -> Int -> M.Map k Int -> Int
solveQuadrant limit dimension grid
    | n < 0 = calcManual (limit `mod` dimension) grid
    | otherwise = ((n+1) * n `div` 2) * gridCount + (n+1) * calcManual (limit `mod` dimension + dimension) grid + (n+2) * calcManual (limit `mod` dimension) grid
        where n = limit `div` dimension - 1
              gridCount = length $ M.filter ((== limit `mod` 2) . (`mod` 2)) grid

quadrants :: Int -> Coord -> M.Map Coord Char -> [M.Map Coord Int]
quadrants dimension start grid = map (\f -> bestPaths' (f dimension start grid) start) [filterBottomRight, filterBottomLeft, filterTopLeft, filterTopRight]

filterBottomRight :: Int -> Coord -> M.Map Coord a -> M.Map Coord a
filterBottomRight dimension (x,y) = M.filterWithKey (\(x',y') _ -> x' >= x && x' < x + dimension && y' >= y && y' < y + dimension)

filterBottomLeft :: Int -> Coord -> M.Map Coord a -> M.Map Coord a
filterBottomLeft dimension (x,y) = M.filterWithKey (\(x',y') _ -> x' <= x && x' > x - dimension && y' >= y && y' < y + dimension)

filterTopLeft :: Int -> Coord -> M.Map Coord a -> M.Map Coord a
filterTopLeft dimension (x,y) = M.filterWithKey (\(x',y') _ -> x' <= x && x' > x - dimension && y' <= y && y' > y - dimension)

filterTopRight :: Int -> Coord -> M.Map Coord a -> M.Map Coord a
filterTopRight dimension (x,y) = M.filterWithKey (\(x',y') _ -> x' >= x  && x' < x + dimension && y' <= y && y' > y - dimension)

removeLines :: Int -> Int -> Int
removeLines limit x = x-(2*limit)

calcManual :: Int -> M.Map k Int -> Int
calcManual limit = length . M.filter((== limit `mod` 2) . (`mod` 2)) . M.filter (<=limit)

getDimension :: M.Map Coord a -> Int
getDimension = (+1) . maximum . map fst . M.keys

parse :: String -> M.Map Coord Char
parse = textToCoordMap

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

bestPaths :: M.Map Coord Char -> M.Map Coord Int
bestPaths input = bestPaths' input (startingPosition input)

bestPaths' :: M.Map Coord Char -> Coord -> M.Map Coord Int
bestPaths' input start = dijkstra21 unvisitedNodes M.empty M.empty start 0
    where unvisitedNodes = S.delete start $ S.fromList (M.keys (M.filter (/='#') input))

dijkstra21 :: S.Set Coord -> M.Map Coord Int -> M.Map Coord Int -> Coord -> Int -> M.Map Coord Int
dijkstra21 unvisitedNodes tentativeNodes foundNodes nextNode distance =
    let toUpdate = S.fromList $ filter (isUnvisited unvisitedNodes tentativeNodes) (neighbours nextNode)
        unvisitedNodes' = unvisitedNodes S.\\ toUpdate
        tentativeNodes' = M.delete nextNode tentativeNodes
        tentativeNodes'' = foldl (updateDistance (distance+1)) tentativeNodes' toUpdate
        foundNodes' = M.insert nextNode distance foundNodes
        (nextNode', distance') = minimumBy (compare `on` snd) $ M.toList tentativeNodes''
    in  if null tentativeNodes''
        then foundNodes'
        else dijkstra21 unvisitedNodes' tentativeNodes'' foundNodes' nextNode' distance'

isUnvisited :: S.Set Coord -> M.Map Coord a -> Coord -> Bool
isUnvisited unvisitedNodes tentativeNodes node = S.member node unvisitedNodes || M.member node tentativeNodes

updateDistance :: Int -> M.Map Coord Int -> Coord -> M.Map Coord Int
updateDistance distance nodes node = M.insertWith min node distance nodes