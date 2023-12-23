{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P21 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap, Coord)
import Data.Maybe (fromJust)
import Data.List (find, minimumBy)
import Data.Function (on)

type Distances = M.Map Coord Int
type Grid = M.Map Coord Char

run1 :: String -> Int
run1 = solve1 . parse

parseDup :: Int -> String -> Grid
parseDup x = parse . dupRows x . dupColumns x

dupColumns :: Int -> String -> [String]
dupColumns x = map (concat . replicate x) . lines

dupRows :: Int -> [String] -> String
dupRows x = unlines . concat . replicate x

run2 :: String -> Int
run2 = solve2 . parseDup 5

inputLocation :: String
inputLocation = "inputs/input21"

solve1 :: Grid -> Int
solve1 = calcManual 64 . bestPaths

startingPosition :: Grid -> Coord
startingPosition = fst . fromJust . find ((=='S') . snd) . M.toList

solve2 :: Grid -> Int
solve2 input = (+ (-2)) $ removeLines limit $ solveTessellated limit expandedDimension start input
    where limit = 26501365
          dimension = getDimension input
          expandedDimension = 2*dimension `div` 5
          start = (dimension `div` 2, dimension `div` 2)

solveTessellated :: Int -> Int -> Coord -> Grid -> Int
solveTessellated limit dimension start = sum . map (solveQuadrant limit dimension) . quadrants dimension start

solveQuadrant :: Int -> Int -> Distances -> Int
solveQuadrant limit dimension grid = fullGridCount + partialGridEvenCount + partialGridOddCount
        where n = limit `div` dimension - 1
              gridCount = calcManual limit grid
              fullGridCount = ((n+1) * n `div` 2) * gridCount
              partialGridEvenCount = (n+1) * calcManual (limit `mod` dimension + dimension) grid
              partialGridOddCount = (n+2) * calcManual (limit `mod` dimension) grid

quadrants :: Int -> Coord -> Grid -> [Distances]
quadrants dimension start grid = map (bestPaths' start  . (\f -> f dimension start grid)) [filterBottomRight, filterBottomLeft, filterTopLeft, filterTopRight]

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

calcManual :: Int -> Distances -> Int
calcManual limit = length . M.filter (<=limit) . M.filter((== limit `mod` 2) . (`mod` 2))

getDimension :: M.Map Coord a -> Int
getDimension = (+1) . maximum . map fst . M.keys

parse :: String -> Grid
parse = textToCoordMap

neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

bestPaths :: Grid -> Distances
bestPaths input = bestPaths' (startingPosition input) input

bestPaths' :: Coord -> Grid -> Distances
bestPaths' start input = dijkstra21 unvisitedNodes M.empty M.empty start 0
    where unvisitedNodes = initUnvisitedNodes start input

initUnvisitedNodes :: Coord -> Grid -> S.Set Coord
initUnvisitedNodes start = S.delete start . M.keysSet . M.filter (/='#')

dijkstra21 :: S.Set Coord -> Distances -> Distances -> Coord -> Int -> Distances
dijkstra21 unvisitedNodes tentativeNodes foundNodes nextNode distance
    | null tentativeNodes' = foundNodes'
    | otherwise = dijkstra21 unvisitedNodes' tentativeNodes' foundNodes' nextNode' distance'
    where   toUpdate = S.fromList $ filter (isUnvisited unvisitedNodes tentativeNodes) (neighbours nextNode)
            unvisitedNodes' = unvisitedNodes S.\\ toUpdate
            tentativeNodes' = updateReachable nextNode toUpdate distance tentativeNodes
            foundNodes' = M.insert nextNode distance foundNodes
            (nextNode', distance') = minimumBy (compare `on` snd) $ M.toList tentativeNodes'

updateReachable :: Coord -> S.Set Coord -> Int -> Distances -> Distances
updateReachable nextNode toUpdate distance = updateReachable' toUpdate distance . M.delete nextNode

updateReachable' :: S.Set Coord -> Int -> Distances -> Distances
updateReachable' toUpdate distance reachable = foldl (updateDistance (distance+1)) reachable toUpdate

isUnvisited :: S.Set Coord -> Distances -> Coord -> Bool
isUnvisited unvisitedNodes tentativeNodes node = S.member node unvisitedNodes || M.member node tentativeNodes

updateDistance :: Int -> Distances -> Coord -> Distances
updateDistance distance nodes node = M.insertWith min node distance nodes