module P23 (run1, run2, inputLocation) where

import Lib (textToCoordMap, Coord)
import qualified Data.Map as M

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input23"

parse :: String -> M.Map Coord Char
parse = removeWalls . textToCoordMap

removeWalls :: M.Map k Char -> M.Map k Char
removeWalls = M.filter (/= '#')

solve1 :: M.Map Coord Char -> Int
solve1 grid = longestPath 0 (finishNode grid) grid 0 (startingNode grid)

longestPath :: Int -> Coord -> M.Map Coord Char -> Int -> Coord -> Int
longestPath distance targetNode grid bestDistance currentNode
    | currentNode == targetNode = max distance bestDistance
    | otherwise = foldl (longestPath distance' targetNode grid') bestDistance nextNodes
        where   distance' = distance + 1
                nextNodes = neighbours grid currentNode
                grid' = M.delete currentNode grid

finishNode :: M.Map Coord a -> Coord
finishNode = maximum . M.keys

startingNode :: M.Map Coord a -> Coord
startingNode = minimum . M.keys

neighbours :: M.Map Coord Char -> Coord -> [Coord]
neighbours grid node@(x,y)
    | c == Just 'v' = filter (`M.member` grid) [(x,y+1)]
    | c == Just '<' = filter (`M.member` grid) [(x-1,y)]
    | c == Just '^' = filter (`M.member` grid) [(x,y-1)]
    | c == Just '>' = filter (`M.member` grid) [(x+1,y)]
    | otherwise = filter (`M.member` grid) [(x,y+1), (x-1,y), (x,y-1), (x+1,y)]
    where c = M.lookup node grid

solve2 :: M.Map Coord a -> Int
solve2 = longestPath2 . makeGraph . M.map (const '.')

makeGraph ::  M.Map Coord Char -> M.Map Coord [(Coord, Int)]
makeGraph grid = M.fromList $ map (makeNode grid) nodes
    where nodes = filter (isNode grid) $ M.keys grid

isNode :: M.Map Coord Char -> Coord -> Bool
isNode grid = (/=2) . length . neighbours grid

makeNode :: M.Map Coord Char -> Coord -> (Coord, [(Coord, Int)])
makeNode grid node = (node, map (walkPath grid' 1) (neighbours grid node))
    where grid' = M.delete node grid

walkPath :: M.Map Coord Char -> Int -> Coord -> (Coord, Int)
walkPath grid distance cell =
    case neighbours grid cell of
        [nextCell] -> walkPath grid' (distance+1) nextCell
        _ -> (cell, distance)
    where grid' = M.delete cell grid

longestPath2 :: M.Map Coord [(Coord, Int)] -> Int
longestPath2 nodes =
    let start = fst $ M.findMin nodes
        end = fst $ M.findMax nodes
    in  longestPath2' nodes start end 0

longestPath2' :: M.Map Coord [(Coord, Int)] -> Coord -> Coord -> Int -> Int
longestPath2' nodes current end distance
    | M.notMember current nodes = 0
    | current == end = distance
    | otherwise = maximum (map (\(next, newDistance) -> longestPath2' nodes' next end (distance + newDistance)) (nodes M.! current))
        where nodes' = M.delete current nodes