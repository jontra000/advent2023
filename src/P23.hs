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
solve1 = longestPath . makeGraph

neighbours :: M.Map Coord Char -> Coord -> [Coord]
neighbours grid node = filter (`M.member` grid) $ neighbours' node
    where c = M.lookup node grid
          neighbours' (x,y)
            | c == Just 'v' = [(x,y+1)]
            | c == Just '<' = [(x-1,y)]
            | c == Just '^' = [(x,y-1)]
            | c == Just '>' = [(x+1,y)]
            | otherwise = [(x,y+1), (x-1,y), (x,y-1), (x+1,y)]

solve2 :: M.Map Coord a -> Int
solve2 = solve1 . M.map (const '.')

makeGraph ::  M.Map Coord Char -> M.Map Coord [(Coord, Int)]
makeGraph grid = M.fromList $ map (makeNode grid) (findNodes grid)

findNodes :: M.Map Coord Char -> [Coord]
findNodes grid = filter (isNode grid) $ M.keys grid

isNode :: M.Map Coord Char -> Coord -> Bool
isNode grid = (/=2) . length . neighbours grid

makeNode :: M.Map Coord Char -> Coord -> (Coord, [(Coord, Int)])
makeNode grid node = (node, connections)
    where grid' = M.delete node grid
          connections = map (walkPath grid' 1) (neighbours grid node)

walkPath :: M.Map Coord Char -> Int -> Coord -> (Coord, Int)
walkPath grid distance cell =
    case neighbours grid cell of
        [nextCell] -> walkPath grid' (distance+1) nextCell
        _ -> (cell, distance)
    where grid' = M.delete cell grid

longestPath :: M.Map Coord [(Coord, Int)] -> Int
longestPath nodes =
    let start = fst $ M.findMin nodes
        end = fst $ M.findMax nodes
    in  longestPath' nodes start end 0

longestPath' :: M.Map Coord [(Coord, Int)] -> Coord -> Coord -> Int -> Int
longestPath' nodes current end distance
    | M.notMember current nodes = 0
    | current == end = distance
    | otherwise = maximum (map longestPath'' (nodes M.! current))
        where nodes' = M.delete current nodes
              longestPath'' (next, newDistance) = longestPath' nodes' next end (distance + newDistance)