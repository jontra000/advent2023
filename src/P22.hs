{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P22 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)

import qualified Data.Set as S
import Data.List (sortOn, delete)

type Coord3 = (Int, Int, Int)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input22"

solve1 :: [[Coord3]] -> Int
solve1 = disintegrationCount . dropBricks . sortBricks

solve2 :: [[Coord3]] -> Int
solve2 = fallCount . dropBricks . sortBricks

parse :: String -> [[Coord3]]
parse = map parseLine . lines

parseLine :: String -> [Coord3]
parseLine = parseBrick . map parseCoord . splitOn "~"

parseCoord :: String -> [Int]
parseCoord = map read . splitOn ","

parseBrick :: [[Int]] -> [Coord3]
parseBrick [a@[x1,y1,z1], b@[x2,y2,z2]]
    | x1 < x2 = map (\x -> (x, y1, z1)) [x1..x2]
    | y1 < y2 = map (\y -> (x1, y, z1)) [y1..y2]
    | z1 < z2 = map (\z -> (x1, y1, z)) [z1..z2]
    | x1 == x2 && y1 == y2 && z1 == z2 = [(x1,y1,z1)]
    | otherwise = error ("non linear brick: " ++ show a ++ " " ++ show b) 
parseBrick _ = error "bad brick input"

dropBricks :: [[Coord3]] -> [[Coord3]]
dropBricks bricks =
    let bricks' = dropBricks' S.empty bricks
    in  if bricks'== bricks
        then bricks
        else dropBricks bricks'

dropBricks' :: S.Set Coord3 -> [[Coord3]] -> [[Coord3]]
dropBricks' _ [] = []
dropBricks' laidBricks (next:bricks) =
    toAdd : dropBricks' (S.union laidBricks (S.fromList toAdd)) bricks
        where next' = map (\(x,y,z) -> (x,y,z-1)) next
              toAdd = if any ((<1) . (\(_,_,z) -> z)) next' || not (S.null (S.intersection laidBricks (S.fromList next'))) then next else next'

sortBricks :: [[Coord3]] -> [[Coord3]]
sortBricks = sortOn (minimum . map (\(_,_,z) -> z))

disintegrationCount :: [[Coord3]] -> Int
disintegrationCount bricks = length $ filter (canDisintegrate bricks) bricks

canDisintegrate :: [[Coord3]] -> [Coord3] -> Bool
canDisintegrate bricks brick =
    let bricks' = delete brick bricks
    in  bricks' == dropBricks' S.empty bricks' 

fallCount :: [[Coord3]] -> Int
fallCount bricks = sum $ map (fallCount' bricks) bricks

fallCount' :: [[Coord3]] -> [Coord3] -> Int
fallCount' bricks brick =
    let bricks' = delete brick bricks
        bricks'' = dropBricks' S.empty bricks'
    in  length $ filter not $ zipWith (==) bricks' bricks''