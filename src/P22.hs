{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P22 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn, delete)
import Data.Maybe (fromMaybe)
import Lib (Coord)

type Coord3 = (Int, Int, Int)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input22"

solve1 :: [S.Set Coord3] -> Int
solve1 = length . filter (==0) . fallCount M.empty

solve2 :: [S.Set Coord3] -> Int
solve2 = sum . fallCount M.empty

parse :: String -> [S.Set Coord3]
parse = dropBricks . sortBricks . map parseLine . lines

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

dropBricks :: [S.Set Coord3] -> [S.Set Coord3]
dropBricks = dropBricks' M.empty

dropBricks' :: M.Map Coord Int -> [S.Set Coord3] -> [S.Set Coord3]
dropBricks' _ [] = []
dropBricks' laidBricks (next:bricks) = next' : dropBricks' laidBricks' bricks
        where next' = dropBrick laidBricks next
              laidBricks' = updateLaidBricks laidBricks next'

updateLaidBricks :: M.Map Coord Int -> S.Set Coord3 -> M.Map Coord Int
updateLaidBricks = foldl (\state (x,y,z) -> M.insertWith max (x,y) z state)

dropBrick :: M.Map Coord Int -> S.Set Coord3 -> S.Set Coord3
dropBrick laidBricks brick = dropToZ brick $ (+1) $ topZ laidBricks $ horizontalCoords brick

horizontalCoords :: S.Set Coord3 -> S.Set Coord
horizontalCoords = S.map (\(x,y,_) -> (x,y))

topZ :: M.Map Coord Int -> S.Set Coord -> Int
topZ laidBricks horizontal = fromMaybe 0 (S.lookupMax (S.map (fromMaybe 0 . (`M.lookup` laidBricks)) horizontal))

dropToZ :: S.Set Coord3 -> Int -> S.Set Coord3
dropToZ brick z' =
    let zDiff = z' - minimum (S.map (\(_,_,z) -> z) brick)
    in  S.map (\(x,y,z) -> (x,y,z+zDiff)) brick

sortBricks :: [[Coord3]] -> [S.Set Coord3]
sortBricks = map S.fromList . sortOn (minimum . map (\(_,_,z) -> z))

disintegrationCount :: [S.Set Coord3] -> Int
disintegrationCount bricks = length $ filter (canDisintegrate bricks) bricks

canDisintegrate :: [S.Set Coord3] -> S.Set Coord3 -> Bool
canDisintegrate bricks brick =
    let bricks' = delete brick bricks
    in  bricks' == dropBricks bricks' 

fallCount :: M.Map Coord Int -> [S.Set Coord3] -> [Int]
fallCount _ [] = []
fallCount laid (nextBrick:bricks) =
    let bricks' = dropBricks' laid bricks
        droppedCount = length $ filter not $ zipWith (==) bricks bricks'
        laid' = updateLaidBricks laid nextBrick
    in  droppedCount : fallCount laid' bricks