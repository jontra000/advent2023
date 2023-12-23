{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P22 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Lib (Coord)

type Coord3 = (Int, Int, Int)
type TopLevel = M.Map Coord Int

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
parseBrick [[x1,y1,z1], [x2,y2,z2]]
    | x1 < x2 = map (\x -> (x, y1, z1)) [x1..x2]
    | y1 < y2 = map (\y -> (x1, y, z1)) [y1..y2]
    | z1 < z2 = map (\z -> (x1, y1, z)) [z1..z2]
    | otherwise = [(x1,y1,z1)]
parseBrick _ = error "bad brick input"

dropBricks :: [S.Set Coord3] -> [S.Set Coord3]
dropBricks = dropBricks' M.empty

dropBricks' :: TopLevel -> [S.Set Coord3] -> [S.Set Coord3]
dropBricks' _ [] = []
dropBricks' laidBricks (next:bricks) = next' : dropBricks' laidBricks' bricks
    where next' = dropBrick laidBricks next
          laidBricks' = updateTopLevel laidBricks next'

updateTopLevel :: TopLevel -> S.Set Coord3 -> TopLevel
updateTopLevel = foldl (\state (x,y,z) -> M.insertWith max (x,y) z state)

dropBrick :: TopLevel -> S.Set Coord3 -> S.Set Coord3
dropBrick laidBricks brick = dropToZ brick $ (+1) $ topZ laidBricks $ horizontalCoords brick

horizontalCoords :: S.Set Coord3 -> S.Set Coord
horizontalCoords = S.map (\(x,y,_) -> (x,y))

topZ :: TopLevel -> S.Set Coord -> Int
topZ laidBricks = fromMaybe 0 . S.lookupMax . topZs laidBricks

topZs :: TopLevel -> S.Set Coord -> S.Set Int
topZs laidBricks = S.map (fromMaybe 0 . (`M.lookup` laidBricks))

dropToZ :: S.Set Coord3 -> Int -> S.Set Coord3
dropToZ brick z' =
    let zDiff = z' - minZ brick
    in  shiftZ zDiff brick

minZ :: S.Set Coord3 -> Int
minZ =  minimum . S.map (\(_,_,z) -> z)

shiftZ :: Int -> S.Set Coord3 -> S.Set Coord3
shiftZ zDiff = S.map shiftZ'
    where shiftZ' (x,y,z) = (x,y,z+zDiff)

sortBricks :: [[Coord3]] -> [S.Set Coord3]
sortBricks = sortOn minZ . map S.fromList

fallCount :: M.Map Coord Int -> [S.Set Coord3] -> [Int]
fallCount _ [] = []
fallCount laid (nextBrick:bricks) =
    let bricks' = dropBricks' laid bricks
        droppedCount = length $ filter not $ zipWith (==) bricks bricks'
        laid' = updateTopLevel laid nextBrick
    in  droppedCount : fallCount laid' bricks