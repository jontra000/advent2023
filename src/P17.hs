{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P17 (run1, run2, inputLocation) where

import Lib ( textToCoordMap, Coord )
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)
import Data.Function (on)

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Ord, Eq)
data CurrentNode = CurrentNode Coord Direction Int Int deriving Eq -- location direction momentum heat

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input17"

instance Ord CurrentNode where
    compare (CurrentNode _ _ _ heat1) (CurrentNode _ _ _ heat2) = compare heat1 heat2

parse :: String -> M.Map Coord Int
parse =  M.map digitToInt . textToCoordMap

solve1 :: M.Map Coord Int -> Int
solve1 input = dijkstra17 input 1 3 (targetCoord input) (startState $ initState input) ((0,0), DirRight, 0)

solve2 :: M.Map Coord Int -> Int
solve2 input = dijkstra17 input 4 7 (targetCoord input) (startState $ initState input) ((0,0), DirRight, 0)

targetCoord :: M.Map Coord Int -> Coord
targetCoord input = (maximum (map fst (M.keys input)), maximum (map snd (M.keys input)))

initState :: M.Map Coord Int -> M.Map (Coord, Direction) Int
initState = M.fromList . concatMap (\loc -> [((loc, direction), maxBound) | direction <- [DirLeft, DirRight, DirUp, DirDown]]) . M.keys

startState :: M.Map (Coord, Direction) Int -> M.Map (Coord, Direction) Int
startState state = foldl (\state' (k, v) -> M.update (Just . min v) k state') state [(((0,0), DirDown), 0), (((0,0), DirRight), 0)] 

dijkstra17 :: M.Map Coord Int -> Int -> Int -> Coord -> M.Map (Coord, Direction) Int -> (Coord, Direction, Int) -> Int
dijkstra17 heatMap minMomentum maxMove target state currentNode@(location, direction, heat)
    | location == target = heat
    | otherwise =
        let directions = turnDirs direction
            toUpdate = catMaybes $ concatMap (\direction' -> nextBlocks minMomentum maxMove heatMap location direction' heat) directions
            state' = updateNeighbours state toUpdate
            state'' = markVisited state' currentNode
        in  dijkstra17 heatMap minMomentum maxMove target state'' (getNextNode state'')

getNextNode :: M.Map (a, b) Int -> (a, b, Int)
getNextNode = toNode . minimumBy (compare `on` snd) . M.toList

toNode :: ((a, b), c) -> (a, b, c)
toNode ((location, direction), heat) = (location, direction, heat)

markVisited :: (Ord a1, Ord b) => M.Map (a1, b) a2 -> (a1, b, c) -> M.Map (a1, b) a2
markVisited state (location, direction, _) = M.delete (location, direction) state 

updateNeighbours :: M.Map (Coord, Direction) Int -> [(Coord, Direction, Int)] -> M.Map (Coord, Direction) Int
updateNeighbours = foldl updateNeighbour

updateNeighbour :: (Ord a1, Ord b, Ord a2) => M.Map (a1, b) a2 -> (a1, b, a2) -> M.Map (a1, b) a2
updateNeighbour state (location, direction, heat) = M.update (Just . min heat) (location, direction) state

move :: (Num b, Num a) => Direction -> (a, b) -> (a, b)
move DirDown (x,y) = (x, y+1)
move DirUp (x,y) = (x, y-1)
move DirLeft (x,y) = (x-1, y)
move DirRight (x,y) = (x+1, y)

step :: (Ord a, Ord b, Num b, Num a, Num c) => M.Map (a, b) c -> Maybe ((a, b), Direction, c) -> Maybe ((a, b), Direction, c)
step _ Nothing = Nothing
step heatMap (Just (loc, direction, heat)) =
    let loc' = move direction loc
    in  case M.lookup loc' heatMap of
            Nothing -> Nothing
            Just newHeat ->
                let heat' = heat + newHeat
                in  Just (loc', direction, heat')

nextBlocks :: (Ord a, Ord b, Num b, Num a, Num c) => Int -> Int -> M.Map (a, b) c -> (a, b) -> Direction -> c -> [Maybe ((a, b), Direction, c)]
nextBlocks minMomentum maxMove heatMap location direction startHeat = take maxMove $ drop minMomentum $ iterate (step heatMap) (Just (location, direction, startHeat))

turnDirs :: Direction -> [Direction]
turnDirs DirUp = [DirLeft, DirRight]
turnDirs DirDown = [DirLeft, DirRight]
turnDirs DirLeft = [DirUp, DirDown]
turnDirs DirRight = [DirUp, DirDown]