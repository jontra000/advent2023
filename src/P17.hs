{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P17 (run1, run2, inputLocation) where

import Lib ( textToCoordMap, Coord )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)
import Data.Function (on)

data Orientation = Horizontal | Vertical deriving (Ord, Eq)
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
solve1 input = dijkstra17 input 1 3 (targetCoord input) (initState input) (M.fromList [(((0,0), Vertical), 0), (((0,0), Horizontal), 0)]) ((0,0), Vertical, 0)

solve2 :: M.Map Coord Int -> Int
solve2 input = dijkstra17 input 4 7 (targetCoord input) (initState input) (M.fromList [(((0,0), Vertical), 0), (((0,0), Horizontal), 0)]) ((0,0), Vertical, 0)

targetCoord :: M.Map Coord Int -> Coord
targetCoord input = (maximum (map fst (M.keys input)), maximum (map snd (M.keys input)))

initState :: M.Map Coord Int -> S.Set (Coord, Orientation)
initState = S.fromList . concatMap (\loc -> [(loc, orientation) | orientation <- [Horizontal, Vertical]]) . M.keys . M.delete (0,0)

dijkstra17 :: M.Map Coord Int -> Int -> Int -> Coord -> S.Set (Coord, Orientation) -> M.Map (Coord, Orientation) Int -> (Coord, Orientation, Int) -> Int
dijkstra17 heatMap minMomentum maxMove target unvisited reachable (location, orientation, heat)
    | location == target = heat
    | otherwise =
        let directions = turnDirs orientation
            toUpdate = filter (isUnvisited unvisited reachable) $ catMaybes $ concatMap (\direction' -> nextBlocks minMomentum maxMove heatMap location direction' heat) directions
            reachable' = updateNeighbours reachable toUpdate
            reachable'' = M.delete (location, orientation) reachable'
            unvisited' = markVisited unvisited toUpdate
        in  dijkstra17 heatMap minMomentum maxMove target unvisited' reachable'' (getNextNode reachable'')

isUnvisited :: S.Set (Coord, Orientation) -> M.Map (Coord, Orientation) a -> (Coord, Orientation, c) -> Bool
isUnvisited unvisited reachable (coord, orientation, _) = S.member node unvisited || M.member node reachable
    where node = (coord, orientation)

getNextNode :: M.Map (a, b) Int -> (a, b, Int)
getNextNode = toNode . minimumBy (compare `on` snd) . M.toList

toNode :: ((a, b), c) -> (a, b, c)
toNode ((location, direction), heat) = (location, direction, heat)

markVisited :: (Ord a, Ord b) => S.Set (a, b) -> [(a, b, c)] -> S.Set (a, b)
markVisited state = foldl (flip S.delete) state . map (\(loc, orientation, _) -> (loc, orientation))

updateNeighbours :: M.Map (Coord, Orientation) Int -> [(Coord, Orientation, Int)] -> M.Map (Coord, Orientation) Int
updateNeighbours = foldl updateNeighbour

updateNeighbour :: M.Map (Coord, Orientation) Int -> (Coord, Orientation, Int) -> M.Map (Coord, Orientation) Int
updateNeighbour state (location, orientation, heat) = M.insertWith min (location, orientation) heat state

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

nextBlocks :: (Ord a, Ord b, Num b, Num a, Num c) => Int -> Int -> M.Map (a, b) c -> (a, b) -> Direction -> c -> [Maybe ((a, b), Orientation, c)]
nextBlocks minMomentum maxMove heatMap location direction startHeat = map convertOrientation $ take maxMove $ drop minMomentum $ iterate (step heatMap) (Just (location, direction, startHeat))

convertOrientation :: Maybe (a, Direction, c) -> Maybe (a, Orientation, c)
convertOrientation = fmap (\(c, dir, heat) -> (c, dirOrientation dir, heat))

turnDirs :: Orientation -> [Direction]
turnDirs Horizontal = [DirLeft, DirRight]
turnDirs Vertical = [DirUp, DirDown]

dirOrientation :: Direction -> Orientation
dirOrientation DirLeft = Vertical
dirOrientation DirRight = Vertical
dirOrientation DirUp = Horizontal
dirOrientation DirDown = Horizontal