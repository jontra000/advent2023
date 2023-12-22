{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P17 (run1, run2, inputLocation) where

import Lib ( textToCoordMap, Coord )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Char (digitToInt)
import Data.Function (on)

data Orientation = Horizontal | Vertical deriving (Ord, Eq)
data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Ord, Eq)
type Node = (Coord, Orientation)
type Unvisited = S.Set Node
type Reachable = M.Map Node Int
type Input = M.Map Coord Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input17"

parse :: String -> Input
parse =  M.map digitToInt . textToCoordMap

solve1 :: Input -> Int
solve1 input = dijkstra17 input 0 3 (targetCoord input) (initState input) initialReachable startNode 0

solve2 :: Input -> Int
solve2 input = dijkstra17 input 3 7 (targetCoord input) (initState input) initialReachable startNode 0

initialReachable :: Reachable
initialReachable = M.fromList [(((0,0), Vertical), 0), (((0,0), Horizontal), 0)]

startNode :: Node
startNode = ((0,0), Vertical)

targetCoord :: Input -> Coord
targetCoord input = (maxCoord fst input, maxCoord snd input)

maxCoord :: (Coord -> Int) -> Input -> Int
maxCoord f = maximum . map f . M.keys

initState :: Input -> Unvisited
initState = S.fromList . concatMap locationToNodes . M.keys . M.delete (0,0)

locationToNodes :: Coord -> [(Coord, Orientation)]
locationToNodes loc = [(loc, orientation) | orientation <- [Horizontal, Vertical]]

dijkstra17 :: Input -> Int -> Int -> Coord -> Unvisited -> Reachable -> Node -> Int -> Int
dijkstra17 heatMap minMomentum maxMove target unvisited reachable node@(location, _) heat
    | location == target = heat
    | otherwise =
        let toUpdate = nodesToUpdate heatMap minMomentum maxMove unvisited reachable node heat
            reachable' = updateReachable reachable node toUpdate
            unvisited' = markVisited unvisited toUpdate
            (node', heat') = getNextNode reachable'
        in  dijkstra17 heatMap minMomentum maxMove target unvisited' reachable' node' heat'

nodesToUpdate :: Input -> Int -> Int -> Unvisited -> Reachable -> Node -> Int -> [(Node, Int)]
nodesToUpdate heatMap minMomentum maxMove unvisited reachable node = filter (isUnvisited unvisited reachable) . nodesToUpdate' heatMap minMomentum maxMove node

nodesToUpdate' :: Input -> Int -> Int -> Node -> Int -> [(Node, Int)]
nodesToUpdate' heatMap minMomentum maxMove (location, orientation) heat = concatMap (nodesToUpdateInDirection minMomentum maxMove heatMap location heat) (turnDirs orientation)

nodesToUpdateInDirection :: Int -> Int -> Input -> Coord -> Int -> Direction -> [(Node, Int)]
nodesToUpdateInDirection minMomentum maxMove heatMap location heat direction = nextBlocks heatMap minMomentum maxMove location direction heat

updateReachable :: Reachable -> Node -> [(Node, Int)] -> Reachable
updateReachable reachable node = M.delete node . updateNeighbours reachable

isUnvisited :: S.Set (Coord, Orientation) -> M.Map (Coord, Orientation) a -> (Node, c) -> Bool
isUnvisited unvisited reachable (node, _) = S.member node unvisited || M.member node reachable

getNextNode :: M.Map Node Int -> (Node, Int)
getNextNode = minimumBy (compare `on` snd) . M.toList

markVisited ::Unvisited -> [(Node, a)] -> Unvisited
markVisited state = foldl (flip S.delete) state . map fst

updateNeighbours :: M.Map Node Int -> [(Node, Int)] -> M.Map Node Int
updateNeighbours = foldl updateNeighbour

updateNeighbour :: M.Map Node Int -> (Node, Int) -> M.Map Node Int
updateNeighbour state (node, heat) = M.insertWith min node heat state

move :: Direction -> Coord -> Coord
move DirDown (x,y) = (x, y+1)
move DirUp (x,y) = (x, y-1)
move DirLeft (x,y) = (x-1, y)
move DirRight (x,y) = (x+1, y)

nextBlocks :: Input -> Int -> Int -> Coord -> Direction -> Int -> [(Node, Int)]
nextBlocks input minMomentum maxMove start direction initHeat =
    let locations = moveContinuous direction start
        orientation = dirOrientation direction
    in  validStops minMomentum maxMove $ addHeat input orientation initHeat locations

addHeat :: Input -> Orientation -> Int -> [Coord] -> [(Node, Int)]
addHeat _ _ _ [] = []
addHeat input orientation heatTotal (location:locations) = case M.lookup location input of
    Nothing -> []
    Just heat -> (node, heatTotal') : addHeat input orientation heatTotal' locations
        where heatTotal' = heat + heatTotal
              node = (location, orientation)

moveContinuous :: Direction -> Coord -> [Coord]
moveContinuous direction = tail . iterate (move direction)

validStops :: Int -> Int -> [a] -> [a]
validStops minMomentum maxMove = take maxMove . drop minMomentum

turnDirs :: Orientation -> [Direction]
turnDirs Horizontal = [DirLeft, DirRight]
turnDirs Vertical = [DirUp, DirDown]

dirOrientation :: Direction -> Orientation
dirOrientation DirLeft = Vertical
dirOrientation DirRight = Vertical
dirOrientation DirUp = Horizontal
dirOrientation DirDown = Horizontal