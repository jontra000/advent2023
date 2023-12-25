module Lib (memoize, dijkstra, textToCoordMap, Coord) where

import qualified Data.Map as M

type Coord = (Int, Int)

textToCoordMap :: String -> M.Map Coord Char
textToCoordMap = M.fromList . concat . zipWith (\y line -> zipWith(\x c -> ((x,y), c)) [0..] line) [0..] . lines

type DistanceMap a = M.Map a Int
data Node a = Node Int (DistanceMap a)
type UnvisitedNodes a = M.Map a (Maybe Int)
type Nodes a = M.Map a (Node a)

dijkstra :: Ord a => Nodes a -> a -> a -> Maybe Int
dijkstra nodes start = dijkstra' nodes (M.map (const Nothing) nodes) (Just (start, 0))

dijkstra' :: Ord a => Nodes a -> UnvisitedNodes a -> Maybe (a, Int) -> a -> Maybe Int
dijkstra' _ _ Nothing _ = Nothing
dijkstra' nodes unvisitedNodes (Just (currentNode, currentDistance)) targetNode
    | currentNode == targetNode = Just currentDistance
    | otherwise = 
        let unvisitedNodes' = updateUnvisited nodes currentDistance currentNode unvisitedNodes
        in  dijkstraStep nodes targetNode unvisitedNodes'

dijkstraStep :: Ord a => Nodes a -> a -> UnvisitedNodes a -> Maybe Int
dijkstraStep input targetNode unvisitedNodes =
    let nextNode = smallestDistance unvisitedNodes
    in  dijkstra' input unvisitedNodes nextNode targetNode

updateUnvisited :: Ord a => Nodes a -> Int -> a -> UnvisitedNodes a -> UnvisitedNodes a
updateUnvisited nodes currentDistance currentNode unvisitedNodes = M.delete currentNode $ updateNeighbours nodes currentDistance unvisitedNodes currentNode

updateNeighbours :: Ord a => Nodes a -> Int -> UnvisitedNodes a -> a -> UnvisitedNodes a
updateNeighbours nodes currentDistance unvisitedNodes = foldl (updateNeighbour currentDistance) unvisitedNodes . M.toList . reachableNeighbours nodes

reachableNeighbours :: Ord a => Nodes a -> a -> DistanceMap a
reachableNeighbours input nodeName = case input M.! nodeName of (Node _ neighbours) -> neighbours

updateNeighbour :: Ord a => Int -> UnvisitedNodes a -> (a, Int) -> UnvisitedNodes a
updateNeighbour currentDistance unvisitedNodes (key, distance) =
    M.adjust (updateDistance (currentDistance + distance)) key unvisitedNodes

smallestDistance :: UnvisitedNodes a -> Maybe (a, Int)
smallestDistance = M.foldlWithKey minTentativeDistance Nothing

updateDistance :: Int -> Maybe Int -> Maybe Int
updateDistance d Nothing = Just d
updateDistance dNew (Just dOld) = Just (min dNew dOld)

minTentativeDistance :: Maybe (a, Int) -> a -> Maybe Int -> Maybe (a, Int)
minTentativeDistance prev _ Nothing = prev
minTentativeDistance Nothing key (Just tentativeDistance) = Just (key, tentativeDistance)
minTentativeDistance prev@(Just (_, prevDistance)) key (Just tentativeDistance)
    | tentativeDistance < prevDistance = Just (key, tentativeDistance)
    | otherwise = prev
    
memoize :: Ord a => M.Map a b -> a -> (b, M.Map a b) -> (b, M.Map a b)
memoize cache key f = 
    case M.lookup key cache of
        Just x -> (x, cache)
        Nothing ->
            let (result, cache') = f
            in  (result, M.insert key result cache')