module P8 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List (findIndex, elemIndex)
import Data.Maybe (fromJust)

data Direction = DirectionLeft | DirectionRight 
type Nodes = M.Map String (String, String)
data Input = Input [Direction] Nodes

run1 :: String -> Maybe Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input8"

parse :: String -> Input
parse = parseInput . lines

parseInput :: [String] -> Input
parseInput (directions:nodes) = Input (map parseDirection directions) (parseNodes nodes)
parseInput _ = error "Bad input"

parseDirection :: Char -> Direction
parseDirection 'L' = DirectionLeft
parseDirection 'R' = DirectionRight
parseDirection _ = error "Bad direction"

parseNodes :: [String] -> Nodes
parseNodes = M.fromList . map parseNode . tail

parseNode :: String -> (String, (String, String))
parseNode s = (parseNodeName s, (parseNodeLeft s, parseNodeRight s))

parseNodeName :: String -> String
parseNodeName = take 3

parseNodeLeft :: String -> String
parseNodeLeft = take 3 . drop 7

parseNodeRight :: String -> String
parseNodeRight = take 3 . drop 12

solve1 :: Input -> Maybe Int
solve1 = elemIndex "ZZZ" . navigateNetwork "AAA"

navigateNetwork :: String -> Input -> [String]
navigateNetwork startingNode (Input directions nodes) = scanl (step nodes) startingNode (cycle directions)

step :: Nodes -> String -> Direction -> String
step nodes location = stepDirection (nodes M.! location)

stepDirection :: (String, String) -> Direction -> String
stepDirection (left, _) DirectionLeft = left
stepDirection (_, right) DirectionRight = right

solve2 :: Input -> Int
solve2 input@(Input _ nodes) = findCommonCycle $ map (findCycle . (`navigateNetwork` input)) (startingNodes nodes)

findCycle :: [String] -> Int
findCycle = fromJust . findIndex endsInZ

endsInZ :: String -> Bool
endsInZ = (=='Z') . last

startingNodes :: Nodes -> [String]
startingNodes = filter ((=='A') . last) . M.keys

findCommonCycle :: [Int] -> Int
findCommonCycle = foldl lcm 1