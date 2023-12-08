module P8 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List (findIndex, elemIndex)

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
parseNode (a:b:c:_:_:_:_:d:e:f:_:_:g:h:i:_) = ([a,b,c], ([d,e,f],[g,h,i]))
parseNode _ = error "Bad node definition"

solve1 :: Input -> Maybe Int
solve1 (Input directions nodes) = elemIndex "ZZZ" $ scanl (step nodes) "AAA" (cycle directions)

step :: Nodes -> String -> Direction -> String
step nodes location = stepDirection (nodes M.! location)

stepDirection :: (String, String) -> Direction -> String
stepDirection (left, _) DirectionLeft = left
stepDirection (_, right) DirectionRight = right

solve2 :: Input -> Int
solve2 (Input directions nodes) = findCommonCycle $ map (findCycle . \node -> scanl (step nodes) node (cycle directions)) (startingNodes nodes)

findCycle :: [String] -> Int
findCycle xs =
    case findIndex endsInZ xs of
        Just cycleInit -> cycleInit
        _ -> error "No solution"

endsInZ :: String -> Bool
endsInZ = (=='Z') . last

startingNodes :: Nodes -> [String]
startingNodes = filter ((=='A') . last) . M.keys

findCommonCycle :: [Int] -> Int
findCommonCycle = foldl lcm 1