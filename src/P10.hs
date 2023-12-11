module P10 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (fromJust)

type Coord = (Int, Int)
type PipeMap = M.Map Coord Char
data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show
data Pipe = Pipe Coord Direction Char

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse :: String -> PipeMap
parse = M.fromList . concatMap (\(y, line) -> zipWith (\x c -> ((x,y),c)) [0..] line) . zip [0..] . lines 

solve1 :: PipeMap -> Int
solve1 = maxDistance . length . tunnelSequence

maxDistance :: Int -> Int
maxDistance tunnelLength = tunnelLength `div` 2 + tunnelLength `mod` 2

tunnelSequence :: PipeMap -> [Pipe]
tunnelSequence pipes = nextPipe pipes (initialStep pipes)

initialStep :: PipeMap -> Pipe
initialStep pipes = startingPipe pipes $ startingPosition pipes

startingPosition :: PipeMap -> Coord
startingPosition = fst . fromJust . find ((=='S') . snd) . M.toList

isLink :: PipeMap -> Coord -> Direction -> Bool
isLink pipes c DirUp = M.lookup c pipes `elem` [Just '|', Just '7', Just 'F']
isLink pipes c DirDown = M.lookup c pipes `elem` [Just '|', Just 'J', Just 'L']
isLink pipes c DirLeft = M.lookup c pipes `elem` [Just '-', Just 'L', Just 'F']
isLink pipes c DirRight = M.lookup c pipes `elem` [Just '-', Just '7', Just 'J']

startingPipe :: PipeMap -> Coord -> Pipe
startingPipe pipes c = Pipe c (head links) (toPipe links)
    where links = filter (\dir -> isLink pipes (move dir c) dir) [DirUp, DirRight, DirDown, DirLeft]

toPipe :: [Direction] -> Char
toPipe [DirUp, DirRight] = 'L'
toPipe [DirUp, DirDown] = '|'
toPipe [DirUp, DirLeft] = 'J'
toPipe [DirRight, DirDown] = 'F'
toPipe [DirRight, DirLeft] = '-'
toPipe [DirDown, DirLeft] = '7'
toPipe _ = error "bad pipe connections" 

nextPipe :: PipeMap -> Pipe -> [Pipe]
nextPipe pipes prev@(Pipe c dir _)
    | pipe == 'S' = [prev]
    | otherwise = prev : nextPipe pipes next    
    where c' = move dir c
          pipe = pipes M.! c'
          next = Pipe c' (nextDir pipe dir) pipe

nextDir :: Char -> Direction -> Direction
nextDir c DirUp =
    case c of
        '7' -> DirLeft
        'F' -> DirRight
        '|' -> DirUp
        _ -> error "Bad pipe match"
nextDir c DirDown =
    case c of
        '|' -> DirDown
        'J' -> DirLeft
        'L' -> DirRight
        _ -> error "Bad pipe match"
nextDir c DirRight =
    case c of
        '7' -> DirDown
        'J' -> DirUp
        '-' -> DirRight
        _ -> error "Bad pipe match"
nextDir c DirLeft =
    case c of
        'L' -> DirUp
        'F' -> DirDown
        '-' -> DirLeft
        _ -> error "Bad pipe match"

move :: Direction -> (Int, Int) -> (Int, Int)
move DirUp = moveUp
move DirDown = moveDown
move DirLeft = moveLeft
move DirRight = moveRight

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (x,y) = (x-1, y)

moveRight :: (Int, Int) -> (Int, Int)
moveRight (x,y) = (x + 1, y)

moveUp :: (Int, Int) -> (Int, Int)
moveUp (x,y) = (x, y-1)

moveDown :: (Int, Int) -> (Int, Int)
moveDown (x,y) = (x, y+1)

solve2 :: PipeMap -> Int
solve2 pipes =
    let borderCoords = tunnelSequence pipes
        filledArea = foldl tryFill (S.fromList (map location borderCoords)) borderCoords
    in  length filledArea - length borderCoords

location :: Pipe -> Coord
location (Pipe c _ _) = c

tryFill :: S.Set (Int, Int) -> Pipe -> S.Set (Int, Int)
tryFill countedCoords (Pipe c dir pipe) = foldl flood countedCoords (innerNeighbours dir pipe c)

innerNeighbours :: Direction -> Char -> (Int, Int) -> [(Int, Int)]
innerNeighbours DirDown '7' c = [moveUp c, moveRight c]
innerNeighbours DirLeft '7' _ = []
innerNeighbours DirRight 'F' c = [moveUp c, moveLeft c]
innerNeighbours DirDown 'F' _ = []
innerNeighbours DirLeft 'J' c = [moveRight c, moveDown c]
innerNeighbours DirUp 'J' _ = []
innerNeighbours DirRight 'L' _ = []
innerNeighbours DirUp 'L' c = [moveDown c, moveLeft c]
innerNeighbours DirDown '|' c = [moveRight c]
innerNeighbours DirUp '|' c = [moveLeft c]
innerNeighbours DirLeft '-' c = [moveDown c]
innerNeighbours DirRight '-' c = [moveUp c]
innerNeighbours dir c _ = error ("Bad pipe connection: " ++ show c ++ " " ++ show dir)

flood :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
flood filledCoords c@(x,y) =
    if S.member c filledCoords || x < 0 || y < 0 || x > 142 || y > 142
    then filledCoords
    else foldl flood (S.insert c filledCoords) ([moveLeft c, moveRight c, moveUp c, moveDown c] ++ [(x-1,y-1), (x+1, y-1), (x-1,y+1),(x+1,y+1)])