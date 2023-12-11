module P10 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (findIndex, find)
import Data.Maybe (fromJust)

type PipeMap = M.Map (Int, Int) Char
data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse :: String -> PipeMap
parse = M.fromList . concatMap (\(y, line) -> zipWith (\x c -> ((x,y),c)) [0..] line) . zip [0..] . lines 

solve1 :: PipeMap -> Int
solve1 = furthestDistance . tunnelLength
-- solve1 pipes = take 10 $ map (fst) $ tunnelSequence pipes

tunnelLength :: PipeMap -> Int
tunnelLength pipes = fromJust $ findIndex ((=='S') . (pipes M.!) . fst) $ tunnelSequence pipes

tunnelSequence :: PipeMap -> [((Int, Int), Direction)]
tunnelSequence pipes = iterate (nextPipe pipes) (initialStep pipes)

initialStep :: PipeMap -> ((Int, Int), Direction)
initialStep pipes = getNeighbour pipes $ startingPosition pipes

startingPosition :: PipeMap -> (Int, Int)
startingPosition = fst . fromJust . find ((=='S') . snd) . M.toList

getNeighbour :: PipeMap -> (Int, Int) -> ((Int, Int), Direction)
getNeighbour pipes = fromJust . find (isLink pipes) . neighbourCoords

neighbourCoords :: (Int, Int) -> [((Int, Int), Direction)]
neighbourCoords (x,y) = [((x, y-1), DirUp), ((x+1,y), DirRight), ((x,y+1), DirDown), ((x-1, y), DirLeft)]

isLink :: PipeMap -> ((Int, Int), Direction) -> Bool
isLink pipes (c, DirUp) = M.lookup c pipes `elem` [Just '|', Just '7', Just 'F']
isLink pipes (c, DirDown) = M.lookup c pipes `elem` [Just '|', Just 'J', Just 'L']
isLink pipes (c, DirLeft) = M.lookup c pipes `elem` [Just '-', Just 'L', Just 'F']
isLink pipes (c, DirRight) = M.lookup c pipes `elem` [Just '-', Just '7', Just 'J']

nextPipe :: PipeMap -> ((Int, Int), Direction) -> ((Int, Int), Direction)
nextPipe pipes (c, DirUp) =
    case pipes M.! c of
        '7' -> (moveLeft c, DirLeft)
        'F' -> (moveRight c, DirRight)
        '|' -> (moveUp c, DirUp)
        _ -> error "Bad pipe match"
nextPipe pipes (c, DirDown) =
    case pipes M.! c of
        '|' -> (moveDown c, DirDown)
        'J' -> (moveLeft c, DirLeft)
        'L' -> (moveRight c, DirRight)
        _ -> error "Bad pipe match"
nextPipe pipes (c, DirRight) =
    case pipes M.! c of
        '7' -> (moveDown c, DirDown)
        'J' -> (moveUp c, DirUp)
        '-' -> (moveRight c, DirRight)
        _ -> error "Bad pipe match"
nextPipe pipes (c, DirLeft) =
    case pipes M.! c of
        'L' -> (moveUp c, DirUp)
        'F' -> (moveDown c, DirDown)
        '-' -> (moveLeft c, DirLeft)
        _ -> error "Bad pipe match"

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (x,y) = (x-1, y)

moveRight :: (Int, Int) -> (Int, Int)
moveRight (x,y) = (x + 1, y)

moveUp :: (Int, Int) -> (Int, Int)
moveUp (x,y) = (x, y-1)

moveDown :: (Int, Int) -> (Int, Int)
moveDown (x,y) = (x, y+1)

furthestDistance :: Int -> Int
furthestDistance x = (x+1) `div` 2

solve2 :: PipeMap -> Int
solve2 pipes =
    let borderRepeating = tunnelSequence pipes
        borderLength = 1 + fromJust (findIndex ((=='S') . (pipes M.!) . fst) borderRepeating)
        borderCoords = take borderLength borderRepeating
        filledArea = foldl tryFill (S.fromList (map fst borderCoords)) (map (addPipe pipes) borderCoords)
    in  length filledArea - borderLength
    -- in  plot filledArea

addPipe :: PipeMap -> ((Int, Int), Direction) -> ((Int, Int), Direction, Char)
addPipe pipes (c, dir) = (c, dir, pipes M.! c)

tryFill :: S.Set (Int, Int) -> ((Int, Int), Direction, Char) -> S.Set (Int, Int)
tryFill countedCoords (c, dir, pipe) = foldl flood countedCoords (innerNeighbours dir pipe c)

innerNeighbours :: Direction -> Char -> (Int, Int) -> [(Int, Int)]
innerNeighbours DirRight '7' c = [moveUp c, moveRight c]
innerNeighbours DirUp '7' _ = []
innerNeighbours DirUp 'F' c = [moveUp c, moveLeft c]
innerNeighbours DirLeft 'F' _ = []
innerNeighbours DirDown 'J' c = [moveRight c, moveDown c]
innerNeighbours DirRight 'J' _ = []
innerNeighbours DirDown 'L' _ = []
innerNeighbours DirLeft 'L' c = [moveDown c, moveLeft c]
innerNeighbours DirDown '|' c = [moveRight c]
innerNeighbours DirUp '|' c = [moveLeft c]
innerNeighbours DirLeft '-' c = [moveDown c]
innerNeighbours DirRight '-' c = [moveUp c]
innerNeighbours _ 'S' _ = []
innerNeighbours dir c _ = error ("Bad pipe connection: " ++ show c ++ " " ++ show dir)

flood :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
flood filledCoords c@(x,y) =
    if S.member c filledCoords || x < 0 || y < 0 || x > 142 || y > 142
    then filledCoords
    else foldl flood (S.insert c filledCoords) ([moveLeft c, moveRight c, moveUp c, moveDown c] ++ [(x-1,y-1), (x+1, y-1), (x-1,y+1),(x+1,y+1)])