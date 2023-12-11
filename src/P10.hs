module P10 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)

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
        filledArea = head $ mapMaybe (\innerNeighbours -> foldl (tryFill innerNeighbours) (Just $ S.fromList (map location borderCoords)) borderCoords) [innerNeighboursLeft, innerNeighboursRight]
    in  length filledArea - length borderCoords

location :: Pipe -> Coord
location (Pipe c _ _) = c

tryFill :: (Direction -> Char -> (Int, Int) -> [(Int, Int)]) -> Maybe (S.Set (Int, Int)) -> Pipe -> Maybe (S.Set (Int, Int))
tryFill innerNeighbours countedCoords (Pipe c dir pipe) = foldl flood countedCoords (innerNeighbours dir pipe c)

innerNeighboursLeft :: Direction -> Char -> (Int, Int) -> [(Int, Int)]
innerNeighboursLeft DirDown '7' c = [moveUp c, moveRight c]
innerNeighboursLeft DirLeft '7' _ = []
innerNeighboursLeft DirRight 'F' c = [moveUp c, moveLeft c]
innerNeighboursLeft DirDown 'F' _ = []
innerNeighboursLeft DirLeft 'J' c = [moveRight c, moveDown c]
innerNeighboursLeft DirUp 'J' _ = []
innerNeighboursLeft DirRight 'L' _ = []
innerNeighboursLeft DirUp 'L' c = [moveDown c, moveLeft c]
innerNeighboursLeft DirDown '|' c = [moveRight c]
innerNeighboursLeft DirUp '|' c = [moveLeft c]
innerNeighboursLeft DirLeft '-' c = [moveDown c]
innerNeighboursLeft DirRight '-' c = [moveUp c]
innerNeighboursLeft dir c _ = error ("Bad pipe connection: " ++ show c ++ " " ++ show dir)

innerNeighboursRight :: Direction -> Char -> (Int, Int) -> [(Int, Int)]
innerNeighboursRight DirLeft '7' c = [moveUp c, moveRight c]
innerNeighboursRight DirDown '7' _ = []
innerNeighboursRight DirDown 'F' c = [moveUp c, moveLeft c]
innerNeighboursRight DirRight 'F' _ = []
innerNeighboursRight DirUp 'J' c = [moveRight c, moveDown c]
innerNeighboursRight DirLeft 'J' _ = []
innerNeighboursRight DirUp 'L' _ = []
innerNeighboursRight DirRight 'L' c = [moveDown c, moveLeft c]
innerNeighboursRight DirUp '|' c = [moveRight c]
innerNeighboursRight DirDown '|' c = [moveLeft c]
innerNeighboursRight DirRight '-' c = [moveDown c]
innerNeighboursRight DirLeft '-' c = [moveUp c]
innerNeighboursRight dir c _ = error ("Bad pipe connection: " ++ show c ++ " " ++ show dir)

flood :: Maybe (S.Set (Int, Int)) -> (Int, Int) -> Maybe (S.Set (Int, Int))
flood Nothing _ = Nothing
flood (Just filledCoords) c@(x,y)
    | x < 0 || y < 0 || x > 142 || y > 142 = Nothing
    | S.member c filledCoords = Just filledCoords
    | otherwise = foldl flood (Just (S.insert c filledCoords)) ([moveLeft c, moveRight c, moveUp c, moveDown c] ++ [(x-1,y-1), (x+1, y-1), (x-1,y+1),(x+1,y+1)])