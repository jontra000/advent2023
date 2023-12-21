module P16 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap)

data Mirror = None | Horizontal | Vertical | TiltRight | TiltLeft
data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Ord, Eq)
type Coord = (Int, Int)
type Input = M.Map Coord Mirror
type Output = S.Set (Coord, Direction)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input16"

parse :: String -> Input
parse = M.map parseMirror . textToCoordMap

parseMirror :: Char -> Mirror
parseMirror '-' = Horizontal
parseMirror '|' = Vertical
parseMirror '/' = TiltRight
parseMirror '\\' = TiltLeft
parseMirror _ = None

solve1 :: Input -> Int
solve1 = solve (0,0) RightDir

solve :: Coord -> Direction -> Input -> Int
solve startLoc startDir = energisedCellCount . energise S.empty startLoc startDir

energisedCellCount :: Output -> Int
energisedCellCount = length . S.map fst

energise :: Output -> Coord -> Direction -> Input -> Output
energise state loc dir input
    | S.member (loc, dir) state = state
    | otherwise = maybe state (processCell state' loc dir input) (M.lookup loc input)
        where state' = S.insert (loc, dir) state

processCell :: Output -> Coord -> Direction -> Input -> Mirror -> Output
processCell state loc dir input TiltRight = rotateSlash state loc dir input
processCell state loc dir input TiltLeft = rotateCounterSlash state loc dir input
processCell state loc UpDir input Horizontal = move (move state loc RightDir input) loc LeftDir input
processCell state loc DownDir input Horizontal = move (move state loc RightDir input) loc LeftDir input
processCell state loc LeftDir input Vertical = move (move state loc DownDir input) loc UpDir input
processCell state loc RightDir input Vertical = move (move state loc DownDir input) loc UpDir input
processCell state loc dir input _ = move state loc dir input

rotateSlash :: Output -> Coord -> Direction -> Input -> Output
rotateSlash state loc UpDir input = move state loc RightDir input
rotateSlash state loc DownDir input = move state loc LeftDir input
rotateSlash state loc LeftDir input = move state loc DownDir input
rotateSlash state loc RightDir input = move state loc UpDir input

rotateCounterSlash :: Output -> Coord -> Direction -> Input -> Output
rotateCounterSlash state loc UpDir input = move state loc LeftDir input
rotateCounterSlash state loc DownDir input = move state loc RightDir input
rotateCounterSlash state loc LeftDir input = move state loc UpDir input
rotateCounterSlash state loc RightDir input = move state loc DownDir input

move :: Output -> Coord -> Direction -> Input -> Output
move state (x,y) UpDir input = energise state (x, y-1) UpDir input
move state (x,y) DownDir input = energise state (x, y+1) DownDir input
move state (x,y) LeftDir input = energise state (x-1, y) LeftDir input
move state (x,y) RightDir input = energise state (x+1, y) RightDir input

solve2 :: Input -> Int
solve2 input = maximum $ map (\(startLoc, startDir) -> solve startLoc startDir input) (entryPoints input)

entryPoints :: Input -> [(Coord, Direction)]
entryPoints input = left ++ right ++ up ++ down
    where maxX = maximum $ map fst $ M.keys input
          maxY = maximum $ map snd $ M.keys input
          left = map (\y -> ((0,y), RightDir)) [0..maxY]
          right = map (\y -> ((maxX, y), LeftDir)) [0..maxY]
          down = map (\x -> ((x,0), DownDir)) [0..maxX]
          up = map (\x -> ((x,maxY), UpDir)) [0..maxX]