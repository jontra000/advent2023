module P12 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.Map as M

data Input = Input String [Int]

type Cache = M.Map (String, [Int]) Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input12"

parse :: String -> [Input]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Input
parseLine (springsStr:blockStr:_) = Input springsStr (parseBlocks blockStr)
parseLine _ = error "bad input"

parseBlocks :: String -> [Int]
parseBlocks = map read . splitOn ","

solve1 :: [Input] -> Int
solve1 = sum . map (fst . solutions)

solve2 :: [Input] -> Int
solve2 = sum . map (fst . solutions . unfoldRecord)

unfoldRecord :: Input -> Input
unfoldRecord (Input pattern blocks) = Input (unfoldPattern pattern) (unfoldBlocks blocks)

unfoldPattern :: String -> String
unfoldPattern = intercalate "?" . replicate 5

unfoldBlocks :: [Int] -> [Int]
unfoldBlocks = concat . replicate 5

solutions :: Input -> (Int, Cache)
solutions (Input springs blocks) = countSolutions M.empty springs blocks

countSolutions :: Cache -> String -> [Int] -> (Int, Cache)
countSolutions cache pattern []
    | '#' `elem` pattern = (0, cache)
    | otherwise = (1, cache)
countSolutions cache [] _ = (0, cache)
countSolutions cache ('.':pattern) blocks = countSolutions cache pattern blocks
countSolutions cache ('#':pattern) (nextBlock:blocks) = placeBlock cache pattern (nextBlock-1) blocks
countSolutions cache (_:pattern) blocks@(nextBlock:remBlocks) = memoize cache (pattern, blocks) (processQ cache pattern blocks nextBlock remBlocks)

memoize :: Cache -> (String, [Int]) -> (Int, Cache) -> (Int, Cache)
memoize cache key f = 
    case M.lookup key cache of
        Just x -> (x, cache)
        Nothing ->
            let (result, cache') = f
            in  (result, M.insert key result cache')

processQ :: Cache -> String -> [Int] -> Int -> [Int] -> (Int, Cache)
processQ cache pattern blocks nextBlock remBlocks =
    let (count1, cache1) = placeBlock cache pattern (nextBlock-1) remBlocks 
        (count2, cache2) = countSolutions cache1 pattern blocks
    in  (count1 + count2, cache2)

placeBlock :: Cache -> String -> Int -> [Int] -> (Int, Cache)
placeBlock cache pattern 0 blocks = placeSpace cache pattern blocks
placeBlock cache [] _ _ = (0, cache)
placeBlock cache ('.':_) _ _ = (0, cache)
placeBlock cache pattern nextBlock blocks = placeBlock cache (tail pattern) (nextBlock - 1) blocks

placeSpace :: Cache -> String -> [Int] -> (Int, Cache)    
placeSpace cache [] [] = (1, cache)
placeSpace cache [] _ = (0, cache)
placeSpace cache ('#':_) _ = (0, cache)
placeSpace cache (_:pattern) blocks = countSolutions cache pattern blocks