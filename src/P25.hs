{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P25 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: b -> Int
run2 = const 0

inputLocation :: String
inputLocation = "inputs/input25"

parse :: String -> M.Map String [String]
parse = M.unionsWith (++) . map (parseLine . words) . lines

parseLine :: [String] -> M.Map String [String]
parseLine (name:connections) = M.fromList $ (name', connections) : reverseConnections name' connections
    where name' = init name
parseLine _ = error "bad input"

reverseConnections :: a1 -> [a2] -> [(a2, [a1])]
reverseConnections name = map (\connection -> (connection, [name]))

solve1 :: M.Map String [String] -> Int
solve1 input = score input $ head $ mapMaybe (findThreeConnections input . (:[])) $ M.keys input

findThreeConnections :: M.Map String [String] -> [String] -> Maybe ( M.Map String [String])
findThreeConnections input c@(connection:connections)
    | length c == 3 = Just input
    | otherwise = findThreeConnections' connections' input' (M.lookup connection input)
            where input' = M.delete connection input
                  connections' = filter (/= connection) connections
findThreeConnections _ _ = Nothing

findThreeConnections' :: [String] -> M.Map String [String] -> Maybe [String] -> Maybe (M.Map String [String])
findThreeConnections' connections input = findThreeConnections input . (connections ++) . filter (`M.member` input) . fromMaybe []

score ::  M.Map String [String] -> M.Map String [String] -> Int
score input1 input2 = count * (length input1 - count)
    where count = length input2