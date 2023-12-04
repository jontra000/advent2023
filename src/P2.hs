module P2 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

type CubeSet = M.Map Char Int
data Draw = Draw Char Int
data Game = Game { gameId :: Int, gameDraws :: [Draw] }
type Input = [Game]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse :: String -> Input
parse = map (parseGame . words) . lines

parseGame :: [String] -> Game
parseGame (_:gameStr:res) = Game (parseGameId gameStr) (parseDraws res)
parseGame x = error ("Malformed game string: " ++ unwords x)

parseGameId :: String -> Int
parseGameId = read . init

parseDraws :: [String] -> [Draw]
parseDraws (amountStr:colourStr:res) = parseDraw amountStr colourStr : parseDraws res
parseDraws _ = []

parseDraw :: String -> String -> Draw
parseDraw amountStr colourStr = Draw (head colourStr) (read amountStr)

solve1 :: Input -> Int
solve1 = sum . map gameId . filter checkPossible

checkPossible :: Game -> Bool
checkPossible (Game _ draws) = all isPossible draws

isPossible :: Draw -> Bool
isPossible (Draw 'r' amount) = amount <= 12
isPossible (Draw 'g' amount) = amount <= 13
isPossible (Draw 'b' amount) = amount <= 14
isPossible _ = True

solve2 :: Input -> Int
solve2 = sum . map gamePower

gamePower :: Game -> Int
gamePower = power . minimumCubeSet . gameDraws

minimumCubeSet :: [Draw] -> CubeSet
minimumCubeSet = M.fromListWith max . map (\(Draw colour amount) -> (colour, amount))

power :: CubeSet -> Int
power cubeSet = product $ mapMaybe (`M.lookup` cubeSet) "rgb"