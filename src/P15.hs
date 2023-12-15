module P15 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (ord)

type State = M.Map Int [(String, Int)]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input15"

parse :: String -> [String]
parse = splitOn ","

solve1 :: [String] -> Int
solve1 = sum . map hash

hash :: String -> Int
hash = foldl hashChar 0 

hashChar :: Int -> Char -> Int
hashChar x c = ((x + ord c) * 17) `mod` 256

solve2 :: [String] -> Int
solve2 = sum . map focusingPower . M.toList . installLenses

installLenses :: [String] -> State
installLenses = foldl installLens M.empty

installLens :: State -> String -> State
installLens state command
    | last command == '-' = removeLens state command
    | otherwise = addLens state command

removeLens :: State -> String ->  State
removeLens state command =
    let label = init command
        box = hash label
    in  M.adjust (filter ((/=label) . fst)) box state

addLens :: State -> String -> State
addLens state command =
    case break (=='=') command of
        (label, _:focalLength) ->
            let box = hash label
            in  M.alter (addLens' label (read focalLength)) box state
        _ -> error "bad command"

addLens' :: String -> Int -> Maybe [(String, Int)] -> Maybe [(String, Int)]
addLens' label focalLength (Just existing) =
    if any ((==label).fst) existing
    then Just $ map (\old@(label',_) -> if label' == label then (label, focalLength) else old) existing
    else Just $ (label, focalLength) : existing
addLens' label focalLength Nothing = Just [(label, focalLength)]

focusingPower :: (Int, [(String, Int)]) -> Int
focusingPower (box, labels) =
    sum $ zipWith (\i (_,focalLength) -> (box+1) * i * focalLength) [1..] $ reverse labels