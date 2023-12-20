module P15 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (ord)

type Box = [(String, Int)]
type State = M.Map Int Box

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
hashChar acc = (`mod` 256) . (*17) . (+ acc) . ord

solve2 :: [String] -> Int
solve2 = sum . map focusingPower . M.toList . installLenses

installLenses :: [String] -> State
installLenses = foldl (flip installLens) M.empty

installLens :: String -> State -> State
installLens command
    | last command == '-' = removeLens (init command)
    | otherwise = addLens command

removeLens :: String -> State -> State
removeLens label = M.adjust (removeLabel label) box
    where box = hash label

removeLabel :: String -> Box -> Box
removeLabel label = filter ((/=label) . fst)

addLens :: String -> State -> State
addLens command =
    case break (=='=') command of
        (label, _:focalLength) -> addLens' label (read focalLength)
        _ -> error "bad command"

addLens' :: String -> Int -> State -> State
addLens' label focalLength = M.alter (Just . alterBox label focalLength) box
    where box = hash label

alterBox :: String -> Int -> Maybe Box -> Box
alterBox label focalLength (Just existing)
    | labelExists label existing = updateFocalLength label focalLength existing
    | otherwise = (label, focalLength) : existing
alterBox label focalLength Nothing = [(label, focalLength)]

labelExists :: String -> Box -> Bool
labelExists label = any ((==label) . fst)

updateFocalLength :: String -> Int -> Box -> Box
updateFocalLength label focalLength = map (updateLabel label focalLength)

updateLabel :: String -> Int -> (String, Int) -> (String, Int)
updateLabel label focalLength old@(label',_)
    | label' == label = (label, focalLength)
    | otherwise = old

focusingPower :: (Int, Box) -> Int
focusingPower (boxIndex, labels) = boxFocusingPower boxIndex labels

boxFocusingPower :: Int -> Box -> Int
boxFocusingPower boxIndex = sum . zipWith (lensFocusingPower boxIndex) [1..] . reverse

lensFocusingPower :: Int -> Int -> (a, Int) -> Int
lensFocusingPower boxIndex lensIndex (_, focalLength) = (boxIndex + 1) * lensIndex * focalLength