module P6 (run1, run2, inputLocation) where
import Data.List (singleton)

data Race = Race Int Int -- time distance
type Input = [Race]

run1 :: String -> Int
run1 = solve . parse1

run2 :: String -> Int
run2 = solve . parse2

inputLocation :: String
inputLocation = "inputs/input6"

parse1 :: String -> Input
parse1 = map race . zipTwo . map parseRow . lines

parseRow :: String -> [Int]
parseRow = map read . tail . words

zipTwo :: [[Int]] -> [(Int, Int)]
zipTwo (a:b:_) = zip a b
zipTwo _ = error "bad input"

race :: (Int, Int) -> Race
race (time, distance) = Race time distance

solve :: [Race] -> Int
solve = product . map waysToBeat

waysToBeat :: Race -> Int
waysToBeat (Race time distance) =
    let (rootA, rootB) = quadratic time (distance + 1)
        rootARounded = ceiling rootA
        rootBRounded = floor rootB
    in  rootBRounded - rootARounded + 1

quadratic :: Int -> Int -> (Double, Double)
quadratic time distance =
    let root = sqrt $ fromIntegral (time * time - 4 * distance)
    in  ((fromIntegral time - root) / 2.0, (fromIntegral time + root) / 2.0)

parse2 :: String -> [Race]
parse2 = map race . zipTwo . map parseRow2 . lines

parseRow2 :: String -> [Int]
parseRow2 = singleton . read . concat . tail . words
