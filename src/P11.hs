module P11 (run1, run2, inputLocation) where

import qualified Data.Set as S

type Galaxies = S.Set (Int, Int)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse :: String -> Galaxies
parse = S.fromList . map fst . filter ((=='#') . snd) . concat . zipWith (\y s -> zipWith (\x c -> ((x,y), c)) [0..] s) [0..] . lines

solve1 :: Galaxies -> Int
solve1 = sum . shortestDistances . S.toList . expand 1

solve2 :: Galaxies -> Int
solve2 = sum . shortestDistances . S.toList . expand 999999

expand :: Int -> Galaxies -> Galaxies
expand expansion galaxies =
    let yMin = S.findMin $ S.map snd galaxies
        yMax = S.findMax $ S.map snd galaxies
        xMin = S.findMin $ S.map fst galaxies
        xMax = S.findMax $ S.map fst galaxies
    in  expandRows expansion yMin yMax $ expandColumns expansion xMin xMax galaxies

expandColumns :: Int -> Int -> Int -> Galaxies -> Galaxies
expandColumns expansion xMin xMax galaxies = foldl (expandColumn expansion) galaxies (reverse [xMin..xMax])

expandColumn :: Int -> Galaxies -> Int -> Galaxies
expandColumn expansion galaxies x =
    if any ((==x) . fst) galaxies
    then galaxies
    else S.map (expandColumn' expansion x) galaxies

expandColumn' :: Int -> Int -> (Int, Int) -> (Int, Int)
expandColumn' expansion column c@(x, y)
    | x > column = (x+expansion, y)
    | otherwise = c

expandRows :: Int -> Int -> Int -> Galaxies -> Galaxies
expandRows expansion yMin yMax galaxies = foldl (expandRow expansion) galaxies (reverse [yMin..yMax])

expandRow :: Int -> Galaxies -> Int -> Galaxies
expandRow expansion galaxies y =
    if any ((==y) . snd) galaxies
    then galaxies
    else S.map (expandRow' expansion y) galaxies

expandRow' :: Int -> Int -> (Int, Int) -> (Int, Int)
expandRow' expansion row c@(x,y)
    | y > row = (x,y+expansion)
    | otherwise = c

shortestDistances :: [(Int, Int)] -> [Int]
shortestDistances [] = []
shortestDistances (galaxy:res) = map (shortestDistance galaxy) res ++ shortestDistances res

shortestDistance :: (Int, Int) -> (Int, Int) -> Int
shortestDistance (x1,y1) (x2,y2) = abs(x2-x1) + abs(y2-y1)