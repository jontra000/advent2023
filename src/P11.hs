module P11 (run1, run2, inputLocation) where

type Coord = (Int, Int)
type Galaxies = [Coord]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse :: String -> Galaxies
parse = parseGalaxies . parseMap

parseGalaxies :: [(Coord, Char)] -> Galaxies
parseGalaxies = map fst . filter ((=='#') . snd)

parseMap :: String -> [(Coord, Char)]
parseMap = concat . zipWith parseLine [0..] . lines

parseLine :: Int -> String -> [((Int, Int), Char)]
parseLine y = zipWith (parseChar y) [0..]

parseChar :: Int -> Int -> Char -> ((Int, Int), Char)
parseChar y x c = ((x,y), c)

solve1 :: Galaxies -> Int
solve1 = solve 1

solve2 :: Galaxies -> Int
solve2 = solve 999999

solve :: Int -> Galaxies -> Int
solve expansion = sum . shortestDistances . expand expansion

expand :: Int -> Galaxies -> Galaxies
expand expansion galaxies =
    let yMin = minimum $ map snd galaxies
        yMax = maximum $ map snd galaxies
        xMin = minimum $ map fst galaxies
        xMax = maximum $ map fst galaxies
    in  expandDimension (expandRow expansion) yMin yMax $ expandDimension (expandColumn expansion) xMin xMax galaxies

expandDimension :: (Galaxies -> Int -> Galaxies) -> Int -> Int -> Galaxies -> Galaxies
expandDimension expandFunction dMin dMax galaxies  = foldl expandFunction galaxies (reverse [dMin..dMax])

expandColumn :: Int -> Galaxies -> Int -> Galaxies
expandColumn expansion galaxies x =
    if any ((==x) . fst) galaxies
    then galaxies
    else map (expandColumn' expansion x) galaxies

expandColumn' :: Int -> Int -> (Int, Int) -> (Int, Int)
expandColumn' expansion column c@(x, y)
    | x > column = (x+expansion, y)
    | otherwise = c

expandRow :: Int -> Galaxies -> Int -> Galaxies
expandRow expansion galaxies y =
    if any ((==y) . snd) galaxies
    then galaxies
    else map (expandRow' expansion y) galaxies

expandRow' :: Int -> Int -> (Int, Int) -> (Int, Int)
expandRow' expansion row c@(x,y)
    | y > row = (x,y+expansion)
    | otherwise = c

shortestDistances :: [(Int, Int)] -> [Int]
shortestDistances [] = []
shortestDistances (galaxy:res) = map (shortestDistance galaxy) res ++ shortestDistances res

shortestDistance :: (Int, Int) -> (Int, Int) -> Int
shortestDistance (x1,y1) (x2,y2) = abs(x2-x1) + abs(y2-y1)