module P5 (run1, run2, inputLocation) where
import Data.List.Split (splitOn)
import Data.List (sort)

data Range = Range Int Int Int deriving (Eq, Ord)
type Mapping = [Range]
data Input = Input [Int] [Mapping]
data ReverseMappingResult = ReverseMappingResult Int Int deriving Show -- result, to skip on next try

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input5"

parse :: String -> Input
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks (seedBlock:mapBlocks) = Input (parseSeeds seedBlock) (parseMaps mapBlocks)
parseBlocks _ = error "empty input"

parseSeeds :: [String] -> [Int]
parseSeeds = map read . tail . words . head

parseMaps ::[[String]] -> [Mapping]
parseMaps = map parseMap

parseMap :: [String] -> Mapping
parseMap = sort . map parseRange . tail

parseRange :: String -> Range
parseRange = range . map read . words

range :: [Int] -> Range
range [destStart, sourceStart, rangeLength] = Range destStart sourceStart rangeLength
range _ = error "bad range definition"

solve1 :: Input -> Int
solve1 = minimum . seedLocations

seedLocations :: Input -> [Int]
seedLocations (Input seeds mappings) = map (applyMappings mappings) seeds

applyMappings :: [Mapping] -> Int -> Int
applyMappings mapping seed = foldl applyMapping seed mapping

applyMapping :: Int -> [Range] -> Int
applyMapping x [] = x
applyMapping x ((Range destStart sourceStart rangeLength):ranges)
    | inRange sourceStart rangeLength x = translateRange sourceStart destStart x
    | otherwise = applyMapping x ranges

inRange :: Int -> Int -> Int -> Bool
inRange sourceStart rangeLength x = x >= sourceStart && x < sourceStart + rangeLength

translateRange :: Int -> Int -> Int -> Int
translateRange sourceStart destStart x = destStart + x - sourceStart

solve2 :: Input -> Int
solve2 = solveBackwards 0 . reverseMappings

reverseMappings :: Input -> Input
reverseMappings (Input seeds mappings) = Input seeds (reverse mappings)

solveBackwards :: Int -> Input -> Int
solveBackwards guess input@(Input seeds mappings) =
    let ReverseMappingResult result toSkip = applyMappingsReversed mappings guess
    in  if isSeed result seeds
        then guess
        else solveBackwards (guess + toSkip) input

isSeed :: Int -> [Int] -> Bool
isSeed x = any (inSeedRange x) . chunkPairs

inSeedRange :: Int -> (Int, Int) -> Bool
inSeedRange x (start, rangeLength) = inRange start rangeLength x

chunkPairs :: [a] -> [(a,a)]
chunkPairs (x:y:res) = (x,y) : chunkPairs res
chunkPairs _ = []

applyMappingsReversed :: [Mapping] -> Int -> ReverseMappingResult
applyMappingsReversed mapping x = foldl applyMappingReverse (ReverseMappingResult x maxBound) mapping

applyMappingReverse :: ReverseMappingResult -> [Range] -> ReverseMappingResult
applyMappingReverse (ReverseMappingResult x toSkip) ranges =
    let (lowerRanges, upperRanges) = span (\(Range start _ _) -> start <= x) ranges
        toSkip' = min toSkip (rangeDistance x upperRanges)
    in  applyMappingReverse' x toSkip' lowerRanges

applyMappingReverse' :: Int -> Int -> [Range] -> ReverseMappingResult
applyMappingReverse' x toSkip [] =  ReverseMappingResult x toSkip
applyMappingReverse' x toSkip lowerRanges
    | x < sourceStart + rangeLength = ReverseMappingResult x' toSkip'
    | otherwise = ReverseMappingResult x toSkip
    where Range sourceStart destStart rangeLength = last lowerRanges
          x' = translateRange sourceStart destStart x
          distanceToRangeEnd = sourceStart + rangeLength - x
          toSkip' = min toSkip distanceToRangeEnd

rangeDistance :: Int -> [Range] -> Int
rangeDistance _ [] = maxBound
rangeDistance x (Range sourceStart _ _:_) = sourceStart - x
