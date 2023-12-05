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
seedLocations (Input seeds mappings) = map (seedLocation mappings) seeds

seedLocation :: [Mapping] -> Int -> Int
seedLocation mapping seed = foldl applyMapping seed mapping

applyMapping :: Int -> [Range] -> Int
applyMapping seed [] = seed
applyMapping seed ((Range destStart sourceStart rangeLength):ranges)
    | seed >= sourceStart && seed < sourceStart + rangeLength = destStart + seed - sourceStart
    | otherwise = applyMapping seed ranges

solve2 :: Input -> Int
solve2 = solveBackwards 0 . transformSeedRanges

transformSeedRanges :: Input -> Input
transformSeedRanges (Input seeds mappings) = Input seeds (reverse mappings)

solveBackwards :: Int -> Input -> Int
solveBackwards guess input@(Input seeds mappings) =
    let ReverseMappingResult result toSkip = reverseMappings mappings guess
    in  if isInSeedRange seeds result
        then guess
        else solveBackwards (guess + toSkip) input

isInSeedRange :: [Int] -> Int -> Bool
isInSeedRange (start:rangeLength:res) x = (x >= start && x < start + rangeLength) || isInSeedRange res x
isInSeedRange _ _ = False 

reverseMappings :: [Mapping] -> Int -> ReverseMappingResult
reverseMappings mapping x = foldl applyMappingReverse (ReverseMappingResult x maxBound) mapping

applyMappingReverse :: ReverseMappingResult -> [Range] -> ReverseMappingResult
applyMappingReverse (ReverseMappingResult x toSkip) ranges =
    let (lowerRanges, upperRanges) = span (\(Range start _ _) -> start <= x) ranges
        toSkip' = min toSkip (rangeDistance x upperRanges)
    in  case reverse lowerRanges of
            [] -> ReverseMappingResult x toSkip'
            ((Range rangeStart mapStart rangeLength):_) -> 
                if x < rangeStart + rangeLength
                then ReverseMappingResult (mapStart + x - rangeStart) (min toSkip (rangeStart + rangeLength - x))
                else ReverseMappingResult x toSkip'

rangeDistance :: Int -> [Range] -> Int
rangeDistance _ [] = maxBound
rangeDistance x (Range rangeStart _ _:_) = rangeStart - x
