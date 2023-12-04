module P4 (run1, run2, inputLocation) where

import Data.Char (isDigit)

type Input = [Card]
type CopiesWins = (Int, Int)

data Card = Card [Int] [Int] -- winning numbers, my numbers

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse :: String -> Input
parse = map parseLine . lines

parseLine :: String -> Card
parseLine = parseCard . stripCardId

stripCardId :: String -> String
stripCardId = tail . dropWhile (/= ':')

parseCard :: String -> Card
parseCard = makeCard . mapTuple parseNumberList . break (=='|')

parseNumberList :: String -> [Int]
parseNumberList = map read . words . removeSeparator

removeSeparator :: String -> String
removeSeparator = dropWhile (not . isDigit)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

makeCard :: ([Int], [Int]) -> Card
makeCard (winners, ours) = Card winners ours

solve1 :: Input -> Int
solve1 = sum . map (score . winningNumbers)

winningNumbers :: Card -> Int
winningNumbers (Card winners ours) = length $ filter (`elem` winners) ours

score :: Int -> Int
score x 
    | x <= 0 = 0
    | otherwise = 2^(x-1)

solve2 :: Input -> Int
solve2 = countCards . zip (repeat 1) . map winningNumbers

countCards :: [CopiesWins] -> Int
countCards [] = 0
countCards ((copies, wins):res) =
    let res' = updateCopies res copies wins
    in  copies + countCards res'

updateCopies :: [CopiesWins] -> Int -> Int -> [CopiesWins]
updateCopies copies count wins =
    let (toUpdate, res) = splitAt wins copies
        toUpdate' = map (addCopies count) toUpdate
    in  toUpdate' ++ res

addCopies :: Int -> CopiesWins -> CopiesWins
addCopies newCopies (oldCopies, wins) = (oldCopies + newCopies, wins)