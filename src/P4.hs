module P4 (run1, run2, inputLocation) where

import Data.Char (isDigit)
import qualified Data.Map as M

type Input = [Card]

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
parseCard = makeCard . mapTuple (map read . words . removeSeparator) . break (=='|')

removeSeparator :: String -> String
removeSeparator = dropWhile (not . isDigit)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

makeCard :: ([Int], [Int]) -> Card
makeCard (winners, ours) = Card winners ours

solve1 :: Input -> Int
solve1 = sum . map (score . winningNumbers)

winningNumbers :: Card -> Int
winningNumbers (Card winners ours)= length $ filter (`elem` winners) ours

score :: Int -> Int
score x 
    | x <= 0 = 0
    | otherwise = 2^(x-1)

solve2 :: Input -> Int
solve2 input = countCards $ foldl winCopies (M.fromList (zip [0..(length input - 1)] (repeat 1))) (zip [0..] input)

winCopies :: M.Map Int Int -> (Int, Card) -> M.Map Int Int
winCopies copies (i, card) =
    let points = winningNumbers card
        copyIndices = [i+1..i+points]
        winnings = copies M.! i
    in  addCopies winnings copies copyIndices

addCopies :: Int -> M.Map Int Int -> [Int] -> M.Map Int Int
addCopies winnings = foldl (addCopy winnings)

addCopy :: Int -> M.Map Int Int -> Int -> M.Map Int Int
addCopy winnings copies i = M.adjust (+winnings) i copies

countCards :: M.Map Int Int -> Int
countCards = sum . M.elems
