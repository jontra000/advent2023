module P7 (run1, run2, inputLocation) where
import Data.Char (digitToInt)
import Data.List (sort, group, sortBy, partition, delete)

data Hand = Hand [Int] Int

run1 :: String -> Int
run1 = solve . parse

run2 :: String -> Int
run2 = solve . map changeJokers . parse

inputLocation :: String
inputLocation = "inputs/input7"

parse :: String -> [Hand]
parse = map (parseHand . words) . lines

parseHand :: [String] -> Hand
parseHand (cardsStr:bidStr:_) = Hand cards bid
    where bid = read bidStr
          cards = map parseCard cardsStr
parseHand _ = error "Bad input"

parseCard :: Char -> Int
parseCard 'T' = 10
parseCard 'J' = 11
parseCard 'Q' = 12
parseCard 'K' = 13
parseCard 'A' = 14
parseCard c = digitToInt c

solve :: [Hand] -> Int
solve = sum . winnings

winnings :: [Hand] -> [Int]
winnings = zipWith handWinnings [1..] . sortBy compareHand

compareHand :: Hand -> Hand -> Ordering
compareHand (Hand handA _) (Hand handB _) =
    case compareHandType handA handB of
        EQ -> compare handA handB
        x -> x

groupKinds :: [Int] -> [Int]
groupKinds [1,1,1,1,1] = [5]
groupKinds hand =
    let (jokers, hand') = partition (==1) hand
        jokersCount = length jokers
        groups = map length $ group $ sort hand'
        bestGroup = maximum groups
    in  (bestGroup + jokersCount) : delete bestGroup groups

compareHandType :: [Int] -> [Int] -> Ordering
compareHandType handA handB =
    let typeA = groupKinds handA
        typeB = groupKinds handB
    in  case compare (maximum typeA) (maximum typeB) of
            EQ -> compare (length typeB) (length typeA) -- args switched because shorter is better
            x -> x 

handWinnings :: Int -> Hand -> Int
handWinnings rank (Hand _ bid) = rank * bid

changeJokers :: Hand -> Hand
changeJokers (Hand cards bid) = Hand (map changeJokers' cards) bid
    where changeJokers' 11 = 1
          changeJokers' x = x