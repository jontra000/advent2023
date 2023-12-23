{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use first" #-}
module P20 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.List (findIndex, partition)
import Data.Maybe (fromMaybe)

type Graph = [(String, [String])]
data Component = Broadcaster [String] | FlipFlop Bool [String] | Conjunction (M.Map String Bool) [String] deriving Show
type Components = M.Map String Component
data Signal = Signal Bool String String -- signal target source
type State = ([Signal], Components)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input20"

parse :: String -> Components
parse = parseComponents . parseGraph

parseGraph :: String -> Graph
parseGraph = map (parseLine . words) . lines

parseLine :: [String] -> (String, [String])
parseLine (name:_:outputs) = (name, map parseOutput outputs)
parseLine _ = error "bad target"

parseOutput :: String -> String
parseOutput = filter isAlpha

parseComponents :: Graph -> Components
parseComponents graph = M.fromList $ map (parseComponent graph) graph

parseComponent :: Graph -> (String, [String]) -> (String, Component)
parseComponent _ ("broadcaster", outputs) = ("broadcaster", Broadcaster outputs)
parseComponent _ ('%':name, outputs) = (name, FlipFlop False outputs)
parseComponent graph ('&':name, outputs) = (name, Conjunction (initialiseConjunction name graph) outputs)
parseComponent _ _ = error "bad component"

initialiseConjunction :: String -> Graph -> M.Map String Bool
initialiseConjunction name = initialiseInputs . graphInputs name

initialiseInputs :: Graph -> M.Map String Bool
initialiseInputs = M.fromList . map (\(k,_) -> (tail k, False))

graphInputs :: String -> Graph -> Graph
graphInputs name = filter ((name `elem`) . snd)

solve1 :: Components -> Int
solve1 = score . pushButtonRepeated

solve2 :: Components -> Int
solve2 state = predictRx state $ pushButtonRepeated state

score :: [[Signal]] -> Int
score = score' . partition getSignal . concat . take 1000
    where score' (highs, lows) = length highs * length lows

getSignal :: Signal -> Bool
getSignal (Signal x _ _) = x

pushButtonRepeated :: Components -> [[Signal]]
pushButtonRepeated state =
    let (toProcess, state') = step [Signal False "broadcaster" "button"] ([], state)
    in  toProcess : pushButtonRepeated state'

step :: [Signal] -> State -> State
step [] state = state
step (nextSignal@(Signal signal target source):toProcess) (processedSignals, state) = step toProcess' (processedSignals', state')
    where   (outputSignals, state') = maybe ([], state) (updateState target state . sendSignal signal source target) (M.lookup target state)
            processedSignals' = nextSignal : processedSignals
            toProcess' = toProcess ++ outputSignals

updateState :: String -> Components -> ([Signal], Component) -> State
updateState target state (signals, component) = (signals, M.insert target component state)

sendSignal :: Bool -> String -> String -> Component -> ([Signal], Component)
sendSignal signal _ target component@(Broadcaster outputs) = (signalOutputs signal target outputs, component)
sendSignal True _ _ component@(FlipFlop _ _) = ([], component)
sendSignal False _ target (FlipFlop signal outputs) =
    let signal' = not signal
    in  (signalOutputs signal' target outputs, FlipFlop signal' outputs)
sendSignal signal source target (Conjunction inputs outputs) =
    let inputs' = M.insert source signal inputs
        signal' = not $ and (M.elems inputs')
    in  (signalOutputs signal' target outputs, Conjunction inputs' outputs)

signalOutputs :: Bool -> String -> [String] -> [Signal]
signalOutputs signal input = map (signalOutput signal input)

signalOutput :: Bool -> String -> String -> Signal
signalOutput signal input output = Signal signal output input

predictRx :: Components -> [[Signal]] -> Int
predictRx graph toProcess = getCycle toProcess' graph False "rx"
    where toProcess' = [] : take 5000 toProcess -- include init state for periodicity to be calculated correctly
          
getInputs :: String -> Components -> [String]
getInputs target = map fst . filter (isInput target . snd) . M.toList

isInput :: String -> Component -> Bool
isInput target (Broadcaster outputs) = target `elem` outputs
isInput target (FlipFlop _ outputs) = target `elem` outputs
isInput target (Conjunction _ outputs) = target `elem` outputs

getCycle :: [[Signal]] -> Components -> Bool -> String -> Int
getCycle toProcess graph expected target = fromMaybe (predictCycle toProcess graph target expected (M.lookup target graph)) (lookupCycle target expected toProcess)
    
predictCycle :: [[Signal]] -> Components -> String -> Bool -> Maybe Component -> Int
predictCycle toProcess graph target _ (Just (FlipFlop _ _)) = predictOutputFlipFlop toProcess graph target
predictCycle toProcess graph target expected (Just (Conjunction _ _)) = predictOutputConjunction toProcess graph target expected
predictCycle _ _ _ _ (Just (Broadcaster _)) = 1
predictCycle toProcess graph target expected Nothing = predictOutputRx toProcess graph target expected

lookupCycle :: String -> Bool -> [[Signal]] -> Maybe Int
lookupCycle target expected = findIndex (any (signalMatches expected target))

signalMatches :: Bool -> String -> Signal -> Bool
signalMatches expected target (Signal signal _ target') = signal == expected && target == target'

predictOutputFlipFlop :: [[Signal]] -> M.Map String Component -> String -> Int
predictOutputFlipFlop signals graph name = 2 * minimum (inputCycles signals graph False inputs)
    where inputs = getInputs name graph

predictOutputConjunction :: [[Signal]] -> M.Map String Component -> String -> Bool -> Int
predictOutputConjunction signals graph name False = lowestCommonMultiple (inputCycles signals graph True inputs)
    where inputs = getInputs name graph
predictOutputConjunction signals graph name True = minimum (inputCycles signals graph False inputs)
    where inputs = getInputs name graph

lowestCommonMultiple :: [Int] -> Int
lowestCommonMultiple = foldl lcm 1

predictOutputRx :: [[Signal]] -> M.Map String Component -> String -> Bool -> Int
predictOutputRx signals graph name expected = minimum (inputCycles signals graph expected inputs)
    where inputs = getInputs name graph

inputCycles :: [[Signal]] -> Components -> Bool -> [String] -> [Int]
inputCycles toProcess graph signal = map (getCycle toProcess graph signal)