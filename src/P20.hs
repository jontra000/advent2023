{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use first" #-}
module P20 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.List (findIndex, partition)
import Data.Maybe (fromMaybe)

data Component = Broadcaster [String] | FlipFlop Bool [String] | Conjunction (M.Map String Bool) [String] deriving Show

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input20"

parse :: String -> M.Map String Component
parse = parseComponents . parseGraph

parseGraph :: String -> [(String, [String])]
parseGraph = map (parseLine . words) . lines

parseLine :: [String] -> (String, [String])
parseLine (name:_:outputs) = (name, map parseOutput outputs)
parseLine _ = error "bad target"

parseOutput :: String -> String
parseOutput = filter isAlpha

parseComponents :: [(String, [String])] -> M.Map String Component
parseComponents graph = M.fromList $ map (parseComponent graph) graph

parseComponent :: [(String, [String])] -> (String, [String]) -> (String, Component)
parseComponent _ ("broadcaster", outputs) = ("broadcaster", Broadcaster outputs)
parseComponent _ ('%':name, outputs) = (name, FlipFlop False outputs)
parseComponent graph ('&':name, outputs) = (name, Conjunction (graphInputs name graph) outputs)
parseComponent _ _ = error "bad component"

graphInputs :: String -> [(String, [String])] -> M.Map String Bool
graphInputs name = M.map (const False) . M.fromList . map (\(k,v) -> (tail k, v)) . filter ((name `elem`) . snd)

solve1 :: M.Map String Component -> Int
solve1 state = score (iterate pushButton ([], state))

solve2 :: M.Map String Component -> Int
solve2 state = predictRx state $ map fst $ take 5000 (iterate pushButton ([], state))

score :: [([(Bool, b1, c)], b2)] -> Int
score = score' . partition (\(x, _, _) -> x) . concat . take 1001 . map fst
    where score' (highs, lows) = length highs * length lows

pushButton :: ([(Bool, String, String)], M.Map String Component) -> ([(Bool, String, String)], M.Map String Component)
pushButton (_, state) = step [(False, "broadcaster", "button")] ([], state)

step :: [(Bool, String, String)] -> ([(Bool, String, String)], M.Map String Component) -> ([(Bool, String, String)], M.Map String Component)
step [] results = results
step (s@(signal, target, source):signals) (results, state) =
    let (outputSignals, newModule) = sendSignal signal source target (state M.! target)
        state' = M.insert target newModule state
        results' = s : results
        signals' = signals ++ outputSignals
    in  if M.member target state then step signals' (results', state') else step signals (results', state)

sendSignal :: Bool -> String -> String -> Component -> ([(Bool, String, String)], Component)
sendSignal signal _ target component@(Broadcaster outputs) = (map (\output -> (signal, output, target)) outputs, component)
sendSignal True _ _ component@(FlipFlop _ _) = ([], component)
sendSignal False _ target (FlipFlop signal outputs) =
    let signal' = not signal
    in  (map (\output -> (signal', output, target)) outputs, FlipFlop signal' outputs)
sendSignal signal source target (Conjunction inputs outputs) =
    let inputs' = M.insert source signal inputs
        signal' = not $ and (M.elems inputs')
    in  (map (\output -> (signal', output, target)) outputs, Conjunction inputs' outputs)

predictRx :: M.Map String Component -> [[(Bool, String, String)]] -> Int
predictRx graph signals  = predictOutput signals graph "rx" False

getInputs :: M.Map b Component -> String -> [b]
getInputs graph target = map fst $ filter (isInput target . snd) $ M.toList graph

isInput :: String -> Component -> Bool
isInput target (Broadcaster outputs) = target `elem` outputs
isInput target (FlipFlop _ outputs) = target `elem` outputs
isInput target (Conjunction _ outputs) = target `elem` outputs

predictOutput :: [[(Bool, String, String)]] -> M.Map String Component -> String -> Bool -> Int
predictOutput states graph target expected =
    fromMaybe (
        case M.lookup target graph of
            Just (FlipFlop _ _) -> predictOutputFlipFlop states graph target
            Just (Conjunction _ _) -> predictOutputConjunction states graph target expected
            Just (Broadcaster _) -> 1
            Nothing -> predictOutputRx states graph target expected)
            (checkStates states target expected)

checkStates :: [[(Bool, String, String)]] -> String -> Bool -> Maybe Int
checkStates signals target expected = findIndex (any (\(signal, _, target') -> signal == expected && target == target')) signals

predictOutputFlipFlop :: [[(Bool, String, String)]] -> M.Map String Component -> String -> Int
predictOutputFlipFlop states graph name = 2 * minimum (map (\input -> predictOutput states graph input False) inputs)
    where inputs = getInputs graph name

predictOutputConjunction :: [[(Bool, String, String)]] -> M.Map String Component -> String -> Bool -> Int
predictOutputConjunction states graph name False = foldl lcm 1 (map (\input -> predictOutput states graph input True) inputs)
    where inputs = getInputs graph name
predictOutputConjunction states graph name True = minimum (map (\input -> predictOutput states graph input False) inputs)
    where inputs = getInputs graph name

predictOutputRx :: [[(Bool, String, String)]] -> M.Map String Component -> String -> Bool -> Int
predictOutputRx states graph name expected = minimum (map (\input -> predictOutput states graph input expected) inputs)
    where inputs = getInputs graph name 