{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P19 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (find)

data Rule = Rule Char (Int -> Int -> Bool) Int String -- property operator value destination
data Workflow = Workflow [Rule] String
type Part = M.Map Char Int
data Input = Input (M.Map String Workflow) [Part]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input19"

parse :: String -> Input
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks (workflows:parts:_) = Input (parseWorkflows workflows) (parseParts parts)
parseBlocks _ = error "bad input"

parseWorkflows :: [String] -> M.Map String Workflow
parseWorkflows = M.fromList . map parseWorkflow

parseWorkflow :: String -> (String, Workflow)
parseWorkflow s =
    let (name, rules) = break (=='{') s
    in  (name, parseRules rules)

parseRules :: String -> Workflow
parseRules = parseRules' . splitOn "," . tail . init

parseRules' :: [String] -> Workflow
parseRules' ruleStrs = Workflow (map parseRule (init ruleStrs)) (last ruleStrs)

parseRule :: String -> Rule
parseRule (property:operator:ruleStr) =
    let (valStr, res) = break (==':') ruleStr
    in  Rule property (parseOperator operator) (read valStr) (tail res)
parseRule _ = error "bad rule"

parseOperator :: Ord a => Char -> a -> a -> Bool
parseOperator '>' = (>)
parseOperator '<' = (<)
parseOperator _ = error "bad operator"

parseParts :: [String] -> [Part]
parseParts = map parsePart

parsePart :: String -> Part
parsePart = M.fromList . map parseProperty . splitOn "," . tail . init

parseProperty :: String -> (Char, Int)
parseProperty (p:_:valStr) = (p, read valStr)
parseProperty _ = error "bad property"

solve1 :: Input -> Int
solve1 = sum . map ratings . acceptedParts

acceptedParts :: Input -> [Part]
acceptedParts (Input workflows parts) = filter (isAccepted workflows) parts

isAccepted :: M.Map String Workflow -> Part -> Bool
isAccepted workflows = (=="A") . applyWorkflows workflows "in"

applyWorkflows :: M.Map String Workflow -> String -> Part -> String
applyWorkflows workflows workflowName part = maybe workflowName (applyWorkflows' workflows part) (M.lookup workflowName workflows)

applyWorkflows' :: M.Map String Workflow -> Part -> Workflow -> String
applyWorkflows' workflows part nextWorkflow =
    let next = applyWorkflow nextWorkflow part
    in  applyWorkflows workflows next part

applyWorkflow :: Workflow -> Part -> String
applyWorkflow (Workflow rules defaultDestination) part = maybe defaultDestination getDestination (find (ruleMatches part) rules)

getDestination :: Rule -> String
getDestination (Rule _ _ _ destination) = destination

ruleMatches :: Part -> Rule -> Bool
ruleMatches part (Rule property operator value _) = operator (part M.! property) value

ratings :: M.Map k Int -> Int
ratings = sum . M.elems

solve2 :: Input -> Int
solve2 = countPaths "in" initialPaths . getWorkflows

initialPaths :: M.Map Char [Int]
initialPaths = M.fromList $ map (\c -> (c, [1..4000])) "xmas"

getWorkflows :: Input -> M.Map String Workflow
getWorkflows (Input workflows _) = workflows

countPaths :: String -> M.Map Char [Int] -> M.Map String Workflow -> Int
countPaths "A" paths _ = product (map length (M.elems paths))
countPaths name paths workflows = maybe 0 (workflowPaths workflows paths) (M.lookup name workflows)

workflowPaths :: M.Map String Workflow -> M.Map Char [Int] -> Workflow -> Int
workflowPaths workflows paths (Workflow [] defaultDestination) = countPaths defaultDestination paths workflows
workflowPaths workflows paths (Workflow (rule@(Rule _ _ _ destination):rules) defaultDestination) =
    countPaths destination (applyRule rule paths) workflows +
        workflowPaths workflows (applyRule (invertRule rule) paths) (Workflow rules defaultDestination)

applyRule :: Rule -> M.Map Char [Int] -> M.Map Char [Int]
applyRule (Rule property operator value _) = M.update (Just . applyRule' operator value) property

applyRule' :: (a -> p -> Bool) -> p -> [a] -> [a]
applyRule' operator value = filter (`operator` value)

invertRule :: Rule -> Rule
invertRule (Rule property operator value destination) = Rule property (\a b -> not (operator a b)) value destination