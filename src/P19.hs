module P19 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (find)

data Rule = Rule Char Char Int String -- property operator value destination
data Workflow = Workflow [Rule] String
data Input = Input (M.Map String Workflow) [M.Map Char Int]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input19"

parse :: String -> Input
parse = parse' . splitOn [""] . lines

parse' :: [[String]] -> Input
parse' (workflows:parts:_) = Input (parseWorkflows workflows) (parseParts parts)
parse' _ = error "bad input"

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
    in  Rule property operator (read valStr) (tail res)
parseRule _ = error "bad rule"

parseParts :: [String] -> [M.Map Char Int]
parseParts = map parsePart

parsePart :: String -> M.Map Char Int
parsePart = M.fromList . map parseProperty . splitOn "," . tail . init

parseProperty :: String -> (Char, Int)
parseProperty (p:_:valStr) = (p, read valStr)
parseProperty _ = error "bad property"

solve1 :: Input -> Int
solve1 (Input workflows parts) = sum $ map ratings $ filter (isAccepted workflows) parts

isAccepted :: M.Map String Workflow -> M.Map Char Int -> Bool
isAccepted workflows = (=="A") . applyWorkflows workflows "in"

applyWorkflows :: M.Map String Workflow -> String -> M.Map Char Int -> String
applyWorkflows workflows workflowName part =
    case M.lookup workflowName workflows of
        Nothing -> workflowName
        Just nextWorkflow ->
            let next = applyWorkflow nextWorkflow part
            in  applyWorkflows workflows next part

applyWorkflow :: Workflow -> M.Map Char Int -> String
applyWorkflow (Workflow rules defaultDestination) part =
    case find (ruleMatches part) rules of
        Nothing -> defaultDestination
        Just (Rule _ _ _ destination) -> destination

ruleMatches :: M.Map Char Int -> Rule -> Bool
ruleMatches part (Rule property '>' value _) =
    part M.! property > value
ruleMatches part (Rule property '<' value _) =
    part M.! property < value
ruleMatches _ _ = error "bad operator"

ratings :: M.Map k Int -> Int
ratings = sum . M.elems

solve2 :: Input -> Int
solve2 = countPaths "in" [] . getWorkflows

getWorkflows :: Input -> M.Map String Workflow
getWorkflows (Input workflows _) = workflows

countPaths :: String -> [Rule] -> M.Map String Workflow -> Int
countPaths "A" paths _ = comboCount paths
countPaths name paths workflows =
    case M.lookup name workflows of
        Nothing -> 0
        Just workflow -> workflowPaths workflows paths workflow

workflowPaths :: M.Map String Workflow -> [Rule] -> Workflow -> Int
workflowPaths workflows paths (Workflow [] defaultDestination) =
    countPaths defaultDestination paths workflows
workflowPaths workflows paths (Workflow (rule@(Rule _ _ _ destination):rules) defaultDestination) =
    countPaths destination (rule : paths) workflows +
        workflowPaths workflows (invertRule rule : paths) (Workflow rules defaultDestination)

invertRule :: Rule -> Rule
invertRule (Rule property '<' value destination) = Rule property '>' (value-1) destination
invertRule (Rule property '>' value destination) = Rule property '<' (value+1) destination
invertRule _ = error "bad operator"

comboCount :: [Rule] -> Int
comboCount rules = product $ map (ruleCombos rules) "xmas"

ruleCombos :: [Rule] -> Char -> Int
ruleCombos rules property = ruleCombos' $ filter (hasProperty property) rules

hasProperty :: Char -> Rule -> Bool
hasProperty property (Rule property' _ _ _) = property == property'

ruleCombos' :: [Rule] -> Int
ruleCombos' rules = length $ filter (\i -> all (passes i) rules) [1..4000]

passes :: Int -> Rule -> Bool
passes i (Rule _ '<' value _) = i < value
passes i (Rule _ '>' value _) = i > value
passes _ _ = error "bad operator"