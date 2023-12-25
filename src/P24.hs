module P24 (run1, run2, inputLocation) where

import Data.List (tails, transpose, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Matrix as Mat
import Data.Either (fromRight)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Double
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input24"

parse :: String -> [((Double, Double, Double), (Double, Double, Double))]
parse = map (parseLine . words) . lines

parseLine :: [String] -> ((Double, Double, Double), (Double, Double, Double))
parseLine (x:y:z:_:a:b:c:_) = ((read $ init x, read $ init y, read z), (read $ init a, read $ init b, read c))
parseLine _ = error "bad input"

solve1 :: [((Double, Double, Double), (Double, Double, Double))] -> Int
solve1 = sum . map countCollisions . tails . mapMaybe (exitPoints 200000000000000.0 400000000000000.0)

exitPoints :: Double -> Double -> ((Double, Double, Double), (Double, Double, Double)) -> Maybe ((Double, Double), (Double, Double))
exitPoints entry exit ((x,y,_), (dx,dy,_))
    | u1 > u2 = Nothing
    | u2 < 0 = Nothing
    | otherwise = Just ((x + p2 * u1, y + p4 * u1), (x + p2 * u2, y + p4 * u2))
    where   q1 = x - entry
            q2 = exit - x
            q3 = y - entry
            q4 = exit - y
            p1 = -dx
            p2 = dx
            p3 = -dy
            p4 = dy
            r1 = q1 / p1
            r2 = q2 / p2
            r3 = q3 / p3
            r4 = q4 / p4
            u1 = max 0 $ max (if p1 < 0 then r1 else r2) (if p3 < 0 then r3 else r4)
            u2 = min (if p1 < 0 then r2 else r1) (if p3 < 0 then r4 else r3)

countCollisions :: [((Double, Double), (Double, Double))] -> Int
countCollisions [] = 0
countCollisions (current:next) = length $ filter (crosses current) next

crosses :: ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double)) -> Bool
crosses (p1, q1) (p2, q2)
    | o1 /= o2 && o3 /= o4 = True
    | o1 == 0 = onSegment p1 p2 q1
    | o2 == 0 = onSegment p1 q2 q1
    | o3 == 0 = onSegment p2 p1 q2
    | o4 == 0 = onSegment p2 q1 q2
    | otherwise = False
    where   o1 = orientation p1 q1 p2 
            o2 = orientation p1 q1 q2
            o3 = orientation p2 q2 p1
            o4 = orientation p2 q2 q1

onSegment :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
onSegment (px, py) (qx, qy) (rx, ry) = qx <= max px rx && qx >= min px rx && qy <= max py ry && qy >= min py ry

orientation :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Int
orientation (px, py) (qx, qy) (rx, ry)
    | val == 0 = 0
    | val < 0 = 2
    | otherwise = 1
        where val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)

solve2 :: [((Double, Double, Double), (Double, Double, Double))] -> Double
solve2 = median . map solveShift . takeWhile ((>2) . length) . tails

median :: [Double] -> Double
median xs = sort xs !! (length xs `div` 2) 

solveShift :: [((Double, Double, Double), (Double, Double, Double))] -> Double
solveShift xs =
    let dx = minimum $ map (\((x,_,_), _) -> x) xs
        dy = minimum $ map (\((_,y,_), _) -> y) xs
        dz = minimum $ map (\((_,_,z), _) -> z) xs
        xs' = map (\((x,y,z), v) -> ((x-dx, y-dy, z-dz), v)) xs
        result = solveLa xs'
    in  result + dx + dy + dz

solveLa :: [((Double, Double, Double), (Double, Double, Double))] -> Double
solveLa ((((ax, ay, az), (vax, vay, vaz)):((bx, by, bz), (vbx, vby, vbz)):((cx, cy, cz), (vcx, vcy, vcz)):_)) =
    let m = Mat.fromLists [
            [ vay - vby, vbx - vax, 0.0,       by - ay, ax - bx, 0.0     ],
            [ vay - vcy, vcx - vax, 0.0,       cy - ay, ax - cx, 0.0     ],
            [ vbz - vaz, 0.0,       vax - vbx, az - bz, 0.0,     bx - ax ],
            [ vcz - vaz, 0.0,       vax - vcx, az - cz, 0.0,     cx - ax ],
            [ 0.0,       vaz - vbz, vby - vay, 0.0,     bz - az, ay - by ],
            [ 0.0,       vaz - vcz, vcy - vay, 0.0,     cz - az, ay - cy ]]
        b = Mat.fromList 6 1 [
            (by * vbx - bx * vby) - (ay * vax - ax * vay),
            (cy * vcx - cx * vcy) - (ay * vax - ax * vay),
            (bx * vbz - bz * vbx) - (ax * vaz - az * vax),
            (cx * vcz - cz * vcx) - (ax * vaz - az * vax),
            (bz * vby - by * vbz) - (az * vay - ay * vaz),
            (cz * vcy - cy * vcz) - (az * vay - ay * vaz)]
        result = Mat.rref (m Mat.<|> b)
    in  sum $ take 3 $ last $ transpose $ Mat.toLists $ fromRight (error "couldn't solve matrix") result
solveLa _ = error "bad input"