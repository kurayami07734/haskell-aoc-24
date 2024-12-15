module Two where

import Data.List (inits, tails)

readReport :: String -> [Int]
readReport report = map read $ words report

allSameSign :: [Int] -> Bool
allSameSign xs = all (> 0) xs || all (< 0) xs

smallEnough :: [Int] -> Bool
smallEnough = all ((<= 3) . abs)

bigEnough :: [Int] -> Bool
bigEnough = all ((>= 1) . abs)

-- Predicate for safe report as per problem statement
-- Time Complexity: O(n)
-- Space Complexity: O(n) -> n adjacent pairs
isSafe :: [Int] -> Bool
isSafe xs = and $ [allSameSign, smallEnough, bigEnough] <*> pure diffs
  where
    diffs = zipWith (-) xs $ tail xs

-- Creating variants of reports by excluding one level
-- Eg: [1, 2, 3] -> [[2,3], [1,2], [1,3]]
-- Time Complexity: O(n^2) -> nested loops
-- Space Complexity: O(n^2) -> nested loops
damped :: [Int] -> [[Int]]
damped line = zipWith (++) (inits line) (drop 1 $ tails line)

safeWhenDamped :: [Int] -> Bool
safeWhenDamped = any isSafe . damped

-- Count number of safe reports
-- Time complexity: O(n*m)
-- Space complexity: Same as isSafe
partA :: [[Int]] -> Int
partA reports = length $ filter isSafe reports

-- Count number of safe and false unsafe reports
-- Time complexity: O(n*m + m*k^2) (k is false unsafe reports)
-- Space complexity: Same as damped
partB :: [[Int]] -> Int
partB reports = length $ filter isSafe' reports
  where
    isSafe' l = isSafe l || safeWhenDamped l

solve :: IO ()
solve = do
  text <- readFile "./puzzle-inputs/two/input.txt"
  let reports = map readReport $ lines text
  print $ partA reports
  print $ partB reports