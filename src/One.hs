module One where

import Data.List (sort)
import qualified Data.MultiSet as MS

-- AOC 2024
-- problem 1

type Pair = (Int, Int)

readPair :: String -> Pair
readPair s = (read a, read b)
  where
    (a : b : _) = words s

-- Sum over the differences after sorting the unzipped pairs
-- Time Complexity : O(nlogn) -> merge sort
-- Space Complexity: O(n) -> size of intermediate lists during merge sort
partA :: [Pair] -> Int
partA pairs = sum $ zipWith (\a b -> abs (a - b)) (sort lefts) (sort rights)
  where
    (lefts, rights) = unzip pairs

-- Use Multiset to counts elements
-- Time Complexity : O(nlogn) -> construction of multiset
-- Space Complexity: O(n) -> size of multiset
partB :: [Pair] -> Int
partB pairs = sum $ map similarity lefts
  where
    (lefts, rights) = unzip pairs
    counts = MS.fromList rights
    similarity l = l * MS.occur l counts

solve :: IO ()
solve = do
  text <- readFile "./puzzle-inputs/one/input.txt"
  let pairs = map readPair $ lines text
  print $ partA pairs
  print $ partB pairs
