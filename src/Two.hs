module Two where

makePairs :: [Int] -> [(Int, Int)]
makePairs [] = []
makePairs [_] = []
makePairs (x : y : xs) = [(x, y)] <> makePairs (y : xs)

pairWiseDiff :: [Int] -> [Int]
-- Added uncurry on suggestion of linter; it says code will be better at lazy eval
pairWiseDiff xs = map (uncurry (-)) (makePairs xs)

-- Passing Infinite list here would result in infinite loop
enumerate :: [Int] -> [(Int, Int)]
enumerate = zip [0 ..]

-- For Discarding zipped indices after done with enumeration
takeValues :: ([Int], [Int]) -> [Int]
takeValues (_, v) = v

readReport :: String -> [Int]
readReport report = map read w
  where
    w = words report

-- Predicate for safe report as per problem statement
-- Time Complexity: O(n) 
-- Space Complexity: O(n) -> n adjacent pairs 
isSafeReport :: [Int] -> Bool
isSafeReport level = allPositive || allNegative
  where
    allPositive = all (\x -> x > 0 && x < 4) levelDiffs
    allNegative = all (\x -> x < 0 && x > -4) levelDiffs
    levelDiffs = pairWiseDiff level

-- Creating variants of reports by excluding one level
-- Eg: [1, 2, 3] -> [[2,3], [1,2], [1,3]]
-- Time Complexity: O(n^2) -> nested loops
-- Space Complexity: O(n^2) -> nested loops
reportsExcludingOneLevel :: [Int] -> [[Int]]
reportsExcludingOneLevel levels = variants
  where
    variants = map (takeValues . unzip) enumeratedVariants
    enumeratedVariants = map excludeOne enumerated
    enumerated = enumerate levels
    excludeOne (i, _) = filter (\(j, _) -> i /= j) enumerated

-- Count number of safe reports
-- Time complexity: O(n*m) 
-- Space complexity: Same as isSafeReport
partA :: [[Int]] -> Int
partA reports = length $ filter isSafeReport reports

-- Count number of safe and false unsafe reports
-- Time complexity: O(n*m + m*k^2) (k is false unsafe reports)
-- Space complexity: Same as reportsExcludingOneLevel
partB :: [[Int]] -> Int
partB reports = partA reports + falseUnsafeCount
  where
    falseUnsafeCount = length $ filter (any isSafeReport) unsafeReportVariants
    unsafeReportVariants = map reportsExcludingOneLevel (filter (not . isSafeReport) reports)

solve :: IO ()
solve = do
  text <- readFile "./puzzle-inputs/two/input.txt"
  let reports = map readReport $ lines text
  print $ partA reports
  print $ partB reports