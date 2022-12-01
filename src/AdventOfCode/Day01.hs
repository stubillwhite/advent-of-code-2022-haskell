module AdventOfCode.Day01
  ( day01, solutionOne, exampleInput
  ) where 

import Data.List.Split ( splitOn )
import Data.Char ( isSpace )

-- >>> exampleInput
-- "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n"
exampleInput :: String
exampleInput = unlines ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000"]

-- >>> stripTrailingNewlines ""
-- >>> stripTrailingNewlines "foo"
-- >>> stripTrailingNewlines "foo\n"
-- ""
-- "foo"
-- "foo"
stripTrailingNewlines :: String -> String
stripTrailingNewlines = reverse . dropWhile isSpace . reverse

-- >>> parseLoad "1"
-- >>> parseLoad "1\n2"
-- >>> parseLoad "1\n2\n"
-- [1]
-- [1,2]
-- [1,2]
parseLoad :: String -> [Int]
parseLoad s =
  map read $ splitOn "\n" $ stripTrailingNewlines s

-- >>> parseInput exampleInput
-- [[1000,2000,3000],[4000],[5000,6000],[7000,8000,9000],[10000]]
parseInput :: String -> [[Int]]
parseInput input =
  map parseLoad (splitOn "\n\n" input)

-- >>> solutionOne exampleInput
-- 24000
solutionOne :: String -> Int
solutionOne input = 
      maximum totalCaloriesPerElf
    where
      totalCaloriesPerElf = map sum $ parseInput input
        
day01 :: IO ()
day01 = do  
  problemInput <- readFile "input/day-1-input.txt"
  putStrLn "Day 1"
  putStrLn $ "  Part one: " ++ show (solutionOne problemInput)
