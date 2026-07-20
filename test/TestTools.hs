module TestTools (red, green, gray, assertEq, testGroup, testSuite) where

import System.Exit (exitFailure)

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

gray :: String -> String
gray s = "\ESC[90m" ++ s ++ "\ESC[0m"

assertEq :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEq msg expected actual =
  if expected == actual
    then putStrLn ("  " ++ green "PASS" ++ ": " ++ msg) >> pure True
    else putStrLn ("  " ++ red   "FAIL" ++ ": " ++ msg ++ " (Expected: " ++ show expected ++ " Actual: " ++ show actual ++ ")") >> pure False

testGroup :: String -> [IO Bool] -> IO Bool
testGroup groupName tests = do
  putStrLn $ gray ("Running test group: " ++ "\"" ++ groupName ++ "\"")
  results <- sequence tests
  let passed = length (filter id results)
  let total = length results
  let isPassed = all id results
  let result = if isPassed then green "PASS" else red "FAIL"
  putStrLn $ gray ("Group \"" ++ groupName ++ "\": ") ++ result ++ " (" ++ show passed ++ " / " ++ show total ++ ")"
  pure isPassed

testSuite :: [IO Bool] -> IO ()
testSuite groups = do
  results <- sequence groups
  let passed = length (filter id results)
  let total = length results
  if all id results
    then putStrLn (green "All tests passed." ++ " (" ++ show passed ++ " / " ++ show total ++ ")")
    else putStrLn (red "Some tests failed!" ++ " (" ++ show passed ++ " / " ++ show total ++ ")") >> exitFailure