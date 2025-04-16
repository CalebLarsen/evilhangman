{-# OPTIONS_GHC -Wno-unused-imports #-}
module Unit (module Unit) where

import qualified Data.List
import qualified Trie as T
import Data.Maybe
import Debug.Trace
import Data.List (find, sort)

testPass :: Bool -> IO ()
testPass f = putStrLn $ if f then "OK" else "FAIL"

testFail :: Bool -> IO ()
testFail f = putStrLn $ if not f then "OK" else "FAIL"

insertThenFind :: Bool
insertThenFind =
  let t = T.insert "abc" T.empty
      in T.contains "abc" t

insertThenFindFail :: Bool
insertThenFindFail =
  let t = T.insert "abc" T.empty
      in T.contains "ab" t

insertThenFindFail2 :: Bool
insertThenFindFail2 =
  let t = T.insert "ab" T.empty
      in T.contains "abc" t

contains :: String -> [String] -> Bool
contains s xs = isJust $ find (== s) xs

getAllTest :: Bool
getAllTest =
  let t1 = T.insert "abc" T.empty
      t2 = T.insert "ab" t1
      t3 = T.insert "a" t2
      contents = T.getAll t3
      conds = [length contents == 3, contains "a" contents, contains "ab" contents, contains "abc" contents]
      in and conds

getAllFail :: Bool
getAllFail =
  let t1 = T.insert "abcd" T.empty
      t2 = T.insert "abc" t1
      t3 = T.insert "d" t2
      contents = T.getAll t3
      conds = [length contents == 4, contains "c" contents, contains "ab" contents, contains "abd" contents, contains "" contents]
      in or conds

sizeTest :: Bool
sizeTest =
  let t1 = T.insert "a" $ T.insert "b" $ T.insert "ab" $ T.insert "ab" T.empty
      in T.size t1 == 3

sizeTestFail :: Bool
sizeTestFail =
  let t1 = T.empty
      in T.size t1 /= 0

deleteTest :: Bool
deleteTest =
  let testWords = ["abc", "abd", "acd", "aaa", "ba", "b", "ab"]
      t1 = T.fromList testWords
      t2 = T.removeChar 'b' t1
      wordsNoB = filter (notElem 'b') testWords
      in T.getAll t2 == sort wordsNoB

letterCountsTest1 :: Bool
letterCountsTest1 =
  let testWords = ["abc", "abd", "abe", "bac", "bad", "bae", "bca", "bda", "bea"]
      t1 = T.fromList testWords
      counts = T.findLetterPosCounts 'a' t1
      expected = [3, 3, 3, 0]
      in counts == expected

letterCountsTest2 :: Bool
letterCountsTest2 =
  let testWords = ["cat", "hat", "sat", "eat", "fat", "rat", "tat", "mat", "vat"]
      t1 = T.fromList testWords
      counts = T.findLetterPosCounts 'a' t1
      expected = [0, 9, 0, 0]
      in counts == expected

findWorstSpotTest1 :: Bool
findWorstSpotTest1 =
  let testWords = ["abc", "abd", "abe", "bac", "bad", "bae", "bca", "bda", "bea"]
      t1 = T.fromList testWords
      worst = T.findWorstSpot 'a' t1
      in worst == (0, 3)

findWorstSpotTest2 :: Bool
findWorstSpotTest2 =
  let testWords = ["cat", "hat", "sat", "eat", "fat", "rat", "tat", "mat", "vat"]
      t1 = T.fromList testWords
      worst = T.findWorstSpot 'a' t1
      in worst == (1, 9)

assignCharTest1 :: Bool
assignCharTest1 =
  let testWords = ["abc", "abd", "abe", "bac", "bad", "bae", "bca", "bda", "bea"]
      t1 = T.fromList testWords
      t2 = T.assignChar 'a' 0 t1
      in T.size t2 == 3

assignCharTest2 :: Bool
assignCharTest2 =
  let testWords = ["cat", "hat", "sat", "eat", "fat", "rat", "tat", "mat", "vat"]
      t1 = T.fromList testWords
      t2 = T.assignChar 'a' 0 t1
      in T.size t2 == 0


runTest :: IO ()
runTest = do
  putStrLn "Unit tests"
  putStrLn "==========="
  testPass insertThenFind
  testFail insertThenFindFail
  testFail insertThenFindFail2
  testPass getAllTest
  testFail getAllFail
  testPass sizeTest
  testFail sizeTestFail
  testPass letterCountsTest1
  testPass letterCountsTest2
  testPass findWorstSpotTest1
  testPass findWorstSpotTest2
  testPass assignCharTest1
  testPass assignCharTest2
