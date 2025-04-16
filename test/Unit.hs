module Unit (module Unit) where

import Trie as T
import Hangman
import Data.List (sort)

testPass :: Bool -> IO ()
testPass f = putStrLn $ if f then "OK" else "FAIL"

testFail :: Bool -> IO ()
testFail f = putStrLn $ if not f then "OK" else "FAIL"

useCharTest1 :: Bool
useCharTest1 =
  let testWords = ["abc", "abd", "abe", "bac", "bad", "bae", "bca", "bda", "bea"]
      t1 = T.fromList testWords
      (t2, i) = useChar 'a' t1
      in (i == 0 && (T.size t2 == 3) && (T.getAll t2 == ["abc", "abd", "abe"]))

useCharTest2 :: Bool
useCharTest2 =
  let testWords = ["cat", "hat", "sat", "eat", "fat", "rat", "tat", "mat", "vat"]
      t1 = T.fromList testWords
      (t2, i) = useChar 'c' t1
      in (i == -1 && (T.size t2 == 8) && (T.getAll t2 == sort (filter (/= "cat") testWords)))

runTests :: IO ()
runTests = do
  testPass useCharTest1
  testPass useCharTest2

