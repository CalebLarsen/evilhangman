{-# OPTIONS_GHC -Wno-unused-imports #-}
module Words (module Words) where

import Data.List (nub)

wordsFile :: FilePath
wordsFile = "./words.txt"

loadFile :: FilePath -> IO String
loadFile = readFile

wordsOfLength :: Int -> IO [String]
wordsOfLength len = do
  str <- loadFile wordsFile
  let ls = lines str
      wordList = filter (\x -> length x == len && (len == length (nub x))) ls
  return wordList
