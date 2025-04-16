{-# OPTIONS_GHC -Wno-unused-imports #-}
module Hangman (module Hangman) where

import Words
import Trie as T
import Data.List.Index

type Result = (T.Trie, Int)

useChar :: Char -> T.Trie -> Result
useChar c t =
  let t1 = T.removeChar c t
      (i, s) = T.findWorstSpot c t
      better = if T.size t1 >= s then t1 else T.assignChar c i t
      pos = if T.size t1 >= s then -1 else i
      in (better, pos)

data GameState = GameState {
  trie :: Trie,
  usedChars :: [Char],
  missingLetters :: Int,
  fixedChars :: String,
  origSize :: Int
}

takeTurn :: Char -> GameState -> GameState
takeTurn c gs =
  let (t', i) = useChar c (trie gs)
      newUsed = c:usedChars gs
      nowMissing = if i == -1 then missingLetters gs else missingLetters gs - 1
      nowFixed = if i == -1 then fixedChars gs else setAt i c (fixedChars gs) 
      in GameState {trie = t', usedChars = newUsed, missingLetters = nowMissing, fixedChars = nowFixed, origSize = origSize gs}

newGameState :: Int -> [String] -> GameState
newGameState i xs = GameState {trie = T.fromList xs, usedChars = [], missingLetters = i, fixedChars = replicate i '_', origSize = length xs}
