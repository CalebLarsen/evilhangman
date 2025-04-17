{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Lib (module Lib) where

import Words
import Hangman
import Text.Printf (printf)
import Trie as T
import System.IO (stdout)
import GHC.IO.Handle (hFlush)
import Data.List (intersperse, sort)

startGame :: IO ()
startGame = do
    putStrLn "How long of a word would you like?"
    lenS <- getLine
    let len = (read lenS :: Int)
    wordList <- wordsOfLength len
    let gs = newGameState len wordList
    finalState <- gameLoop (return gs)
    printf "The word was %s!!!\n" (show $ trie finalState)

gameLoop :: IO GameState -> IO GameState
gameLoop gs = do
    g <- gs
    putStr "Guess a letter: "
    hFlush stdout
    guess <- getLine
    let c = if null guess then ' ' else guess!!0
    newg <- if c == ' ' || elem c (usedChars g) then gameLoop gs else return $ takeTurn c g
    printf "%s | %d/%d remain | [%s]\n" (intersperse ' ' (fixedChars newg)) (T.size (trie newg)) (origSize newg) (intersperse ' ' (sort (usedChars newg)))
    if missingLetters newg == 0 then return newg else gameLoop (return newg)
