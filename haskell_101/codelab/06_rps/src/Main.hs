-- Copyright 2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (unless)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Text.Read (readMaybe)

#ifdef SOLUTION
import Solution (Hand, Score, score)
#else
import Codelab  (Hand, Score, score)
#endif

main :: IO ()
main = hSetBuffering stdout NoBuffering >> printHelp >> play

play :: IO ()
play = playTurn [] []

-- We play up to 3.
gameOver :: Score -> Bool
gameOver (s1, s2) = s1 >= 3 || s2 >= 3

-- Below is the impure IO code that lets us read hands from the standard
-- input and play the game!
-- Beware: Haskell 102 spoilers!
readHand :: String -> IO Hand
readHand prompt = do
  putStr prompt                         -- prints the prompt
  handText <- getLine                   -- reads one line of input
  case readMaybe handText of            -- tries to convert it to Hand
     Just h  -> return h                -- success: our result is h
     Nothing -> badMove prompt handText -- failure: we try again

playTurn :: [Hand] -> [Hand] -> IO ()
playTurn p1 p2 = do
  h1 <- readHand "p1: "
  h2 <- readHand "p2: "
  let p1' = h1 : p1
      p2' = h2 : p2
      newScore = score p1' p2'
  print newScore
  unless (gameOver newScore) $ playTurn p1' p2'

-- Helpers, again using Haskell 102 spoilers!
printHelp :: IO ()
printHelp = printHeader >> printMoves

printHeader :: IO ()
printHeader = do
  putStrLn "Welcome to Rock-Paper-Scissors!"
  putStrLn "-------------------------------"

printMoves :: IO ()
printMoves = do
  putStr "Each move is one of "
  putStrLn (show [minBound :: Hand ..])

badMove :: String -> String -> IO Hand
badMove prompt move = do
  putStr "Bad move: "
  putStrLn move
  printMoves
  readHand prompt
