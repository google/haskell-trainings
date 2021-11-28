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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

module Codelab where

import Internal (codelab)

-- BONUS CODELAB: let's play a game.
--
-- This section goes a bit further and is optional.
--
-- In it, we implement a small (and, arguably, not very interesting)
-- game: Rock Paper Scissors! You don't have to write a lot of code in
-- this section; simply take the time to read the code, and fill in the
-- few blanks. You'll encounter quite a few functions you haven't seen
-- before, and some new weird syntax: if you import this file in GHCI,
-- you can easily inspect the type of any function with :t.
--
-- To play a game, simply type "play" in GHCI!
-- Feel free to try to modify the code and tweak it as you wish.

-- Some simple types for our game.  Ignore the "deriving" part (or don't,
-- I'm a comment, not a cop).
data Hand = Rock | Paper | Scissors deriving (Show, Read, Eq, Enum, Bounded)
type Score = (Int, Int)

-- "winsOver" tells you if a hand wins over another one.  It introduces a
-- nifty trick: any binary function can be used in an infix way if
-- surrounded by backquotes.
winsOver :: Hand -> Hand -> Bool
Rock     `winsOver` Scissors = True
Paper    `winsOver` Rock     = True
Scissors `winsOver` Paper    = True
_        `winsOver` _        = False

-- "computeScore"... computes the score!
-- Remember those | guards?
computeScore :: Hand -> Hand -> Score
computeScore h1 h2
  | h1 `winsOver` h2 = (1, 0)
  | h2 `winsOver` h1 = (0, 1)
  | otherwise        = (0, 0)

-- "combine"... combines!
-- Remember pattern matching?
combine :: Score -> Score -> Score
combine (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)


-- Ok, here's where you come in.
--
-- We want to create a function "score", that takes the two lists of hands
-- the players have played, computes the score at each round, then combines
-- all the scores to yield the final count.
--
-- This function is partially pre-defined, using the ($) operator, to
-- showcase how easily you can combine existing functions into new ones.
-- Your job is to figure out which function goes where.
--
-- Here is the list of functions you will need:
--     combine      :: Score -> Score -> Score
--     computeScore :: Hand  -> Hand  -> Score
--     uncurry      :: (a -> b -> c) -> ((a, b) -> c)
--     foldl1       :: (a -> a -> a) -> [a] -> a
--     map          :: (a -> b) -> [a] -> [b]
--     zip          :: [a] -> [b] -> [(a, b)]
pairScore :: (Hand, Hand) -> Score
pairScore = codelab codelab

score :: [Hand] -> [Hand] -> Score
score h1 h2 = codelab codelab $ codelab codelab $ codelab h1 h2
-- Hint: It creates a list of plays by merging the two lists,
--       then it scores each play,
--       then it sums the scores.
--       merge -> map -> reduce

-- More definitions for the game are defined in src/Main.hs (warning: 102 spoilers!)

-- #####################################################################
-- BONUS BONUS SECTION: wait, you actually read all of that?
--
-- Just for fun, here are a few common one-liners; can you guess what they
-- do, what they are, without testing them in GHCI?

-- This is a lazily evaluated list - the first value is 0, the second value is
-- 1, and subsequent values are calculated (lazily) by the given expression.
mystic :: [Integer]
mystic = 0 : 1 : zipWith (+) mystic (tail mystic)

-- The /= operator below means "not equal", just like != in Java/C/Python/etc.
-- Puzzle: would a `where` (instead of `let ... in ...`) work here?
valor :: [Integer]
valor = let s l = head l : s [n | n <- tail l, (n `mod` head l) /= 0]
        in s [2..]

instinct :: [Int] -> [Int]
instinct []     = []
instinct (x:xs) = instinct [a | a <- xs, a < x] ++ [x] ++ instinct (filter (>= x) xs)
