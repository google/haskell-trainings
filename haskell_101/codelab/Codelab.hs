-- Copyright 2019 Google LLC
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



{-

Welcome to the Haskell 101 codelab!

To run this file, make sure you have the haskell platform, and simply:
$ make
$ ./codelab

You can also load this file in ghci:

$ ghci
> :set -DCODELAB
> :l Codelab
> :l Main
> main
> :r
> main

It will fail hilariously (or ridiculously, depending on your sense of
humour), because you're supposed to write some of the code! You have to
replace and code everything that is named "codelab".

Our goal, here, is to implement some of the builtin functions in the
language, as a way to get familiar with the type system.

Good luck and, most importantly, have fun!

-}





{- #####################################################################
   SECTION 0: setting up the codelab.

   Nothing to see here, please go directly to section 1!

   This section simply define the "codelab" alias that we'll use
   everywhere in the code and still have it compile and does all the
   necessary boilerplate: imports, compiler options, that kind of stuff.
-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}

module Codelab where

import Control.Monad        (void)
import Data.Maybe           (isJust)
import Text.Read            (readMaybe)
import Prelude       hiding (null, head, tail, length, and, or, (++),
                             map, filter, foldr, foldl)

codelab :: a
codelab = error "SOMETHING IS NOT IMPLEMENTED!"





{- #####################################################################
   SECTION 1: deconstructing lists

   The default list is ubiquitous in the Prelude; the default String
   type is but a type alias to [Char] after all. Though they have
   limitations, they're always useful.

   As a reminder, a list is either:
     * []     the empty list
     * (x:xs) a cell containing the value x and followed by the list xs
-}


-- null tells you whether a list is empty or not

null :: [a] -> Bool
null fixme = codelab


-- head returns the first element of the list
-- if the list is empty, it panics: this function is partial

head :: [a] -> a
head []    = error "head: empty list"
head fixme = codelab


-- tail returns everything but the first element
-- if the list is empty it panics

tail :: [a] -> [a]
tail = codelab





{- #####################################################################
   SECTION 2: recursion (c.f. SECTION 2)

   There is no loop in Haskell, so to go through a list, we have to use
   recursion. Here are a few more common functions for you to
   reimplement!
-}


-- do you remember it from the slides?

length :: [a] -> Int
length l = codelab


-- and returns True if all the boolean values in the list are True
-- what do you think it returns for an empty list?

and :: [Bool] -> Bool
and l = codelab


-- or returns True if at least one value in the list is True
-- what do you think it returns for an empty list?

or :: [Bool] -> Bool
or l = codelab


-- (++) is the concatenation operator
-- to concatenate two linked lists you have to chain the second one
-- at the end of the first one

(++) :: [a] -> [a] -> [a]
l1 ++ l2 = codelab





{- #####################################################################
   SECTION 3: abstractions

   Have you noticed that we keep using the same pattern?
   If the list is empty we return a specific value.
   If it is not, we call a function to combine the element with the
   result of the recursive calls.

   This is Haskell: if there is a pattern, it can (must) be abstracted!
   Fortunately, some useful functions are here for us.

   To understand the difference between foldr and foldl, remember that
   the last letter indicates if the "reduction" function is left
   associative or right associative: foldr goes from right to left,
   foldl goes from left to right.

   foldl :: (a -> x -> a) -> a -> [x] -> a
   foldr :: (x -> a -> a) -> a -> [x] -> a
   foldl (-) 0 [1,2,3,4]   ==   (((0 - 1) - 2) - 3) - 4   ==   -10
   foldr (-) 0 [1,2,3,4]   ==   1 - (2 - (3 - (4 - 0)))   ==    -2
-}


-- you probably remember this one?
-- nothing extraordinary here

map :: (a -> b) -> [a] -> [b]
map _ []     = codelab
map f (a:as) = codelab


-- same thing here for filter, except that we use it to introduce a new
-- syntax: those | are called guards; they let you specify different
-- implementations of your function depending on some Boolean
-- value. "otherwise" is not a keyword but simply a constant whose
-- value is True! Try to evaluate "otherwise" in GHCI.
--
-- Simple example of guards usage:
--   abs :: Int -> Int
--   abs x
--     | x < 0     = -x
--     | otherwise =  x

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = codelab
filter f (x:xs)
  | codelab   = codelab
  | otherwise = codelab


-- foldl
-- foldl (-) 0 [1,2,3,4]   ==   (((0 - 1) - 2) - 3) - 4   ==   -10

foldl :: (a -> x -> a) -> a -> [x] -> a
foldl _ a []     = codelab
foldl f a (x:xs) = codelab


-- foldr
-- foldr (-) 0 [1,2,3,4]   ==   1 - (2 - (3 - (4 - 0)))   ==    -2

foldr :: (x -> a -> a) -> a -> [x] -> a
foldr _ a []     = codelab
foldr f a (x:xs) = codelab





{- #####################################################################
   BONUS STAGE!

   For fun, you can try reimplementing all the functions in section 2
   with foldr or foldl! For length, remember that the syntax for a
   lambda function is (\arg1 arg2 -> value).

   You can replace your previous implementation if you want. Otherwise,
   you can add new functions (such as andF, orF), and test them by
   loading your file in GHCI:

   $ ghci
   > :load Codelab
   > and  []
   > andF []

   To go a bit further, you can also try QuickCheck:

   > import Test.QuickCheck
   > quickCheck $ \anyList -> and anyList == andF anyList

   QuickCheck automatically generates tests based on the types
   expected (here, list of boolean values).

   It is also worth noting that there is a special syntax for list
   comprehension in Haskell, which is at a first glance quite similar
   to the syntax of Python's list comprehension

   Python:  [transform(value) for value in container if test(value)]
   Haskell: [transform value  |   value <- container ,  test value ]

   This allows you to succinctly write your map / filters.
-}





{- #####################################################################
   SECTION 4: am I being indecisive? ....hmmmm Maybe?

   Partial functions are bad. Null pointers are a billion dollar
   mistake. Sometimes, what we just want is to have an optional value,
   a value that is either here or not, but with type safety.

   Remember Maybe? If not, here's the definition:

   data Maybe a = Nothing | Just a
-}


-- if we were to fix the "head" function, how could we do that?

safeHead :: [a] -> Maybe a
safeHead []    = codelab
safeHead (x:_) = codelab


-- isNothing should not need an explanation by now!

isNothing :: Maybe a -> Bool
isNothing = codelab


-- the fromMaybe function is your way out of a Maybe value
-- it takes a default value to use in case our Maybe value is Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe _ _ = codelab
-- Consider starting with these patterns:
--
-- fromMaybe def fixme = codelab
-- fromMaybe _   fixme = codelab


-- the maybe function is an extended version of fromMaybe
-- can you guess what it is supposed to do?
-- ...doesn't it kinda look like fold?

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ _ _ = codelab
-- Consider starting with these patterns:
-- maybe b _ fixme = codelab
-- maybe _ f fixme = codelab





{- #####################################################################
   PARTING WORDS

   Have you noticed that we pattern match with Maybe quite like we do
   with lists? You haven't seen Either yet, but spoilers: the pattern
   matching looks quite the same.

   Could we therefore define an equivalent of map for Maybe? For Either?
   But how could we write a function with the same name for different
   types? Will we end up needing some kind of *shivers* interface?

   Stay tuned for Haskell 102! :)

   (If you want more, head below for a bonus section!)
-}





{- #####################################################################
   BONUS SECTION: let's play a game.

   This sections goes a bit further and is optional.

   In it, we implement a small (and, arguably, not very interesting)
   game: Rock Paper Scissors! You don't have to write a lot of code in
   this section; simply take the time to read the code, and fill in the
   few blanks. You'll encounter quite a few functions you haven't seen
   before, and some new weird syntax: if you import this file in GHCI,
   you can easily inspect the type of any function with :t.

   To play a game, simply type "play" in GHCI!
   Feel free to try to modify the code and tweak it as you wish.
-}


-- some simple types for our game
-- ignore the "deriving" part (or don't, I'm a comment, not a cop)

data Hand = Rock | Paper | Scissors deriving (Show, Read, Eq)
type Score = (Int, Int)


-- winsOver tells you if a hand wins over another one
-- it introduces a nifty trick: any binary function can be used in an
-- infix way if surrounded by backquotes

winsOver :: Hand -> Hand -> Bool
Rock     `winsOver` Scissors = True
Paper    `winsOver` Rock     = True
Scissors `winsOver` Paper    = True
_        `winsOver` _        = False


-- computeScore... computes the score!
-- remember those | guards?

computeScore :: Hand -> Hand -> Score
computeScore h1 h2
  | h1 `winsOver` h2 = (1, 0)
  | h2 `winsOver` h1 = (0, 1)
  | otherwise        = (0, 0)


-- combine... combines!
-- remember pattern matching?

combine :: Score -> Score -> Score
combine (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)


-- ok, here's where you come in

-- we want to create a function score, that takes the two lists of hands
-- the players have played, computes the score at each round, then
-- combines all the scores to yield the final count

-- this functions is partially pre-defined, using the ($) operator, to showcase
-- how easily you can combine existing functions into new ones; your job
-- is to figure out which function goes where

-- here is the list of functions you will need:
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

-- hint: it creates a list of plays by merging the two lists,
--       then it scores each play,
--       then it sums the scores.
--       merge -> map -> reduce


-- we play up to 3

gameOver :: Score -> Bool
gameOver (s1, s2) = s1 >= 3 || s2 >= 3


-- below is the impure IO code that lets us read hands from the
-- standard input and play the game!
-- beware: Haskell 102 spoilers!

readHand :: String -> IO Hand
readHand prompt = do
  putStr prompt                  -- prints the prompt
  handText <- getLine            -- reads one line of input
  case readMaybe handText of     -- tries to convert it to Hand
     Just h  -> return h         -- success: our result is h
     Nothing -> readHand prompt  -- failure: we try again

playTurn :: Score -> IO Score
playTurn oldScore = do
  h1 <- readHand "p1: "
  h2 <- readHand "p2: "
  let turnScore = computeScore h1 h2
      newScore  = combine oldScore turnScore
  print newScore
  if gameOver newScore
    then return   newScore
    else playTurn newScore

play :: IO ()
play = void $ playTurn (0,0)





{- #####################################################################
   BONUS BONUS SECTION: wait, you actually read all of that?

   Just for fun, here are a few common one-liners; can you guess what
   they do, what they are, without testing them in GHCI?
-}


mystic :: [Integer]
mystic = 0 : 1 : zipWith (+) mystic (tail mystic)

valor :: [Integer]
valor = let s l = head l : s [n | n <- tail l, n `mod` head l /= 0] in s [2..]

instinct :: [Int] -> [Int]
instinct []     = []
instinct (x:xs) = instinct [a | a <- xs, a < x] ++ [x] ++ (instinct $ filter (>= x) xs)
