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
import Prelude hiding (map, filter, foldr, foldl)

-- CODELAB 04: Abstractions
--
-- Have you noticed that we keep using the same pattern?  If the list is
-- empty we return a specific value.  If it is not, we call a function to
-- combine the element with the result of the recursive calls.
--
-- This is Haskell: if there is a pattern, it can (must) be abstracted!
-- Fortunately, some useful functions are here for us.
--
-- To understand the difference between foldr and foldl, remember that the
-- last letter indicates if the "reduction" function is left associative or
-- right associative: foldr goes from right to left, foldl goes from left
-- to right.
--
-- foldl :: (a -> x -> a) -> a -> [x] -> a
-- foldr :: (x -> a -> a) -> a -> [x] -> a
-- foldl (-) 0 [1,2,3,4]   ==   (((0 - 1) - 2) - 3) - 4   ==   -10
-- foldr (-) 0 [1,2,3,4]   ==   1 - (2 - (3 - (4 - 0)))   ==    -2

-- You probably remember this one?  Nothing extraordinary here.
map :: (a -> b) -> [a] -> [b]
map _ []     = codelab
map f (a:as) = codelab

-- Same thing here for filter, except that we use it to introduce a new
-- syntax: those | are called "guards". They let you specify different
-- implementations of your function depending on some Boolean
-- value. "otherwise" is not a keyword but simply a constant whose value is
-- True! Try to evaluate "otherwise" in GHCI.
--
-- Simple example of guard usage:
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

-- #####################################################################
-- BONUS STAGE!
--
-- For fun, you can try reimplementing the functions in previous codelab with
-- foldr or foldl! For length, remember that the syntax for a lambda function
-- is (\arg1 arg2 -> value).
--
-- You can replace your previous implementation if you want. Otherwise, you can
-- add new functions (such as andF, orF), and test them by loading your file in
-- GHCI.
--
-- To go a bit further, you can also try QuickCheck:
--
-- > import Test.QuickCheck
-- > quickCheck $ \anyList -> and anyList == andF anyList
--
-- QuickCheck automatically generates tests based on the types expected
-- (here, list of boolean values).
--
-- It is also worth noting that there is a special syntax for list
-- comprehension in Haskell, which is at a first glance quite similar to
-- the syntax of Python's list comprehension
--
-- Python:  [transform(value) for value in container if test(value)]
-- Haskell: [transform value  |   value <- container ,  test value ]
--
-- This allows you to succinctly write your map / filters.
