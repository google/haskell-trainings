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

module DoSolution where

import CodeSolution
import ColorSolution
import Internal (codelab)


-- SECTION 5: "do" notation
--
-- While "do" notation is commonly used for "imperative" code, it can be used
-- with anything that is a monad (or even an applicative). As we have seen in
-- the slides, lists are monads, meaning we can use "do" notation to write our
-- list generators in a very flexible way!
--
-- For example, a generator like this:
--
--   [x + 1 | x <- [1..10]]
--
-- can be written using "do" notation like this:
--
--   do x <- [1..10]
--      return $ x + 1
--
-- Code inside the "do" block must evaluate to a "wrapped" value, which would be
-- a list in our case.  If you want to return just one value, you can use the
-- "return" function to wrap a single value to produce a value in the context.

-- Can you rewrite "allCodes" to use the "do" notation?
allCodesDo :: Int -> [Code]
allCodesDo s
  | s <  0    = error "allCodes: size was lower than 0"
  | s == 0    = [[]]
  | otherwise = do
      color <- allColors
      code  <- allCodesDo $ s - 1
      return $ color : code

-- Unlike generators, a "do" block can return any wrapped value. For lists, it
-- means it can return any list, not necessarily a list of length 1. Let's build
-- a "generator" using the "do" notation that would produce a list of duplicates
-- of the specified length.  For example, for length 5 it should produce
--
--   [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
duplicatesList :: Int -> [Int]
duplicatesList len = do
  i <- [1..len]
  [i, i]

-- What if we want the different "blocks" to have different lengths? Let's build
-- a "generator" similar to the previous one, but that would duplicate only odd
-- values. For example, for length 5 it should produce
--
--   [1, 1, 2, 3, 3, 4, 5, 5]
--
-- Do not forget about Hoogle, should you need a new function.
oddlyDuplicateList :: Int -> [Int]
oddlyDuplicateList len = do
  i <- [1..len]
  if odd i then [i, i] else [i]

-- Think about the fact that when coding in "do" notation you have the full
-- power of the language, but you are building something like a generator. For
-- imperative code that uses IO, your generator is producing sequences of
-- actions that your application needs to execute to actually achieve the
-- desired result.
--
-- This approach provides an amazing level of flexibility in terms of
-- abstraction.  You can apply all the abstraction, code sharing and other
-- techniques to the code that produces imperatives actions, as you normally
-- apply to the code that is building lists.

-- You've reached the end of the codelab !
-- But we're not done with the game itself: we haven't seen any error handling,
-- user input, or even IO! The rest of the code is in Game.hs: though there is
-- no codelab function for you to replace there, please take the time to read it
-- and try the functions in GHCI.
