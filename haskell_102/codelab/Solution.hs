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

Welcome to the Haskell 102 codelab!

To run the codelab, make sure you have the Haskell Platform, and simply:
$ make
$ ./test_codelab
$ ./codelab play

You can also test your functions in GHCI:

$ ghci
> :l Tests
> main            -- run the tests
> :r              -- recompile


It will fail hilariously (or ridiculously, depending on your sense of
humor), because you're supposed to write some of the code! You have to
replace and code everything that is named "codelab". Our goal, here, is to
implement a game of Mastermind, and a solver for it. Good luck, and may the
odds always be in your favor!

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
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

module Solution where

import Data.List  hiding (lookup, insert)
import Data.Map   (Map, empty, insert, insertWith, lookup)
import Data.Maybe
import Prelude    hiding (lookup)

codelab :: a
codelab = error "SOMETHING IS NOT IMPLEMENTED!"





{- #####################################################################
   SECTION 1: Color (Enum and Bounded)

   Our game, being a coding exercise, works with the six dev colors: red,
   yellow, green, cyan, blue, and magenta.
-}

data Color = Red     -- this is a constructor, of type Color
           | Yellow  -- same here
           | Green
           | Cyan
           | Blue
           | Magenta
             deriving ( -- the compiler automatically generates the
               Ord,     -- instances for those built-in typeclasses
               Eq,
               Enum,
               Bounded
               )


-- [1.1]
-- We want to have a list of all the colors. We could write such a list
-- manually, but that'd be cumbersome and error-prone. Thankfully,
-- lists support interpolation! The [a .. b] syntax is translated into
-- a call to enumFromTo (defined in the Enum typeclass). Here, all you
-- have to do is figure out which color is the minimum color, and which
-- is the max. Some other typeclass might help you?

allColors :: [Color]
allColors = [minColor .. maxColor] -- enumFromTo minColor maxColor
  where minColor = minBound
        maxColor = maxBound





{- #####################################################################
   SECTION 2: ColorMap (how to use maps)

   We will use color maps to count the occurrences of each color in a
   code. The type Map comes from Data.Map. Its documentation is here:

   https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html

   A ColorMap is a map from a Color to an Int: the code [R, R, G, B] would
   be represented as the map: {R: 2, G: 1, B: 1}.
-}

type ColorMap = Map Color Int


-- [2.1]
-- This simple helper extracts an Int value out of a Maybe. If there is
-- no value to extract, it returns 0. You can implement it by pattern
-- matching, but there is a shorter way to implement it.

-- https://hackage.haskell.org/package/base/docs/Data-Maybe.html

getIntOr0 :: Maybe Int -> Int
getIntOr0 = fromMaybe 0


-- [2.2]
-- "getCount" extracts a color count from a color map; if the color isn't
-- in the map, it returns 0 instead.  To implement it, you will need a
-- lookup function:
--
--     lookup :: key -> Map key value -> Maybe value

getCount :: Color -> ColorMap -> Int
getCount color = getIntOr0 . lookup color


-- [2.3]
-- Increase the count of a color in the map by 1. Since a map is immutable,
-- you in fact create a new one with the modification.  The two functions
-- you will need are:
--
--     getCount   :: Color -> ColorMap -> Int
--     insert     :: Color -> Int -> ColorMap -> ColorMap
--
-- For a fancier version, you can look up "insertWith".

addColorToMap :: Color -> ColorMap -> ColorMap
addColorToMap color = insertWith (+) color 1





{- #####################################################################
   SECTION 3: ErrorOr (handling errors in pure code)

   We know about Maybe. It is used to represent an optional value.
   But sometimes, when a computation fails, we want to have more
   information than just a "Nothing" value.

   For those purposes, we'll introduce here the type "ErrorOr". The
   error it may or may not contain is simply a String.

   Interestingly, it is a Monad! We'll implement the Monad typeclass
   for it.

   TO GO FURTHER: there is a built-in type that does exactly the same
   thing: it is Either a b; though there is no need to know about it
   for this codelab, you can read about it here:
   https://hackage.haskell.org/package/base/docs/Data-Either.html
-}

-- An error message is just a String.

type ErrorMsg = String


-- ErrorOr has two constructors; a value of type "ErrorOr a" is either one
-- of the other.

data ErrorOr a = Error ErrorMsg -- an error with a message
               | Value a        -- a wrapped value of type a
               deriving (Show, Eq)


-- [3.1]
-- "wrapValue" takes a value, and puts it in the context of an "ErrorOr a".

wrapValue :: a -> ErrorOr a
wrapValue = Value


-- [3.2]
-- "fmapValue" takes a function, and tries to apply it on the value inside
-- the "ErrorOr a". If it cannot apply the function because the "ErrorOr a"
-- contains an error, it simply returns this existing error. We do a simple
-- pattern match to decide what to do.

fmapValue :: (a -> b) -> ErrorOr a -> ErrorOr b
fmapValue _ (Error msg) = Error msg
fmapValue f (Value   x) = Value $ f x


-- [3.3]
-- "apValue" is the version of "ap" for our "ErrorOr" type. The first value
-- is an "ErrorOr (a -> b)": if we indeed have a function in it, we can
-- apply it on the second argument; if we don't, we simply keep the
-- error. To apply the function, we will need a way to apply a function on
-- a contextual value...

apValue :: ErrorOr (a -> b) -> ErrorOr a -> ErrorOr b
apValue (Error msg) _   = Error msg
apValue (Value   f) eoa = fmapValue f eoa


-- [3.4]
-- Finally, "bindValue" is our version of "bind". It works exactly like
-- "fmapValue", except we don't have to wrap the result.

bindValue :: (a -> ErrorOr b) -> ErrorOr a -> ErrorOr b
bindValue _ (Error msg) = Error msg
bindValue f (Value   x) = f x





{- #####################################################################
   SECTION 4: Code (combining functions)

   A code is a list of colors, that we will simply represent as...
   a list of colors. This section aims at implementing the functions
   that we will need to compare two codes.

   Basically, we compute three values:
       total: the number of colors the codes have in common
       black: the number of correct colors at the correct position
       white: total - black
-}

type Code  = [Color]
data Score = Score
             { scoreBlack :: Int
             , scoreWhite :: Int } deriving (Eq)


-- [4.1]
-- "allCodes" generates all possible codes of a given size.
--     allCodes 0 = [[]]
--     allCodes 1 = [[Red], [Yellow]...]
--     allCodes 2 = [[Red, Red], [Red, Yellow]...]
--
-- We use list comprehension to generate the list of codes.  The trick,
-- here, is recursion: the codes of size n are obtained by adding all
-- possible colors to all possible codes of size (n-1).  The syntax of list
-- comprehension in Haskell is:
--     [result | value1 <- container1, value2 <- container2]
-- The same list comprehension in Python would be:
--     [result for value1 in container1 for value2 in container2]
--
-- As a reminder: this guard syntax is equivalent to a series of if / else:
-- if s < 0
-- then ...
-- else if s == 0
--      then ...
--      else ...

allCodes :: Int -> [Code]
allCodes s
  | s <  0    = error "allCodes: size was lower than 0"
  | s == 0    = [[]]
  | otherwise = [color:code | color <- allColors, code <- allCodes (s - 1)]


-- [4.2]
-- Transforms a code into the corresponding map of Color to Int. To do so,
-- we fold ("reduce") the list, by using a ColorMap as the accumulator. You
-- will need the following functions:
--
--     foldr         :: (Color -> ColorMap -> ColorMap) -> ColorMap -> [Color] -> ColorMap
--     addColorToMap ::  Color -> ColorMap -> ColorMap
--     empty         ::                                    ColorMap

codeToMap :: Code -> ColorMap
codeToMap = foldr addColorToMap empty


-- [4.3]
-- This function computes the black score of two given codes.  To do so, we
-- "zip" the two lists together to compare them.
--
--     [ R  G  B  M ]
--   ? [ R  B  Y  M ]
--  -> [ 1  0  0  1 ]
--  -> 2
--
-- To compute the result, you will need:
--
--     zipWith  :: (Color -> Color -> Bool) -> [Color] -> [Color] -> [Bool]
--     (==)     ::  Color -> Color -> Bool
--     map      :: (Bool -> Int) -> [Bool] -> [Int]
--     fromEnum ::  Bool -> Int
--     sum      :: [Int] -> Int
--
-- (If this one seems complicated, try testing zip and zipWith in GHCI!)
-- For bonus points, reimplement it with "filter" or with a list comprehension.

countBlacks :: Code -> Code -> Int
countBlacks l1 l2 = sum $ map fromEnum $ zipWith (==) l1 l2


-- [4.4]
-- This one computes the total number of colors in common between two
-- codes, by using ColorMaps. For each color, we take the minimum of the
-- values in each map. You will need:
--     codeToMap :: Code -> ColorMap
--     allColors :: [Color]
--     getCount  :: Color -> ColorMap -> Int
--     map       :: (Color -> Int) -> [Color] -> [Int]
--     sum       :: [Int] -> Int

countTotal :: Code -> Code -> Int
countTotal c1 c2 = sum $ map compareColor allColors
  where compareColor :: Color -> Int
        compareColor color = min (getCount color cmap1) (getCount color cmap2)
        cmap1, cmap2 :: ColorMap
        cmap1 = codeToMap c1
        cmap2 = codeToMap c2


-- [4.5]
-- Finally, "countScore" takes two codes and computes the score. :)

countScore :: Code -> Code -> Score
countScore c1 c2 = Score black white
  where black = countBlacks c1 c2
        total = countTotal  c1 c2
        white = total - black




{- #####################################################################
   SECTION 5: "do" notation

   While "do" notation is commonly used for "imperative" code, it can be
   used with anything that is a monad (or even an applicative).  As we have
   seen in the slides, lists are monads, meaning we can use "do" notation
   to write our list generators in a very flexible way!

   For example, a generator like this:

     [x + 1 | x <- [1..10]]

   can be written using "do" notation like this:

     do x <- [1..10]
        return $ x + 1

   Code inside the "do" block must evaluate to a "wrapped" value, which
   would be a list in our case.  If you want to return just one value, you
   can use the "return" function to wrap a single value to produce a value
   in the context.

-}

-- [5.1]
-- Can you rewrite "allColors" to use the "do" notation?
--
-- Note that inside "do" blocks Haskell switches to indentation sensitive
-- mode!  It is like Python but better, as the rules are more nuanced.
-- Make sure you follow the indentation proposed in the examples.

allCodesDo :: Int -> [Code]
allCodesDo s
  | s <  0    = error "allCodes: size was lower than 0"
  | s == 0    = [[]]
  | otherwise = do color <- allColors
                   code <- allCodesDo (s - 1)
                   return $ color:code

-- [5.2]
-- Unlike generators, a "do" block can return any wrapped value.  For
-- lists, it means it can return any list, not necessarily a list of length
-- 1.  Let's build a "generator" using the "do" notation that would produce
-- a list of duplicates of the specified length.  For example, for length 5
-- it should produce
--
--   [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]

duplicatesList :: Int -> [Int]
duplicatesList len =
  do i <- [1..len]
     [i, i]


-- [5.3]
-- How about the case when the length of different "blocks" would be
-- different?  Let's build a "generator" similar to the previous one, but
-- that would duplicate only odd values.  For example, for length 5 it
-- should produce
--
--   [1, 1, 2, 3, 3, 4, 5, 5]
--
-- I this, this is not something that can be easily expressed with an
-- ordinary generator, is it?
--
-- Do not forget about Hoogle, should you need a new function.
--
-- Hint: Should you use a conditional expression, make sure to put it in
-- one line, as we did not discuss correct indentation rules for that case.

oddlyDuplicateList :: Int -> [Int]
oddlyDuplicateList len =
  do i <- [1..len]
     if odd i
       then [i, i]
       else [i]

-- When you solve [5.3], think about the fact that when coding in "do"
-- notation you have the full power of the language, but you are building
-- something like a generator.  For imperative code that uses IO, your
-- generator is producing sequences of actions that your application needs
-- to execute to actually achieve the desired result.
--
-- This approach provides an amazing level of flexibility in terms of
-- abstraction.  You can apply all the abstraction, code sharing and other
-- techniques to the code that produces imperatives actions, as you
-- normally apply to the code that is building lists.





{- #####################################################################
   THE END

   You've reached the end of the codelab !
   But we're not done with the game itself: we haven't seen any error
   handling, user input, or even IO! The rest of the code is in Game.hs:
   though there is no codelab function for you to replace there, please
   take the time to read it and try the functions in GHCI.
-}

-- -*- fill-column: 75; -*-
