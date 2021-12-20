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

module CodeSolution where

import Data.Map hiding (foldr, filter, map)

import ColorSolution
import ColorMapSolution
import Internal (codelab)


-- SECTION 4: Code (combining functions)
--
-- A code is a list of colors, that we will simply represent as...
-- a list of colors. This section aims at implementing the functions
-- that we will need to compare two codes.
--
-- Basically, we compute three values:
--     total: the number of colors the codes have in common
--     black: the number of correct colors at the correct position
--     white: total - black

type Code  = [Color]
data Score = Score
  { scoreBlack :: Int
  , scoreWhite :: Int
  } deriving Eq

instance Show Score where
  show s = concat
    [ "black: ",   show $ scoreBlack s
    , ", white: ", show $ scoreWhite s
    ]

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

-- Transforms a code into the corresponding map of Color to Int. To do so,
-- we fold ("reduce") the list, by using a ColorMap as the accumulator. You
-- will need the following functions:
--
--     foldr         :: (Color -> ColorMap -> ColorMap) -> ColorMap -> [Color] -> ColorMap
--     addColorToMap ::  Color -> ColorMap -> ColorMap
--     empty         ::                                    ColorMap
codeToMap :: Code -> ColorMap
codeToMap code = foldr addColorToMap empty code

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
countBlacks c1 c2 = sum $ map fromEnum $ zipWith (==) c1 c2
--countBlacks c1 c2 = length $ filter id $ zipWith (==) c1 c2


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
  where
    compareColor :: Color -> Int
    compareColor color = min (getCount color cmap1) (getCount color cmap2)
    cmap1, cmap2 :: ColorMap
    cmap1 = codeToMap c1
    cmap2 = codeToMap c2

-- Finally, "countScore" takes two codes and computes the score. :)
countScore :: Code -> Code -> Score
countScore c1 c2 = Score black white
  where
    black = countBlacks c1 c2
    total = countTotal c1 c2
    white = total - black
