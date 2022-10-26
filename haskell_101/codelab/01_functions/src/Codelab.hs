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
import Prelude hiding (gcd)

-- CODELAB 01: Number manipulation
--
-- As we have not looked at any complex data structures yet, the only thing
-- we have for now is numbers.

add :: Int -> Int -> Int
add x y = codelab

subtract :: Int -> Int -> Int
subtract x y = codelab

double :: Int -> Int
double x = codelab

multiply :: Int -> Int -> Int
multiply x y = codelab

-- Note that Haskell is strict about types even for basic integral types.
-- Int is never automatically converted to Double.  But you can use
-- fromIntegral to convert from any integral type to any number type.
divide :: Int -> Int -> Double
divide x y = codelab

-- Remember that you can use if/then/else:
--
--  if <expr> then <expr> else <expr>
--
-- Integer is just like Int, except that it can store arbitrarily large
-- numbers. Without a type like this, we couldn't compute the factorial of
-- even relatively small numbers.
--
-- Remember that function application binds tighter than operators!
-- E.g.: `3 * f 4` is the same as `3 * (f 4)`, not `(3 * f) 4`.
factorial :: Integer -> Integer
factorial n = codelab

-- Consider Euclid's algorithm:
--
--   https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid's_algorithm
gcd :: Int -> Int -> Int
gcd a b = codelab
