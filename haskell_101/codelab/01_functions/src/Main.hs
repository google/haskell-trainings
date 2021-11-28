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

#ifdef SOLUTION
import qualified Solution as C
#else
import qualified Codelab  as C
#endif

import Internal (check, test, Tests)

main :: IO ()
main = check tests

tests :: Tests
tests =
  [ test "add       1 2"  3                 $ C.add 1 2
  , test "subtract  7 2"  5                 $ C.subtract 7 2
  , test "double    3"    6                 $ C.double 3
  , test "multiply  3 11" 33                $ C.multiply 3 11
  , test "divide    9 2"  4.5               $ C.divide 9 2
  , test "divide    8 4"  2                 $ C.divide 8 4
  , test "factorial 30"   (product [1..30]) $ C.factorial 30
  , test "gcd       12 4" 4                 $ C.gcd 12 4
  , test "gcd       17 7" 1                 $ C.gcd 17 7
  ]
