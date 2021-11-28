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
  [ test "map    (+1)   []"        []                     $ C.map (+1) []
  , test "map    (+1)   [8,0,6,4]" [9,1,7,5]              $ C.map (+1) [8,0,6,4]
  , test "filter (>5)   []"        []                     $ C.filter (>5) []
  , test "filter (>5)   [8,0,6,4]" [8,6]                  $ C.filter (>5) [8,0,6,4]
  , test "foldl  (-)  1   [10]" (-9)                      $ C.foldl (-) 1 [10]
  , test "foldl  (-)  0   [1,2,3,4]" (-10)                $ C.foldl (-) 0 [1,2,3,4]
  , test "foldl  (++) \"_\" [\"A\", \"B\", \"C\"]" "_ABC" $ C.foldl  (++) "_" ["A","B","C"]
  , test "foldr  (-)  1   [10]"   9                       $ C.foldr (-) 1 [10]
  , test "foldr  (-)  0   [1,2,3,4]" (-2)                 $ C.foldr (-) 0 [1,2,3,4]
  , test "foldr  (++) \"_\" [\"A\", \"B\", \"C\"]" "ABC_" $ C.foldr  (++) "_" ["A","B","C"]
  ]
