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
  [ test "null []"                     True      $ C.null []
  , test "null [8,0,6]"                False     $ C.null [8,0,6]
  , test "head [8,0,6]"                8         $ C.head [8,0,6]
  , test "tail [8,0,6]"                [0,6]     $ C.tail [8,0,6]
  , test "length []"                   0         $ C.length []
  , test "length [8,0,6]"              3         $ C.length [8,0,6]
  , test "and    []"                   True      $ C.and []
  , test "and    [True]"               True      $ C.and [True]
  , test "and    [False]"              False     $ C.and [False]
  , test "and    [True,  True]"        True      $ C.and [True,  True]
  , test "and    [True,  False]"       False     $ C.and [True,  False]
  , test "and    [False, True]"        False     $ C.and [False, True]
  , test "and    [False, False]"       False     $ C.and [False, False]
  , test "and    [True, True, True]"   True      $ C.and [True, True, True]
  , test "or     []"                   False     $ C.or []
  , test "or     [True]"               True      $ C.or [True]
  , test "or     [False]"              False     $ C.or [False]
  , test "or     [True,  True]"        True      $ C.or [True,  True]
  , test "or     [True,  False]"       True      $ C.or [True,  False]
  , test "or     [False, True]"        True      $ C.or [False, True]
  , test "or     [False, False]"       False     $ C.or [False, False]
  , test "or     [False, False, True]" True      $ C.or [False, False, True]
  , test "[8,0] ++ [   ]"              [8,0]     $ [8,0] C.++ []
  , test "[   ] ++ [6,4]"              [6,4]     $ [   ] C.++ [6,4]
  , test "[8,0] ++ [6,4]"              [8,0,6,4] $ [8,0] C.++ [6,4]
  ]
