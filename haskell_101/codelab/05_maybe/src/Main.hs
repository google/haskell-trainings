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
  [ test "safeHead  []"           Nothing  $ C.safeHead ([] :: [Int])
  , test "safeHead  [8,0,6]"      (Just 8) $ C.safeHead [8,0,6]
  , test "isNothing (Just 42)"    False    $ C.isNothing (Just 42)
  , test "isNothing Nothing"      True     $ C.isNothing Nothing
  , test "fromMaybe 0 (Just 40)"  40       $ C.fromMaybe 0 (Just 40)
  , test "fromMaybe 0 Nothing"    0        $ C.fromMaybe 0 Nothing
  , test "maybe 0 (+2) (Just 40)" 42       $ C.maybe 0 (+2) (Just 40)
  , test "maybe 0 (+2) Nothing"   0        $ C.maybe 0 (+2) Nothing
  ]
