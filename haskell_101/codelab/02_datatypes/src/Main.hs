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
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  [ test "hours (Minutes 271)" 4 $ C.hours (C.Minutes 271)
  , test "hours (Minutes 15)" 0 $ C.hours (C.Minutes 15)
  , test "timeDistance (Minutes 15) (Minutes 25)" (C.Minutes 10) $ C.timeDistance (C.Minutes 15) (C.Minutes 25)
  , test "timeDistance (Minutes 99) (Minutes 47)" (C.Minutes 52) $ C.timeDistance (C.Minutes 99) (C.Minutes 47)
  , test "pointDistance (1, 1) (1, 3)" 2 $ C.pointDistance (1, 1) (1, 3)
  , test "pointDistance (3, 4) (0, 0)" 5 $ C.pointDistance (3, 4) (0, 0)
  ]

deriving instance Eq C.Minutes
deriving instance Show C.Minutes
