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

module ColorMapSolution where

import Data.Map (Map, insertWith, lookup)
import Data.Maybe
import Prelude hiding (lookup)

import ColorSolution
import Internal (codelab)


-- SECTION 2: ColorMap (how to use maps)
--
-- We will use color maps to count the occurrences of each color in a code. The
-- type Map comes from Data.Map. Its documentation is here:
--
-- https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html
--
-- A ColorMap is a map from a Color to an Int: the code [R, R, G, B] would be
-- represented as the map: {R: 2, G: 1, B: 1}.
type ColorMap = Map Color Int

-- This simple helper extracts an Int value out of a Maybe. If there is
-- no value to extract, it returns 0. You can implement it by pattern
-- matching, but there is a shorter way to implement it.
-- See https://hackage.haskell.org/package/base/docs/Data-Maybe.html
getIntOr0 :: Maybe Int -> Int
getIntOr0 = fromMaybe 0

-- "getCount" extracts a color count from a color map; if the color isn't
-- in the map, it returns 0 instead.  To implement it, you will need a
-- lookup function:
--
--     lookup :: key -> Map key value -> Maybe value
getCount :: Color -> ColorMap -> Int
getCount color cmap = getIntOr0 $ lookup color cmap

-- Increase the count of a color in the map by 1. Since a map is immutable,
-- you in fact create a new one with the modification.  The two functions
-- you will need are:
--
--     getCount   :: Color -> ColorMap -> Int
--     insert     :: Color -> Int -> ColorMap -> ColorMap
--
-- For a fancier version, you can look up "insertWith".
addColorToMap :: Color -> ColorMap -> ColorMap
addColorToMap color cmap = insertWith (+) color 1 cmap
