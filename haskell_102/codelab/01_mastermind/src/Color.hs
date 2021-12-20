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

module Color where

import Internal (codelab)


-- SECTION 1: Color (Enum and Bounded)

-- Our game, being a coding exercise, works with the six dev colors: red,
-- yellow, green, cyan, blue, and magenta.
data Color
  = Red     -- this is a constructor, of type Color
  | Yellow  -- same here
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving
    ( Ord -- the compiler automagically generates the instances for these
    , Eq
    , Enum
    , Bounded
    )

-- We want to have a list of all the colors. We could write such a list
-- manually, but that'd be cumbersome and error-prone. Thankfully, lists
-- support interpolation! The [a .. b] syntax is translated into a call to
-- enumFromTo (defined in the Enum typeclass). Here, all you have to do is
-- figure out which color is the minimum color, and which is the max. Some
-- other typeclass might help you?
allColors :: [Color]
allColors = [minColor .. maxColor] -- this is enumFromTo minColor maxColor
  where
    minColor = codelab
    maxColor = codelab

-- We should also provide a way to display values of type Color.
-- Let's make `show` return only the first letter of the color's name.
instance Show Color where
  show = codelab

-- We will not write the Read instance to convert a String to a Color because
-- read is partial and we want to handle the error case ourselves (see section
-- 3).
