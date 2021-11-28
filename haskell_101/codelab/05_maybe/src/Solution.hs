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

module Solution where

import Internal (codelab)

-- CODELAB 05: am I being indecisive? ....hmmmm Maybe?
--
-- Partial functions are bad. Null pointers are a billion dollar mistake.
-- Sometimes, what we just want is to have an optional value, a value that is
-- either here or not, but with type safety.
--
-- Remember Maybe? If not, here's the definition:
--
-- data Maybe a = Nothing | Just a


-- If we were to fix the "head" function, how could we do that?
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


-- "isNothing" should not need an explanation by now!
isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False


-- The "fromMaybe" function is your way out of a Maybe value.
-- It takes a default value to use in case our Maybe value is Nothing.
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a
-- Consider starting with these patterns:
--
-- fromMaybe def fixme = codelab
-- fromMaybe _   fixme = codelab


-- The "maybe" function is an extended version of "fromMaybe".  Can you
-- guess what it is supposed to do?
-- ...doesn't it kinda look like fold?
maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing  = b
maybe _ f (Just a) = f a
-- Consider starting with these patterns:
-- maybe b _ fixme = codelab
-- maybe _ f fixme = codelab



-- #####################################################################
-- PARTING WORDS
--
-- Have you noticed that we pattern match with Maybe quite like we do
-- with lists? You haven't seen Either yet, but spoilers: the pattern
-- matching looks quite the same.
--
-- Could we, therefore, define an equivalent of map for Maybe? For Either?
-- But how could we write a function with the same name for different
-- types? Will we end up needing some kind of *shivers* interface?
--
-- Stay tuned for Haskell 102! :)
--
-- (If you want more, head to 06_rps for a bonus section!)
