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

module ErrorOrSolution where

import ColorSolution
import Internal (codelab)


-- SECTION 3: ErrorOr (handling errors in pure code)
--
-- We know about Maybe. It is used to represent an optional value. But
-- sometimes, when a computation fails, we want to have more information than
-- just a "Nothing" value.
--
-- For those purposes, we'll introduce here the type "ErrorOr". The error it may
-- or may not contain is simply a String.
--
-- Interestingly, it is a Monad! We'll implement the Monad typeclass for it.
--
-- TO GO FURTHER: there is a built-in type that does exactly the same thing: it
-- is Either a b; though there is no need to know about it for this codelab, you
-- can read about it here:
-- https://hackage.haskell.org/package/base/docs/Data-Either.html

-- An error message is just a String.
type ErrorMsg = String

-- ErrorOr has two constructors; a value of type ErrorOr a is either an error
-- message or a wrapped value.
data ErrorOr a
  = Error ErrorMsg -- an error with a message
  | Value a        -- a wrapped value of type a
  deriving (Show, Eq)

-- "wrapValue" takes a value, and puts it in the context of an "ErrorOr a".
wrapValue :: a -> ErrorOr a
wrapValue = Value

-- "fmapValue" takes a function, and tries to apply it on the value inside the
-- "ErrorOr a". If it cannot apply the function because the "ErrorOr a" contains
-- an error, it simply returns this existing error. We do a simple pattern match
-- to decide what to do.
fmapValue :: (a -> b) -> ErrorOr a -> ErrorOr b
fmapValue _ (Error msg) = Error msg
fmapValue f (Value   x) = Value $ f x

-- "apValue" is the version of "ap" for our "ErrorOr" type. The first value is
-- an "ErrorOr (a -> b)": if we indeed have a function in it, we can apply it on
-- the second argument; if we don't, we simply keep the error. To apply the
-- function, we will need a way to apply a function on a contextual value...
apValue :: ErrorOr (a -> b) -> ErrorOr a -> ErrorOr b
apValue (Error msg) _   = Error msg
apValue (Value   f) eoa = fmapValue f eoa

-- Finally, "bindValue" is our version of "bind". It works exactly like
-- "fmapValue", except we don't have to wrap the result.
bindValue :: (a -> ErrorOr b) -> ErrorOr a -> ErrorOr b
bindValue _ (Error msg) = Error msg
bindValue f (Value   x) = f x

-- Using the functions declared in the Codelab, we can now write the instances
-- of our three beloved typeclasses for ErrorOr.
--
-- The functions that we need for the instances are:
--
--     fmap   :: (a -> b) -> ErrorOr a -> ErrorOr b
--
--     pure   :: a -> ErrorOr a
--     (<*>)  :: ErrorOr (a -> b) -> ErrorOr a -> ErrorOr b
--
--     return :: a -> ErrorOr a
--     (>>=)  :: ErrorOr a -> (a -> ErrorOr b) -> ErrorOr b
--
-- We need both pure and return for historical reasons, even if they do the same
-- thing.
instance Functor ErrorOr where
  fmap = fmapValue

instance Applicative ErrorOr where
  pure  = wrapValue
  (<*>) = apValue

instance Monad ErrorOr where
  return = wrapValue
  (>>=)  = flip bindValue

-- Having ErrorOr, we can define a safe function to convert a Char to a Color.
readColor :: Char -> ErrorOr Color
readColor 'R' = Value Red
readColor 'Y' = Value Yellow
readColor 'G' = Value Green
readColor 'C' = Value Cyan
readColor 'B' = Value Blue
readColor 'M' = Value Magenta
readColor  c  = Error $ '\'' : c : "' is not a proper color."
