-- Copyright 2019 Google LLC
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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Exception
import Control.Monad
import Data.Function         (on)
import Data.List             (nub)
import Data.Map              (empty, fromList)
import System.Exit
import System.Posix.IO       (stdOutput)
import System.Posix.Terminal (queryTerminal)
import System.Timeout
import Text.Printf

import Game





{- #####################################################################
   SECTION 0: setting up the tests.
-}


-- term color

type TermColor = String

kR = "\x1b[31;1m"
kG = "\x1b[32;1m"

putTag :: String -> TermColor -> IO ()
putTag tag color = do
  isTTY <- queryTerminal stdOutput
  if isTTY
    then putStr $ "[" ++ color ++ tag ++ "\x1b[0m]"
    else putStr $ "[" ++ tag ++ "]"


-- tests

ifError :: (String -> a) -> ErrorCall -> a
ifError f (ErrorCallWithLocation s _) = f s

timeLimit :: Int
timeLimit = 1000000 -- 10^6 microseconds = 1 second

test :: (Show a, Eq a) => String -> a -> a -> IO Bool
test name expected actual = do
  let checkEq = if expected == actual
                then True  <$ onSuccess
                else False <$ onError
      runTest = timeout timeLimit $ catch checkEq $ ifError $ (False <$) . onFailure
  void $ printf "%-42s" name
  result <- runTest >>= maybe (False <$ onTimeout) return
  putStrLn ""
  return result
  where onSuccess   = putTag "OK" kG
        onError     = putTag "KO" kR >> printf " want: %s; got: %s" (show expected) (show actual)
        onFailure s = putTag "!!" kR >> printf " error: %s" s
        onTimeout   = putTag "??" kR >> putStr " (timeout)"





{- #####################################################################
   SECTION 1: ErrorOr
-}

type S1F = ErrorOr (Int -> String)

s1f :: Int -> ErrorOr Int
s1f x | even x    = Value x
      | otherwise = Error "ODD X"

s1x = 42       :: Int
s1v = Value 42 :: ErrorOr Int

t11_1 = test "[1.1] wrapValue on Int"    (Value 42)    $ wrapValue s1x
t11_2 = test "[1.1] wrapValue on String" (Value "foo") $ wrapValue "foo"

t12_1 = test "[1.2] fmapValue show   on Int"    (Value $ show s1x)     $ fmapValue show   s1v
t12_2 = test "[1.2] fmapValue length on String" (Value $ length "foo") $ fmapValue length (Value "foo")
t12_3 = test "[1.2] fmapValue show   on Error"  (Error "OH NOES")      $ fmapValue id     (Error "OH NOES" :: ErrorOr String)

t13_1 = test "[1.3] apValue function on value" (Value "42")      $ apValue (Value show      :: S1F) s1v
t13_2 = test "[1.3] apValue function on error" (Error "WAT")     $ apValue (Value show      :: S1F) (Error "WAT")
t13_3 = test "[1.3] apValue error    on value" (Error "OH NOES") $ apValue (Error "OH NOES" :: S1F) s1v
t13_4 = test "[1.3] apValue error    on error" (Error "OH NOES") $ apValue (Error "OH NOES" :: S1F) (Error "WAT")

t14_1 = test "[1.4] bindValue on good Int" (Value 42)        $ bindValue s1f (Value 42)
t14_2 = test "[1.4] bindValue on bad  Int" (Error "ODD X")   $ bindValue s1f (Value 21)
t14_3 = test "[1.4] bindValue on Error"    (Error "OH NOES") $ bindValue s1f (Error "OH NOES")





{- #####################################################################
   SECTION 2: Color
-}

t21_1 = test "[2.1] allColors contains Red"     True $ elem Red     allColors
t21_2 = test "[2.1] allColors contains Yellow"  True $ elem Yellow  allColors
t21_3 = test "[2.1] allColors contains Green"   True $ elem Green   allColors
t21_4 = test "[2.1] allColors contains Cyan"    True $ elem Cyan    allColors
t21_5 = test "[2.1] allColors contains Blue"    True $ elem Blue    allColors
t21_6 = test "[2.1] allColors contains Magenta" True $ elem Magenta allColors
t21_7 = test "[2.1] allColors size is 6"        6 $ length allColors





{- #####################################################################
   SECTION 3: ColorMaps
-}

s3m c x = fromList [(c, x)]

t31_1 = test "[3.1] getIntOr0 (Just 42)" 42 $ getIntOr0 (Just 42)
t31_2 = test "[3.1] getIntOr0 Nothing"    0 $ getIntOr0 Nothing

t32_1 = test "[3.2] getCount on empty map" 0 $ getCount Cyan empty
t32_2 = test "[3.2] getCount on map"       2 $ getCount Cyan $ s3m Cyan 2

t33_1 = test "[3.3] add color in empty map" (s3m Blue 1) $ addColorToMap Blue empty
t33_2 = test "[3.3] add color in map"       (s3m Blue 3) $ addColorToMap Blue $ s3m Blue 2





{- #####################################################################
   SECTION 4: codes
-}

t41_1 = test "[4.1] # codes of size 0: 1"       1  $ length $ allCodes 0
t41_2 = test "[4.1] # codes of size 1: 6"       6  $ length $ allCodes 1
t41_3 = test "[4.1] # codes of size 4: 1296" 1296  $ length $ allCodes 4
t41_4 = test "[4.1] all codes 0 have size 0"  [0]  $ nub $ length <$> allCodes 0
t41_5 = test "[4.1] all codes 1 have size 1"  [1]  $ nub $ length <$> allCodes 1
t41_6 = test "[4.1] all codes 4 have size 4"  [4]  $ nub $ length <$> allCodes 4
t41_7 = test "[4.1] no duplicated codes"      True $ on (==) length (allCodes 4) (nub $ allCodes 4)

t42_1 = test "[4.2] empty code -> empty map" empty $ codeToMap []
t42_2 = test "[4.2] [C,R,C] -> {R: 1, C: 2}" (fromList [(Red, 1), (Cyan, 2)]) $ codeToMap [Cyan, Red, Cyan]

t43_1 = test "[4.3] countBlacks [R,Y,G,B] [B,R,Y,G]" 0 $ countBlacks [Red, Yellow, Green, Blue] [Blue, Red,  Yellow, Green]
t43_2 = test "[4.3] countBlacks [R,Y,G,B] [R,B,G,Y]" 2 $ countBlacks [Red, Yellow, Green, Blue] [Red,  Blue, Green,  Yellow]
t43_3 = test "[4.3] countBlacks [B,B,C,G] [Y,B,G,C]" 1 $ countBlacks [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
t43_4 = test "[4.3] countBlacks [B,B,C,G] [B,B,C,G]" 4 $ countBlacks [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]

t44_1 = test "[4.4] countTotal  [C,R,B,M] [Y,R,G,G]" 1 $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Red,    Green, Green]
t44_2 = test "[4.4] countTotal  [C,R,B,M] [Y,Y,C,M]" 2 $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Yellow, Cyan,  Magenta]
t44_3 = test "[4.4] countTotal  [C,R,B,M] [Y,R,C,M]" 3 $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Red,    Cyan,  Magenta]
t44_4 = test "[4.4] countTotal  [B,B,C,G] [Y,B,G,C]" 3 $ countTotal [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
t44_5 = test "[4.4] countTotal  [B,B,C,G] [B,B,C,G]" 4 $ countTotal [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]

t45_1 = test "[4.5] countScore  [B,B,C,G] [R,R,R,R]" (Score 0 0) $ countScore [Blue, Blue, Cyan, Green] [Red,    Red,  Red,   Red]
t45_2 = test "[4.5] countScore  [B,B,C,G] [Y,B,G,C]" (Score 1 2) $ countScore [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
t45_3 = test "[4.5] countScore  [B,B,C,G] [B,B,C,G]" (Score 4 0) $ countScore [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]





{- #####################################################################
   Main
-}

display s = True <$ putStrLn s

tests = [display "#### Section 1",
         t11_1, t11_2,
         t12_1, t12_2, t12_3,
         t13_1, t13_2, t13_3, t13_4,
         t14_1, t14_2, t14_3,
         display "",
         display "#### Section 2",
         t21_1, t21_2, t21_3, t21_4, t21_5, t21_6, t21_7,
         display "",
         display "#### Section 3",
         t31_1, t31_2,
         t32_1, t32_2,
         t33_1, t33_2,
         display "",
         display "#### Section 4",
         t41_1, t41_2, t41_3, t41_4, t41_5, t41_6, t41_7,
         t42_1, t42_2,
         t43_1, t43_2, t43_3, t43_4,
         t44_1, t44_2, t44_3, t44_4, t44_5,
         t45_1, t45_2, t45_3]

main :: IO ()
main = do
  failing <- length . filter not <$> sequence tests
  when (failing > 0) exitFailure
