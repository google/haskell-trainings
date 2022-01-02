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

module Tests (check) where

import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub)
import qualified Data.Map as Map
import Text.Printf (printf)

#ifdef SOLUTION
import CodeSolution
import ColorSolution
import ColorMapSolution
import DoSolution
import ErrorOrSolution
#else
import Code
import Color
import ColorMap
import Do
import ErrorOr
#endif

import Internal (test, Tests)
import qualified Internal (check)

-- Check one section of the codelab
check :: Int -> IO ()
check x = case IntMap.lookup x testMap of
  Just (file, tests) -> doCheck file tests
  _                  -> noSuchSection

-- When we check a code section, we print the file under test and then the
-- results of all tests from there
doCheck :: String -> Tests -> IO ()
doCheck file tests = do
  putStr "\n\nChecking code from " >> putStrLn file
  Internal.check tests

-- If user supplies invalid arguments, we should print the valid arguments and
-- the files under test
noSuchSection :: IO ()
noSuchSection = do
  putStrLn "\n\nRequested invalid section. Available sections are:"
  mapM_ displaySection $ IntMap.toAscList testMap
  where
    displaySection (k, (f, _)) = printf "\t%d -> %s\n" k f

-- We record a mapping section -> (file, tests)
testMap :: IntMap (String, Tests)
testMap = IntMap.fromList
  [ (1, ("src/Color.hs",    colorTests))
  , (2, ("src/ColorMap.hs", colorMapTests))
  , (3, ("src/ErrorOr.hs",  errorOrTests))
  , (4, ("src/Code.hs",     codeTests))
  , (5, ("src/Do.hs",       doTests))
  ]

colorTests :: Tests
colorTests =
  [ test "allColors contains Red"      True     $ elem Red     allColors
  , test "allColors contains Yellow"   True     $ elem Yellow  allColors
  , test "allColors contains Green"    True     $ elem Green   allColors
  , test "allColors contains Cyan"     True     $ elem Cyan    allColors
  , test "allColors contains Blue"     True     $ elem Blue    allColors
  , test "allColors contains Magenta"  True     $ elem Magenta allColors
  , test "allColors size is 6"         6        $ length allColors
  , test "show Red"                    "R"      $ show Red
  , test "concatMap show allColors"    "RYGCBM" $ concatMap show allColors
  , test "allColors starts with Red"   Red      $ head allColors
  , test "allColors ends with Magenta" Magenta  $ last allColors
  ]

colorMapTests :: Tests
colorMapTests =
  [ test "getIntOr0 (Just 42)"    42          $ getIntOr0 (Just 42)
  , test "getIntOr0 Nothing"      0           $ getIntOr0 Nothing
  , test "getCount on empty map"  0           $ getCount Cyan Map.empty
  , test "getCount on map"        2           $ getCount Cyan (mk Cyan 2)
  , test "add color in empty map" (mk Blue 1) $ addColorToMap Blue Map.empty
  , test "add color in map"       (mk Blue 3) $ addColorToMap Blue (mk Blue 2)
  ]
  where
    mk c x = Map.fromList [(c, x)]

errorOrTests :: Tests
errorOrTests =
  [ test "wrapValue on Int"           (Value 42)                           $ wrapValue 42
  , test "wrapValue on String"        (Value "foo")                        $ wrapValue "foo"
  , test "fmapValue show   on Int"    (Value $ show 42)                    $ fmapValue show   (Value 42)
  , test "fmapValue length on String" (Value $ length "foo")               $ fmapValue length (Value "foo")
  , test "fmapValue show   on Error"  (Error "OH NOES")                    $ fmapValue id     (Error "OH NOES" :: ErrorOr String)
  , test "apValue function on value"  (Value "42")                         $ apValue (Value show      :: ErrorOr (Int -> String)) (Value 42)
  , test "apValue function on error"  (Error "WAT")                        $ apValue (Value show      :: ErrorOr (Int -> String)) (Error "WAT")
  , test "apValue error    on value"  (Error "OH NOES")                    $ apValue (Error "OH NOES" :: ErrorOr (Int -> String)) (Value 42)
  , test "apValue error    on error"  (Error "OH NOES")                    $ apValue (Error "OH NOES" :: ErrorOr (Int -> String)) (Error "WAT")
  , test "bindValue on good Int"      (Value 42)                           $ bindValue fun (Value 42)
  , test "bindValue on bad  Int"      (Error "ODD X")                      $ bindValue fun (Value 21)
  , test "bindValue on Error"         (Error "OH NOES")                    $ bindValue fun (Error "OH NOES")
  , test "readColor 'R'"              (Value Red)                          $ readColor 'R'
  , test "readColor 'Y'"              (Value Yellow)                       $ readColor 'Y'
  , test "readColor 'G'"              (Value Green)                        $ readColor 'G'
  , test "readColor 'C'"              (Value Cyan)                         $ readColor 'C'
  , test "readColor 'B'"              (Value Blue)                         $ readColor 'B'
  , test "readColor 'M'"              (Value Magenta)                      $ readColor 'M'
  , test "readColor 'Z'"              (Error "'Z' is not a proper color.") $ readColor 'Z'
  ]
  where
    fun x = if even x then Value x else Error "ODD X"

codeTests :: Tests
codeTests =
  [ test "# codes of size 0: 1"       1                                 $ length $ allCodes 0
  , test "# codes of size 1: 6"       6                                 $ length $ allCodes 1
  , test "# codes of size 4: 1296" 1296                                 $ length $ allCodes 4
  , test "all codes 0 have size 0"  [0]                                 $ nub $ length <$> allCodes 0
  , test "all codes 1 have size 1"  [1]                                 $ nub $ length <$> allCodes 1
  , test "all codes 4 have size 4"  [4]                                 $ nub $ length <$> allCodes 4
  , test "no duplicated codes"      True                                $ on (==) length (allCodes 4) (nub $ allCodes 4)
  , test "empty code -> empty map" Map.empty                            $ codeToMap []
  , test "[C,R,C] -> {R: 1, C: 2}" (Map.fromList [(Red, 1), (Cyan, 2)]) $ codeToMap [Cyan, Red, Cyan]
  , test "countBlacks [R,Y,G,B] [B,R,Y,G]" 0                            $ countBlacks [Red, Yellow, Green, Blue] [Blue, Red,  Yellow, Green]
  , test "countBlacks [R,Y,G,B] [R,B,G,Y]" 2                            $ countBlacks [Red, Yellow, Green, Blue] [Red,  Blue, Green,  Yellow]
  , test "countBlacks [B,B,C,G] [Y,B,G,C]" 1                            $ countBlacks [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
  , test "countBlacks [B,B,C,G] [B,B,C,G]" 4                            $ countBlacks [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]
  , test "countTotal  [C,R,B,M] [Y,R,G,G]" 1                            $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Red,    Green, Green]
  , test "countTotal  [C,R,B,M] [Y,Y,C,M]" 2                            $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Yellow, Cyan,  Magenta]
  , test "countTotal  [C,R,B,M] [Y,R,C,M]" 3                            $ countTotal [Cyan, Red, Blue, Magenta] [Yellow, Red,    Cyan,  Magenta]
  , test "countTotal  [B,B,C,G] [Y,B,G,C]" 3                            $ countTotal [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
  , test "countTotal  [B,B,C,G] [B,B,C,G]" 4                            $ countTotal [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]
  , test "countScore  [B,B,C,G] [R,R,R,R]" (Score 0 0)                  $ countScore [Blue, Blue, Cyan, Green] [Red,    Red,  Red,   Red]
  , test "countScore  [B,B,C,G] [Y,B,G,C]" (Score 1 2)                  $ countScore [Blue, Blue, Cyan, Green] [Yellow, Blue, Green, Cyan]
  , test "countScore  [B,B,C,G] [B,B,C,G]" (Score 4 0)                  $ countScore [Blue, Blue, Cyan, Green] [Blue,   Blue, Cyan,  Green]
  ]

doTests :: Tests
doTests =
  [ test "# codes of size 0: 1"             1                        $ length $ allCodesDo 0
  , test "# codes of size 1: 6"             6                        $ length $ allCodesDo 1
  , test "# codes of size 4: 1296"          1296                     $ length $ allCodesDo 4
  , test "all codes 0 have size 0"          [0]                      $ nub $ length <$> allCodesDo 0
  , test "all codes 1 have size 1"          [1]                      $ nub $ length <$> allCodesDo 1
  , test "all codes 4 have size 4"          [4]                      $ nub $ length <$> allCodesDo 4
  , test "no duplicated codes"              True                     $ on (==) length (allCodesDo 4) (nub $ allCodesDo 4)
  , test "len: 0: []"                       []                       $ duplicatesList 0
  , test "len: 3: [1, 1, 2, 2, 3, 3]"       [1, 1, 2, 2, 3, 3]       $ duplicatesList 3
  , test "len: 0: []"                       []                       $ oddlyDuplicateList 0
  , test "len: 3: [1, 1, 2, 3, 3]"          [1, 1, 2, 3, 3]          $ oddlyDuplicateList 3
  , test "len: 5: [1, 1, 2, 3, 3, 4, 5, 5]" [1, 1, 2, 3, 3, 4, 5, 5] $ oddlyDuplicateList 5
  ]
