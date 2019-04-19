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



{-# LANGUAGE CPP, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Exception
import Control.Monad
import Data.Function (on)
import Data.List (intercalate, nub)
import Data.Map  (empty, fromList)
import System.Console.GetOpt (ArgDescr(ReqArg, NoArg)
                             , ArgOrder(Permute), OptDescr(Option)
                             , getOpt, usageInfo
                             )
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import System.Timeout
import Text.Printf

import Game


-- term color

type TermColor = String

kR = "\x1b[31;1m"
kG = "\x1b[32;1m"

putTag :: String -> TermColor -> IO ()
putTag = hPutTag stdout

hPutTag :: Handle -> String -> TermColor -> IO ()
hPutTag h tag color = do
  isTTY <- queryTerminal stdOutput
  if isTTY
    then hPutStr h $ "[" ++ color ++ tag ++ "\x1b[0m]"
    else hPutStr h $ "[" ++ tag ++ "]"


-- options

newtype Section = Section Int
  deriving Eq

instance Bounded Section where
  minBound = Section 1
  maxBound = Section 5

instance Ord Section where
  compare (Section s1) (Section s2) = compare s1 s2

instance Enum Section where
  toEnum = Section
  fromEnum (Section i) = i

validSection :: Section -> Bool
validSection s = s >= minBound && s <= maxBound

newtype Options = Options
    { optSections :: [Section]
    }

options :: [OptDescr (Options -> IO Options)]
options =
  [
    Option ['s'] ["section"]
      (ReqArg (\v opts ->
                 do let i = read v
                    unless (validSection (Section i)) $ do
                      hPutTag stderr "Error" kR
                      hPutStrLn stderr $ " Invalid section number: " ++ v
                      let (Section from, Section to) = (minBound, maxBound)
                      hPrintf stderr "Valid section numbers: %d..%d\n" from to
                      exitFailure
                    return $ opts { optSections = optSections opts ++ [Section i] }
              )
        "<id>"
      )
      "Include section <id> in the run",
    Option ['h'] ["help"]
      (NoArg (\_ ->
                do hPutStrLn stderr (usageInfo usageHeader options)
                   exitSuccess
             ))
      "Show this help message"
  ]

usageHeader :: String
usageHeader = "Usage: ./test_codelab [-h] [-s|--section <id>] [--help]"


-- tests

ifError :: (String -> a) -> ErrorCall -> a
#if __GLASGOW_HASKELL__ < 800
ifError f (ErrorCall s) = f s
#else
ifError f (ErrorCallWithLocation s _) = f s
#endif

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
  where onSuccess   = putTag "OK" kG >> printf " got: %s" (show actual)
        onError     = putTag "KO" kR >> printf " want: %s; got: %s" (show expected) (show actual)
        onFailure s = putTag "!!" kR >> printf " error: %s" s
        onTimeout   = putTag "??" kR >> putStr " (timeout)"





{- #####################################################################
   SECTION 1: Color
-}

t11_1 = test "[1.1] allColors contains Red"     True $ elem Red     allColors
t11_2 = test "[1.1] allColors contains Yellow"  True $ elem Yellow  allColors
t11_3 = test "[1.1] allColors contains Green"   True $ elem Green   allColors
t11_4 = test "[1.1] allColors contains Cyan"    True $ elem Cyan    allColors
t11_5 = test "[1.1] allColors contains Blue"    True $ elem Blue    allColors
t11_6 = test "[1.1] allColors contains Magenta" True $ elem Magenta allColors
t11_7 = test "[1.1] allColors size is 6"        6 $ length allColors





{- #####################################################################
   SECTION 2: ColorMaps
-}

s2m c x = fromList [(c, x)]

t21_1 = test "[2.1] getIntOr0 (Just 42)" 42 $ getIntOr0 (Just 42)
t21_2 = test "[2.1] getIntOr0 Nothing"    0 $ getIntOr0 Nothing

t22_1 = test "[2.2] getCount on empty map" 0 $ getCount Cyan empty
t22_2 = test "[2.2] getCount on map"       2 $ getCount Cyan $ s2m Cyan 2

t23_1 = test "[2.3] add color in empty map" (s2m Blue 1) $ addColorToMap Blue empty
t23_2 = test "[2.3] add color in map"       (s2m Blue 3) $ addColorToMap Blue $ s2m Blue 2






{- #####################################################################
   SECTION 3: ErrorOr
-}

type S3F = ErrorOr (Int -> String)

s3f :: Int -> ErrorOr Int
s3f x | even x    = Value x
      | otherwise = Error "ODD X"

s3x = 42       :: Int
s3v = Value 42 :: ErrorOr Int

t31_1 = test "[3.1] wrapValue on Int"    (Value 42)    $ wrapValue s3x
t31_2 = test "[3.1] wrapValue on String" (Value "foo") $ wrapValue "foo"

t32_1 = test "[3.2] fmapValue show   on Int"    (Value $ show s3x)     $ fmapValue show   s3v
t32_2 = test "[3.2] fmapValue length on String" (Value $ length "foo") $ fmapValue length (Value "foo")
t32_3 = test "[3.2] fmapValue show   on Error"  (Error "OH NOES")      $ fmapValue id     (Error "OH NOES" :: ErrorOr String)

t33_1 = test "[3.3] apValue function on value" (Value "42")      $ apValue (Value show      :: S3F) s3v
t33_2 = test "[3.3] apValue function on error" (Error "WAT")     $ apValue (Value show      :: S3F) (Error "WAT")
t33_3 = test "[3.3] apValue error    on value" (Error "OH NOES") $ apValue (Error "OH NOES" :: S3F) s3v
t33_4 = test "[3.3] apValue error    on error" (Error "OH NOES") $ apValue (Error "OH NOES" :: S3F) (Error "WAT")

t34_1 = test "[3.4] bindValue on good Int" (Value 42)        $ bindValue s3f (Value 42)
t34_2 = test "[3.4] bindValue on bad  Int" (Error "ODD X")   $ bindValue s3f (Value 21)
t34_3 = test "[3.4] bindValue on Error"    (Error "OH NOES") $ bindValue s3f (Error "OH NOES")





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
   SECTION 5: "do" notation
-}

t51_1 = test "[5.1] # codes of size 0: 1"       1  $ length $ allCodesDo 0
t51_2 = test "[5.1] # codes of size 1: 6"       6  $ length $ allCodesDo 1
t51_3 = test "[5.1] # codes of size 4: 1296" 1296  $ length $ allCodesDo 4
t51_4 = test "[5.1] all codes 0 have size 0"  [0]  $ nub $ length <$> allCodesDo 0
t51_5 = test "[5.1] all codes 1 have size 1"  [1]  $ nub $ length <$> allCodesDo 1
t51_6 = test "[5.1] all codes 4 have size 4"  [4]  $ nub $ length <$> allCodesDo 4
t51_7 = test "[5.1] no duplicated codes"      True $ on (==) length (allCodesDo 4) (nub $ allCodesDo 4)

t52_1 = test "[5.2] len: 0: []"                []  $ duplicatesList 0
t52_2 = test "[5.2] len: 3: [1, 1, 2, 2, 3, 3]" [1, 1, 2, 2, 3, 3] $ duplicatesList 3

t53_1 = test "[5.3] len: 0: []"                       []                       $ oddlyDuplicateList 0
t53_2 = test "[5.3] len: 3: [1, 1, 2, 3, 3]"          [1, 1, 2, 3, 3]          $ oddlyDuplicateList 3
t53_3 = test "[5.3] len: 5: [1, 1, 2, 3, 3, 4, 5, 5]" [1, 1, 2, 3, 3, 4, 5, 5] $ oddlyDuplicateList 5





{- #####################################################################
   Main
-}

tests :: [Section] -> [IO Bool]
tests sections =
  let display s = True <$ putStrLn s
  in intercalate [display ""]
     $ flip map sections
     $ \case Section 1 ->
               [ display "#### Section 1"
               , t11_1, t11_2, t11_3, t11_4, t11_5, t11_6, t11_7
               ]
             Section 2 ->
               [ display "#### Section 2"
               , t21_1, t21_2
               , t22_1, t22_2
               , t23_1, t23_2
               ]
             Section 3 ->
               [ display "#### Section 3"
               , t31_1, t31_2
               , t32_1, t32_2, t32_3
               , t33_1, t33_2, t33_3, t33_4
               , t34_1, t34_2, t34_3
               ]
             Section 4 ->
               [ display "#### Section 4"
               , t41_1, t41_2, t41_3, t41_4, t41_5, t41_6, t41_7
               , t42_1, t42_2
               , t43_1, t43_2, t43_3, t43_4
               , t44_1, t44_2, t44_3, t44_4, t44_5
               , t45_1, t45_2, t45_3
               ]
             Section 5 ->
               [ display "#### Section 5"
               , t51_1, t51_2, t51_3, t51_4, t51_5, t51_6, t51_7
               , t52_1, t52_2
               , t53_1, t53_2, t53_3
               ]
             Section unexpected ->
               [ display $ "Unexpected section requested: " ++ show unexpected
               ]

parseOpts :: IO Options
parseOpts = do
  args <- getArgs
  case getOpt Permute options args of
    (o, [], []) -> do
      let defaultOptions = Options { optSections = [] }
      opts <- foldM (flip id) defaultOptions o
      let opts' = opts { optSections = if null $ optSections opts
                                       then [minBound .. maxBound]
                                       else optSections opts }
      return opts'
    (_, nonOptions@(_:_), _) -> do
      hPrintf stderr " Unexpected non-option argument(s): %s\n"
        (intercalate ", " $ map show nonOptions)
      hPutStrLn stderr $ usageInfo usageHeader options
      exitFailure
    (_, _, errs@(_:_)) -> do
      hPutStrLn stderr (concat errs ++ usageInfo usageHeader options)
      exitFailure

main :: IO ()
main = do
  opts <- parseOpts
  results <- sequence $ tests (optSections opts)
  let failing = length . filter not $ results
  when (failing > 0) exitFailure
