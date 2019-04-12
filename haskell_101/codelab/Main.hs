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
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Exception
import Control.Monad
import Data.List (intercalate)
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

#ifdef SOLUTION
import qualified Solution              as C
#else
import qualified Codelab               as C
#endif


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
usageHeader = "Usage: ./main [-h] [-s|--section <id>] [--help]"


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

testI :: String -> Int -> Int -> IO Bool
testI = test


-- main

tests :: [Section] -> [IO Bool]
tests sections =
  let display s = True <$ putStrLn s
  in intercalate [display ""]
     $ flip map sections
     $ \case Section 1 ->
               [ display "#### Section 1"
               , display "TODO"
               ]
             Section 2 ->
               [ display "#### Section 2"
               , test  "null []"      True  $ C.null []
               , test  "null [8,0,6]" False $ C.null [8,0,6]
               , testI "head [8,0,6]" 8     $ C.head [8,0,6]
               , test  "tail [8,0,6]" [0,6] $ C.tail [8,0,6]
               ]
             Section 3 ->
               [ display "#### Section 3"
               , testI "length []"                   0         $ C.length []
               , testI "length [8,0,6]"              3         $ C.length [8,0,6]
               , test  "and    []"                   True      $ C.and []
               , test  "and    [True]"               True      $ C.and [True]
               , test  "and    [False]"              False     $ C.and [False]
               , test  "and    [True,  True]"        True      $ C.and [True,  True]
               , test  "and    [True,  False]"       False     $ C.and [True,  False]
               , test  "and    [False, True]"        False     $ C.and [False, True]
               , test  "and    [False, False]"       False     $ C.and [False, False]
               , test  "and    [True, True, True]"   True      $ C.and [True, True, True]
               , test  "or     []"                   False     $ C.or []
               , test  "or     [True]"               True      $ C.or [True]
               , test  "or     [False]"              False     $ C.or [False]
               , test  "or     [True,  True]"        True      $ C.or [True,  True]
               , test  "or     [True,  False]"       True      $ C.or [True,  False]
               , test  "or     [False, True]"        True      $ C.or [False, True]
               , test  "or     [False, False]"       False     $ C.or [False, False]
               , test  "or     [False, False, True]" True      $ C.or [False, False, True]
               , test  "[8,0] ++ [   ]"              [8,0]     $ [8,0] C.++ []
               , test  "[   ] ++ [6,4]"              [6,4]     $ [   ] C.++ [6,4]
               , test  "[8,0] ++ [6,4]"              [8,0,6,4] $ [8,0] C.++ [6,4]
               ]
             Section 4 ->
               [ display "#### Section 4"
               , test  "map    (+1)   []"        []                     $ C.map (+1) []
               , test  "map    (+1)   [8,0,6,4]" [9,1,7,5]              $ C.map (+1) [8,0,6,4]
               , test  "filter (>5)   []"        []                     $ C.filter (>5) []
               , test  "filter (>5)   [8,0,6,4]" [8,6]                  $ C.filter (>5) [8,0,6,4]
               , testI "foldl  (-)  1   [10]" (-9)                      $ C.foldl (-) 1 [10]
               , testI "foldr  (-)  1   [10]"   9                       $ C.foldr (-) 1 [10]
               , testI "foldl  (-)  0   [1,2,3,4]" (-10)                $ C.foldl (-) 0 [1,2,3,4]
               , testI "foldr  (-)  0   [1,2,3,4]" (-2)                 $ C.foldr (-) 0 [1,2,3,4]
               , test  "foldl  (++) \"_\" [\"A\", \"B\", \"C\"]" "_ABC" $ C.foldl  (++) "_" ["A","B","C"]
               , test  "foldr  (++) \"_\" [\"A\", \"B\", \"C\"]" "ABC_" $ C.foldr  (++) "_" ["A","B","C"]
               ]
             Section 5 ->
               [ display "#### Section 5"
               , test  "safeHead  []"           Nothing  $ C.safeHead ([] :: [Int])
               , test  "safeHead  [8,0,6]"      (Just 8) $ C.safeHead [8,0,6]
               , test  "isNothing (Just 42)"    False    $ C.isNothing (Just 42)
               , test  "isNothing Nothing"      True     $ C.isNothing Nothing
               , testI "fromMaybe 0 (Just 40)"  40       $ C.fromMaybe 0 (Just 40)
               , testI "fromMaybe 0 Nothing"    0        $ C.fromMaybe 0 Nothing
               , testI "maybe 0 (+2) (Just 40)" 42       $ C.maybe 0 (+2) (Just 40)
               , testI "maybe 0 (+2) Nothing"   0        $ C.maybe 0 (+2) Nothing
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
