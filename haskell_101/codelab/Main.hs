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
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Control.Exception
import           Control.Monad
import           System.Exit
import           System.Posix.IO       (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import           System.Timeout
import           Text.Printf

#ifdef CODELAB
import qualified Codelab               as C
#endif

#ifdef SOLUTION
import qualified Solution              as C
#endif

(+=+) :: [a] -> [a] -> [a]
(+=+) = (C.++) -- lol C++



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

testI :: String -> Int -> Int -> IO Bool
testI = test


-- main

display s = True <$ putStrLn s

tests = [ display "#### Section 1"
        , test  "null []"      True  $ C.null []
        , test  "null [8,0,6]" False $ C.null [8,0,6]
        , testI "head [8,0,6]" 8     $ C.head [8,0,6]
        , test  "tail [8,0,6]" [0,6] $ C.tail [8,0,6]
        , display ""
        , display "#### Section 2"
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
        , test  "[8,0] ++ [   ]"              [8,0]     $ [8,0] +=+ []
        , test  "[   ] ++ [6,4]"              [6,4]     $ [   ] +=+ [6,4]
        , test  "[8,0] ++ [6,4]"              [8,0,6,4] $ [8,0] +=+ [6,4]
        , display ""
        , display "#### Section 3"
        , test  "map    (+1)   []"        []                     $ C.map (+1) []
        , test  "map    (+1)   [8,0,6,4]" [9,1,7,5]              $ C.map (+1) [8,0,6,4]
        , test  "filter (>5)   []"        []                     $ C.filter (>5) []
        , test  "filter (>5)   [8,0,6,4]" [8,6]                  $ C.filter (>5) [8,0,6,4]
        , testI "foldl  (-)  1   [10]" (-9)                      $ C.foldl (-) 1 [10]
        , testI "foldr  (-)  1   [10]" ( 9)                      $ C.foldr (-) 1 [10]
        , testI "foldl  (-)  0   [1,2,3,4]" (-10)                $ C.foldl (-) 0 [1,2,3,4]
        , testI "foldr  (-)  0   [1,2,3,4]" (-2)                 $ C.foldr (-) 0 [1,2,3,4]
        , test  "foldl  (++) \"_\" [\"A\", \"B\", \"C\"]" "_ABC" $ C.foldl  (++) "_" ["A","B","C"]
        , test  "foldr  (++) \"_\" [\"A\", \"B\", \"C\"]" "ABC_" $ C.foldr  (++) "_" ["A","B","C"]
        , display ""
        , display "#### Section 4"
        , test  "safeHead  []"           Nothing  $ C.safeHead ([] :: [Int])
        , test  "safeHead  [8,0,6]"      (Just 8) $ C.safeHead [8,0,6]
        , test  "isNothing (Just 42)"    False    $ C.isNothing (Just 42)
        , test  "isNothing Nothing"      True     $ C.isNothing Nothing
        , testI "fromMaybe 0 (Just 40)"  40       $ C.fromMaybe 0 (Just 40)
        , testI "fromMaybe 0 Nothing"    0        $ C.fromMaybe 0 Nothing
        , testI "maybe 0 (+2) (Just 40)" 42       $ C.maybe 0 (+2) (Just 40)
        , testI "maybe 0 (+2) Nothing"   0        $ C.maybe 0 (+2) Nothing
        ]

main :: IO ()
main = do
  failing <- length . filter not <$> sequence tests
  when (failing > 0) exitFailure
