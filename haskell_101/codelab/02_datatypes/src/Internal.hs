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

module Internal where

import Control.Exception (ErrorCall(..), catch)
import Control.Monad (void, when)
import System.Console.ANSI
  ( Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..)
  , hSupportsANSI, setSGR)
import System.Exit (exitFailure)
import System.IO (stdout)
import System.Timeout (timeout)
import Text.Printf (printf)

-- for students: what they need to implement

codelab :: a
codelab = error "SOMETHING IS NOT IMPLEMENTED!"


-- tests, checking

type Test = IO Bool
type Tests = [Test]

check :: [Test] -> IO ()
check tests = do
  failing <- (length . filter not) <$> sequence tests
  when (failing > 0) exitFailure

test :: (Show a, Eq a) => String -> a -> a -> IO Bool
test name expected actual = do
  void $ printf "%-42s" name
  result <- runTest >>= maybe (False <$ onTimeout) return
  putStrLn ""
  return result
  where
    runTest = timeout timeLimit $ catch checkEq $ ifError $ (False <$) . onFailure
    onSuccess   = putTag "OK" kG >> printf " got: %s" (show actual)
    onError     = putTag "KO" kR >> printf " want: %s; got: %s" (show expected) (show actual)
    onFailure s = putTag "!!" kR >> printf " error: %s" s
    onTimeout   = putTag "??" kR >> putStr " (timeout)"
    timeLimit   = 1000000 -- 10^6 microseconds = 1 second
    checkEq
      | expected == actual = True  <$ onSuccess
      | otherwise          = False <$ onError

ifError :: (String -> a) -> ErrorCall -> a
#if __GLASGOW_HASKELL__ < 800
ifError f (ErrorCall s) = f s
#else
ifError f (ErrorCallWithLocation s _) = f s
#endif


-- colors for the terminal

type TermColor = [SGR]

kR = [SetColor Foreground Dull Red]
kG = [SetColor Foreground Dull Green]

putTag :: String -> TermColor -> IO ()
putTag tag color = do
  supportsANSIColors <- hSupportsANSI stdout
  if supportsANSIColors
  then putColoredTag tag color
  else putSimpleTag tag

putColoredTag :: String -> TermColor -> IO ()
putColoredTag s sgrColor = setSGR sgrColor >> putStr s >> setSGR []

putSimpleTag :: String -> IO ()
putSimpleTag = putStr
