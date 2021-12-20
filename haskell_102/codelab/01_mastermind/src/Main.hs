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

module Main where

import Control.Monad (join)
import Options.Applicative

import Tests (check)
import Game (startGameAsAI, startGameAsHuman)

-- Parsing arguments uses these typeclasses:
--   - Functor: (<$>)
--   - Applicative: pure
--   - Monad: join
--   - Monoid: (<>)
--
-- Also, we use several helpers from Options.Applicative.
--
-- You might want to look on Hoogle/Hackage for each of them.
main :: IO ()
main = join . customExecParser preferences $ info modes idm
  where
    -- The modes under which the program is run
    modes = subparser
      (  command "play"  (info (pure startGameAsHuman) playHelp)
      <> command "solve" (info (pure startGameAsAI) solveHelp)
      <> command "check" (info (check <$> argument auto argHelp) checkHelp)
      )
    -- We prefer to display help on error or if no argument is supplied
    preferences = prefs (showHelpOnError <> showHelpOnEmpty)
    -- For "check", we want to display a better help message
    argHelp = metavar "SECTION" <> help "Section to test"
    -- Help messages for every mode
    playHelp  = progDesc "Play using human input"
    solveHelp = progDesc "Play using AI input"
    checkHelp = progDesc "Check one section of codelab"
