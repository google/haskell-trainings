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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game (startGameAsHuman, startGameAsAI) where

import System.IO
import System.Random

#ifdef SOLUTION
import CodeSolution
import ColorSolution
import ErrorOrSolution
#else
import Code
import Color
import ErrorOr
#endif

-- We only export the startGame function and the ways to play: human or AI.
startGame :: Input -> IO ()
startGame = play (randomCode kSeed kCodeSize) kMaxTurns

startGameAsHuman = startGame humanInput
startGameAsAI    = startGame computerInput


-- Here, sequence transforms a list of monadic values into a monadic list of
-- values: it moves the context towards the outside.
-- Here on our type it does: [ErrorOr Color] -> ErrorOr [Color]
readCode :: String -> ErrorOr Code
readCode = sequence . map readColor


-- Let's build an automatic solver! We need to keep track of the game we're
-- playing, so we introduce a "History" type which records our guesses and their
-- score.

type History = [(Code, Score)]

check :: History -> Code -> Bool
check hist candidate = all consistentWithCandidate hist
  where
    consistentWithCandidate (code, score) = countScore code candidate == score

-- naiveSolver codeSize history = head $ filter (check history) $ allColors codeSize
naiveSolver :: Int -> History -> Code
naiveSolver codeSize history =
  let codes     = allCodes codeSize            -- we generate all possible codes of that size
      goodCodes = filter (check history) codes -- we keep only the ones that match our history
  in head goodCodes                            -- we simply pick the first one


-- This is where we link everything together.
-- We define an Input function type, which we use to abstract the player.
--   * humanInput uses getCode to read a code from stdin
--   * computerInput uses naiveSolver to guess the next code

validateCode :: Int -> String -> ErrorOr Code
validateCode size input = do
  code <- readCode input                -- readCode input :: ErrorOr Code
  if length code == size                -- but code :: Code
  then Value code
  else Error $ "Expecting a code of size " ++ show size ++ "."

getCode :: Int -> Int -> IO Code
getCode size turn = do
  putStr $ "Turn " ++ show turn ++ ": " -- print the prompt
  hFlush stdout                         -- flush stdout
  line <- getLine                       -- read the input
  case validateCode size line of        -- we try to read the code
    Error msg  -> do                    -- if it wasn't a proper code
      putStrLn msg                      --     we print the error
      getCode size turn                 --     and we retry
    Value code -> return code           -- otherwise, perfect!


type Input = Int -> Int -> History -> IO Code

humanInput :: Input
humanInput size turn _ = getCode size turn

computerInput :: Input
computerInput size _ hist = return $ naiveSolver size hist


play :: Code -> Int -> Input -> IO ()
play answer maxTurn input = do
  putStrLn "\n"
  putStrLn $ "Valid colors: "       ++ show allColors
  putStrLn $ "Size of the answer: " ++ show (length answer)
  putStrLn $ "Number of tries: "    ++ show maxTurn
  putStrLn $ "Good luck!"
  playTurn 1 []
  where
    playTurn turn history
      | turn > maxTurn = putStrLn $ "Sorry, you lost! The answer was " ++ show answer
      | otherwise = do
          code <- input (length answer) turn history
          if code == answer
          then putStrLn $ show code ++ " => well done!"
          else do
            let score = countScore answer code
            putStrLn $ show code ++ " => " ++ show score
            playTurn (turn + 1) $ (code, score) : history


-- Random code generation.

-- We use a fixed seed, in order to be able to replay the same game over
-- and over again. If you read this, as an exercise you could think about
-- what it would entail to have a seed as a parameter to our program.

-- randomCode drops the first generated element to mitigate the poor
-- implementation of System.Random.

type Seed = Int

instance Random Color where
  random         g = randomR (minBound, maxBound) g
  randomR (a, b) g = let (i, n) = randomR (fromEnum a, fromEnum b) g
                     in (toEnum i, n)

randomCode :: Seed -> Int -> Code
randomCode seed size = take size $ drop 1 $ randoms $ mkStdGen seed


-- Game constants
-- Those are the constants on which our game rely.

kMaxTurns :: Int
kMaxTurns = 8

kCodeSize :: Int
kCodeSize = 4

kSeed :: Seed
kSeed = 756
