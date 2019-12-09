module AoC.D09(d09p1, d09p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.Vector (fromList)
import Control.Monad.State

import AoC.IntMachine

d09p1 :: [String] -> IO ()
d09p1 lines =
  print $ evalState (runIntMachine) (initVector, Exec 0, 0, input, [])
  where
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ lines !! 0
    initVector = buildMem parsedLines
    input = [1]
  
d09p2 :: [String] -> IO ()
d09p2 lines =
  print $ evalState (runIntMachine) (initVector, Exec 0, 0, input, [])
  where
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ lines !! 0
    initVector = buildMem parsedLines
    input = [2]
