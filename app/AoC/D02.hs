module AoC.D02(d02p1, d02p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import Data.Vector
import Control.Monad.State

import Debug.Trace (traceShow, traceShowId)

-- From: https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case Prelude.dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = Prelude.break p s'

data ParserState = Exec Int | Halted
type Memory = Vector Int
type IntMachineState = (Memory, ParserState)

op :: Int -> Int -> Int -> Maybe (Int)
op 1 a b = Just $ a + b
op 2 a b = Just $ a * b
op _ _ _ = Nothing

runOp :: Int -> IntMachineState -> (ParserState, IntMachineState)
runOp ip (v, _) = case res of
              Just r -> (Exec $ ip + 4, (v // [(dst, r)], Exec $ ip + 4))
              Nothing -> (Halted, (v, Halted))
  where
    res = op (v ! ip) (v ! param1) (v ! param2)
    param1 = v ! (ip + 1)
    param2 = v ! (ip + 2)
    dst = v ! (ip + 3)

runIntMachine :: State IntMachineState Int
runIntMachine = do
  (v, ps) <- get
  case ps of
    Exec i -> do
      state (runOp i)
      runIntMachine
    Halted -> do
      return $ v ! 0

initialManipulation = [(1, 12), (2, 2)]

d02p1 :: [String] -> IO ()
d02p1 lines =
  print $ evalState runIntMachine (initVector, Exec 0)
  where
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ Prelude.concat lines :: [Int]
    initVector = (fromList parsedLines) // initialManipulation
  
d02p2 :: [String] -> IO ()
d02p2 lines = 
  print $ Prelude.map (\((x, y), _) -> (x, y, 100 * x + y)) results
  where
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ Prelude.concat lines :: [Int]
    initVector = (fromList parsedLines)
    numberRange = [0..99]
    pairs = [(x, y) | x <- numberRange, y <- numberRange]
    possibleResults = Prelude.map (\(x, y) -> ((x, y), evalState runIntMachine (initVector // [(1, x), (2, y)], Exec 0))) pairs
    results = Prelude.filter (\x -> 19690720 == snd x) possibleResults
    
