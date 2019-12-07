module AoC.D05(d05p1, d05p2) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import Data.Vector hiding ((++), map, zip, dropWhile, reverse, take, tail)
import Control.Monad.State

import Debug.Trace (traceShow, traceShowId)

-- From: https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case Prelude.dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = Prelude.break p s'


padL :: a -> Int -> [a] -> [a]
padL p s l
    | Prelude.length l >= s = l
    | otherwise     = Prelude.replicate (s - Prelude.length l) p ++ l

data ParserState = Exec Int | Halted
  deriving (Show)
type Input = [Int]
type Output = [Int]
type Memory = Vector Int
type IntMachineState = (Memory, ParserState, Input, Output)

data OpCode = Add Parameter Parameter Parameter
  | Mul Parameter Parameter Parameter
  | Rd Parameter
  | Wr Parameter
  | JTrue Parameter Parameter
  | JFalse Parameter Parameter
  | CLt Parameter Parameter Parameter
  | CEq Parameter Parameter Parameter
  | Halt
  deriving (Show)

data Parameter = Address Int | Intermediate Int
  deriving (Show)


parseParam :: (Char, Int) -> Parameter
parseParam ('0', x) = Address x
parseParam ('1', x) = Intermediate x

resolveParam :: Memory -> Parameter -> Int
resolveParam _ (Intermediate x) = x
resolveParam mem (Address x) = mem ! x

resolveDst :: Parameter -> Int
resolveDst (Intermediate x) = x
resolveDst (Address x) = x

parseOp :: Memory -> Int -> OpCode
parseOp mem ip
  | opcode == "01" = Add (params !! 0) (params !! 1) (params !! 2)
  | opcode == "02" = Mul (params !! 0) (params !! 1) (params !! 2)
  | opcode == "03" = Rd (params !! 0) 
  | opcode == "04" = Wr (params !! 0)
  | opcode == "05" = JTrue (params !! 0) (params !! 1)
  | opcode == "06" = JFalse (params !! 0) (params !! 1)
  | opcode == "07" = CLt (params !! 0) (params !! 1) (params !! 2)
  | opcode == "08" = CEq (params !! 0) (params !! 1) (params !! 2)
  | otherwise = Halt
  where
    instruction = padL '0' 5 $ show $ mem ! ip
    opcode = Prelude.drop 3 $ instruction
    params = map parseParam $ zip (reverse $ take 3 instruction) $ toList $ Data.Vector.drop (ip + 1) mem


evalOp :: IntMachineState -> OpCode -> IntMachineState
evalOp (mem, Exec ip, i, o) (Add a b (Address dst)) = (mem // [(dst, (resolveParam mem a) + (resolveParam mem b))], Exec (ip + 4), i, o)
evalOp (mem, Exec ip, i, o) (Mul a b (Address dst)) = (mem // [(dst, (resolveParam mem a) * (resolveParam mem b))], Exec (ip + 4), i, o)
evalOp (mem, Exec ip, i, o) (Rd (Address dst))      = (mem // [(dst, i !! 0)], Exec (ip + 2), tail i, o)
evalOp (mem, Exec ip, i, o) (Wr a)                  = (mem, Exec (ip + 2), i, o ++ [resolveParam mem a])

evalOp (mem, Exec ip, i, o) (JTrue a b)             = (mem, Exec newIP, i, o)
  where
    newIP = if 0 /= resolveParam mem a then (resolveParam mem b) else (ip + 3)
    
evalOp (mem, Exec ip, i, o) (JFalse a b)             = (mem, Exec newIP, i, o)
  where
    newIP = if 0 == resolveParam mem a then (resolveParam mem b) else (ip + 3)

evalOp (mem, Exec ip, i, o) (CLt a b c)             = (mem // [(resolveDst c, cmp)], Exec (ip + 4), i, o)
  where
    cmp = if (resolveParam mem a) < (resolveParam mem b) then 1 else 0

evalOp (mem, Exec ip, i, o) (CEq a b c)             = (mem // [(resolveDst c, cmp)], Exec (ip + 4), i, o)
  where
    cmp = if (resolveParam mem a) == (resolveParam mem b) then 1 else 0

evalOp (mem, Exec ip, _, o) (Halt) = (mem, Halted, [], o)
evalOp (mem,  Halted, _, o)      _ = (mem, Halted, [], o)



performOp :: IntMachineState -> IntMachineState
performOp s@(mem, Exec ip, i, o) = evalOp (traceShowId s) $ traceShowId $ parseOp mem ip
performOp s@(mem, Halted, i, o) = evalOp s $ Halt

runIntMachine :: State IntMachineState [Int]
runIntMachine = do
  (mem, ps, i, o) <- get
  case ps of
    Exec i -> do
      state (\x -> ((), performOp x))
      runIntMachine
    Halted -> do
      return $ o
  

d05p1 :: [String] -> IO ()
d05p1 lines =
  print $ evalState (runIntMachine) (initVector, Exec 0, input, [])
  where
    catProgram = "3,0,4,0,99"
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ lines !! 0
    initVector = (fromList parsedLines)
    input = [1]
  
d05p2 :: [String] -> IO ()
d05p2 lines =
  print $ evalState (runIntMachine) (initVector, Exec 0, input, [])
  where
    parsedLines = catMaybes $ Prelude.map (readMaybe) $ wordsWhen (== ',') $ lines !! 0
    initVector = (fromList parsedLines)
    input = [5]
