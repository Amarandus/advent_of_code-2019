module AoC.IntMachine(runIntMachine, wordsWhen, buildMem,
               IntMachineState, Input, Output, Memory,
               ParserState (Exec, Halted), performOp) where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
--import Data.Vector hiding ((++), map, zip, dropWhile, reverse, take, tail)
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

data ParserState = Exec Integer | Halted
  deriving (Show)
type Input = [Integer]
type Output = [Integer]
--type Memory = Vector Int
type Memory = Map.Map Integer Integer
type RelBase = Integer
type IntMachineState = (Memory, ParserState, RelBase, Input, Output)

data OpCode = Add Parameter Parameter Parameter
  | Mul Parameter Parameter Parameter
  | Rd Parameter
  | Wr Parameter
  | JTrue Parameter Parameter
  | JFalse Parameter Parameter
  | CLt Parameter Parameter Parameter
  | CEq Parameter Parameter Parameter
  | AdjBase Parameter
  | Halt
  deriving (Show)

data Parameter = Address Integer | Intermediate Integer | RelativeAddr Integer
  deriving (Show)

(!) :: Memory -> Integer -> Integer
(!) m i = case (Map.lookup i m) of
  Just x -> x
  _ -> 0

(//) :: Memory -> [(Integer, Integer)] -> Memory
(//) m l = foldl (\m' (i, v) -> Map.insert i v m') m l

dropMap :: Memory -> Integer -> [Integer]
dropMap m i = map (\x -> m ! x) [i..]


parseParam :: (Char, Integer) -> Parameter
parseParam ('0', x) = Address x
parseParam ('1', x) = Intermediate x
parseParam ('2', x) = RelativeAddr x

resolveParam :: RelBase -> Memory -> Parameter -> Integer
resolveParam _ _ (Intermediate x) = x
resolveParam _ mem (Address x) = mem ! x
resolveParam b mem (RelativeAddr x) = mem ! (b + x)

resolveDst :: RelBase -> Parameter -> Integer
resolveDst _ (Intermediate x) = x
resolveDst _ (Address x) = x
resolveDst b (RelativeAddr x) = b + x

parseOp :: Memory -> Integer -> OpCode
parseOp mem ip
  | opcode == "01" = Add (params !! 0) (params !! 1) (params !! 2)
  | opcode == "02" = Mul (params !! 0) (params !! 1) (params !! 2)
  | opcode == "03" = Rd (params !! 0) 
  | opcode == "04" = Wr (params !! 0)
  | opcode == "05" = JTrue (params !! 0) (params !! 1)
  | opcode == "06" = JFalse (params !! 0) (params !! 1)
  | opcode == "07" = CLt (params !! 0) (params !! 1) (params !! 2)
  | opcode == "08" = CEq (params !! 0) (params !! 1) (params !! 2)
  | opcode == "09" = AdjBase (params !! 0)
  | otherwise = Halt
  where
    instruction = padL '0' 5 $ show $ mem ! ip
    opcode = Prelude.drop 3 $ instruction
    params = map parseParam $ zip (reverse $ take 3 instruction) $ dropMap mem (ip + 1)


evalOp :: IntMachineState -> OpCode -> IntMachineState
evalOp (mem, Exec ip, base, i, o) (Add a b dst)        = (mem // [(resolveDst base dst, (resolveParam base mem a) + (resolveParam base mem b))], Exec (ip + 4), base, i, o)
evalOp (mem, Exec ip, base, i, o) (Mul a b dst)        = (mem // [(resolveDst base dst, (resolveParam base mem a) * (resolveParam base mem b))], Exec (ip + 4), base, i, o)
evalOp (mem, Exec ip, base, [], o) (Rd dst)            = (mem, Halted, base, [], o)
evalOp (mem, Exec ip, base, i, o) (Rd dst)             = (mem // [(resolveDst base dst, i !! 0)], Exec (ip + 2), base, tail i, o)
evalOp (mem, Exec ip, base, i, o) (Wr a)               = (mem, Exec (ip + 2), base, i, o ++ [resolveParam base mem a])

evalOp (mem, Exec ip, base, i, o) (JTrue a b)             = (mem, Exec newIP, base, i, o)
  where
    newIP = if 0 /= resolveParam base mem a then (resolveParam base mem b) else (ip + 3)
    
evalOp (mem, Exec ip, base, i, o) (JFalse a b)             = (mem, Exec newIP, base, i, o)
  where
    newIP = if 0 == resolveParam base mem a then (resolveParam base mem b) else (ip + 3)

evalOp (mem, Exec ip, base, i, o) (CLt a b c)             = (mem // [(resolveDst base c, cmp)], Exec (ip + 4), base, i, o)
  where
    cmp = if (resolveParam base mem a) < (resolveParam base mem b) then 1 else 0

evalOp (mem, Exec ip, base, i, o) (CEq a b c)             = (mem // [(resolveDst base c, cmp)], Exec (ip + 4), base, i, o)
  where
    cmp = if (resolveParam base mem a) == (resolveParam base mem b) then 1 else 0

evalOp (mem, Exec ip, base, i, o) (AdjBase offset) = (mem, Exec (ip + 2), base + resolveParam base mem offset, i, o)

evalOp (mem, Exec ip, base, _, o) (Halt) = (mem, Halted, base, [], o)
evalOp (mem,  Halted, base, _, o)      _ = (mem, Halted, base, [], o)

performOp :: IntMachineState -> IntMachineState
performOp s@(mem, Exec ip, b, i, o) = evalOp s $ traceShowId $ parseOp mem ip
performOp s@(mem, Halted, b, i, o) = evalOp s $ Halt

runIntMachine :: State IntMachineState [Integer]
runIntMachine = do
  (mem, ps, b, i, o) <- get
  case ps of
    Exec i -> do
      state (\x -> ((), performOp $ x))
      runIntMachine
    Halted -> do
      return $ o

buildMem :: [Integer] -> Memory
buildMem = (Map.fromAscList) . zip [0..]
